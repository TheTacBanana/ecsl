use anyhow::Result;
use ecsl_assembler::Assembler;
use ecsl_ast_pass::*;
use ecsl_codegen::{bp_promotion::BpPromotion, noop::NoOp, pass::CodegenPass, CodeGen};
use ecsl_context::{Context, MapAssocExt};
use ecsl_diagnostics::{Diagnostics, DiagnosticsExt};
use ecsl_error::{ext::EcslErrorExt, EcslError};
use ecsl_gir_pass::{
    block_order::BlockOrder,
    comp_ids::ComponentDefinitions,
    const_eval::ConstEval,
    dead_block::DeadBlocks,
    function_graph::{FunctionDependencies, FunctionGraph},
    linker::FunctionLinker,
    mono::monomorphize,
    return_path::ReturnPath,
    GIRPass,
};
use ecsl_parse::parse_file;
use ecsl_ty::{ctxt::TyCtxt, local::LocalTyCtxtExt};
use ecsl_ty_check::ty_check;
use log::{debug, info};
use std::{collections::BTreeMap, path::PathBuf, sync::Arc, time::Instant};

pub struct Driver;

impl Driver {
    pub fn run(std_path: PathBuf) -> Result<PathBuf, ()> {
        let start_time = Instant::now();

        let diag = Arc::new(Diagnostics::new());

        let path = Driver::inner(std_path, diag.clone());
        diag.finish_stage(|_| ())?;

        if path.is_ok() {
            info!("Finished in {:?}", start_time.elapsed());
        } else {
            info!("Exited in {:?}", start_time.elapsed());
        }

        Ok(path?)
    }

    fn inner(std_path: PathBuf, diag: Arc<Diagnostics>) -> Result<PathBuf, ()> {
        debug!("Starting compilation");

        // Create the context and load all dependencies of the target program
        debug!("Creating context and loading dependencies");
        let path = std::env::current_dir().unwrap();
        let (context, assoc) = match Context::new(path.clone(), std_path, diag.empty_conn()) {
            Ok(ctx) => ctx,
            Err(e) => {
                diag.push_error(e);
                diag.finish_stage(|_| ())?;
                return Err(());
            }
        };
        diag.finish_stage(|_| ())?;

        // Create lexers externally due to lifetimes
        info!("Lexing");
        let mut lexers = BTreeMap::new();
        for (id, src) in context.sources() {
            debug!("Lexing source file {}", src.id);
            let lexer = src.lexer();
            lexers.insert(*id, lexer);
        }

        // Closure to map errors with spans to a path and snippet
        let finish_stage = |err: &mut EcslError| {
            if let (Some(span), Some(file)) = (err.get_span(), err.get_file()) {
                let source = context.get_source_file(file).unwrap();
                let lexer = lexers.get(&file).unwrap();
                let level = err.level();

                err.with_path(|_| source.path.clone());
                err.with_snippet(|_| source.get_snippet(span, level, &lexer));
            }
        };

        // Parse all files
        info!("Parsing");
        let assoc = (&context, assoc).par_map_assoc(
            |_, src, _| {
                debug!("Parsing source file {}", src.id);
                let lexer = lexers.get(&src.id).unwrap();
                let diag = diag.new_conn(src.id);

                let result = parse_file(&src, &lexer, diag.clone());
                if result.ast.is_none() {
                    return None;
                }

                Some((diag, result.ast.unwrap(), Arc::new(result.table)))
            },
            || diag.finish_stage(finish_stage),
        )?;

        // Process the std prelude
        debug!("Getting prelude from std");
        let prelude = {
            let std_lib = context
                .get_source_file_from_package(&"lib".into(), context.config().std_id)
                .unwrap();
            let ast = &assoc.assoc.get(&std_lib).unwrap().1;
            collect_prelude(&ast, std_lib)
        };

        // Include the std prelude in all files outside of std
        debug!("Including prelude into all files");
        let assoc = (&context, assoc).par_map_assoc(
            |ctxt, src, (diag, mut ast, table)| {
                if !ctxt.in_std(src.id) {
                    debug!("Including prelude for source file {}", src.id);

                    let lexer = lexers.get(&prelude.id).unwrap();
                    include_prelude(&prelude, &mut ast, table.clone(), lexer);
                }

                Some((diag, ast, table))
            },
            || Ok(()),
        )?;

        // AST validation, Collect Definitions and Casing Warnings
        info!("AST Passes");
        let ty_ctxt = Arc::new(TyCtxt::new(diag.empty_conn()));
        let assoc = (&context, assoc).par_map_assoc(
            |ctxt, src, (diag, ast, table)| {
                let local_ctxt = ty_ctxt.new_local_ctxt(src.id, table.clone(), diag.clone());

                validate_ast(ctxt, &ast, diag.clone());
                ast_definitions(&ast, ctxt, local_ctxt.clone());
                casing_warnings(&ast, diag.clone(), table.clone());

                Some((diag, ast, table, local_ctxt))
            },
            || diag.finish_stage(finish_stage),
        )?;

        // Verify imports
        debug!("Verifying imports");
        let assoc = (&context, assoc).par_map_assoc(
            |ctxt, src, (diag, ast, table, local_ctxt)| {
                validate_imports(src, &ctxt, local_ctxt.clone());

                Some((diag, ast, table, local_ctxt))
            },
            || diag.finish_stage(finish_stage),
        )?;

        // Closure to map errors with spans to a path and snippet
        let finish_stage = |err: &mut EcslError| {
            finish_stage(err);

            if let Some(new_message) = ty_ctxt.format_str(err.message(), &lexers) {
                err.with_message(|_| new_message.clone());
            }
        };
        // Generate TyIr for all Definitions
        info!("Generating TyIr");
        let assoc = (&context, assoc).par_map_assoc(
            |_, _, (diag, ast, table, local_ctxt)| {
                generate_pre_tyir(local_ctxt.clone());
                Some((diag, ast, table, local_ctxt))
            },
            || diag.finish_stage(finish_stage),
        )?;
        let assoc = (&context, assoc).par_map_assoc(
            |_, _, (diag, ast, table, local_ctxt)| {
                generate_definition_tyir(local_ctxt.clone());
                Some((diag, ast, table, local_ctxt))
            },
            || diag.finish_stage(finish_stage),
        )?;

        // Perform type checking
        info!("Type Checking");
        let assoc = (&context, assoc).par_map_assoc(
            |_, _, (diag, ast, table, local_ctxt)| {
                debug!("Type Checking source file {}", ast.file);

                let linker = ty_check(
                    &ast,
                    local_ctxt.clone(),
                    FunctionLinker::new(ty_ctxt.monos.clone()),
                );

                Some((diag, ast, table, local_ctxt, linker))
            },
            || diag.finish_stage(finish_stage),
        )?;

        // Get the entry point
        debug!("Get entry point");
        let entry_point = {
            let main_file = context
                .get_source_file_from_package(&"main".into(), context.config().root_id)
                .unwrap();
            let assoc = &assoc.assoc.get(&main_file).unwrap();
            get_entry_point(&assoc.1, assoc.3.clone())
        };
        let Some(entry_point) = entry_point else {
            diag.finish_stage(finish_stage)?;
            return Err(());
        };
        ty_ctxt.insert_entry_point(entry_point.0);

        info!("Monomorphization");
        let assoc = (&context, assoc).par_map_assoc(
            |_, _, (diag, ast, table, local_ctxt, mut linker)| {
                monomorphize(&mut linker, &local_ctxt);

                Some((diag, ast, table, local_ctxt, linker))
            },
            || diag.finish_stage(finish_stage),
        )?;

        info!("GIR Passes");
        let assoc = (&context, assoc).par_map_assoc(
            |_, _, (diag, ast, table, local_ctxt, mut linker)| {
                for (_, gir) in linker.fn_gir.iter_mut() {
                    BlockOrder::apply_pass(gir, &local_ctxt);
                    DeadBlocks::apply_pass(gir, ());
                    ReturnPath::apply_pass(gir, &local_ctxt);
                    // Mutability::apply_pass(gir, diag.clone());
                }
                Some((diag, ast, table, local_ctxt, linker))
            },
            || diag.finish_stage(finish_stage),
        )?;

        debug!("Building Function Graph");
        let function_graph = Arc::new(FunctionGraph::new());
        let assoc = (&context, assoc).par_map_assoc(
            |_, _, (diag, ast, table, local_ctxt, mut linker)| {
                let function_graph = function_graph.clone();
                for (_, gir) in linker.fn_gir.iter_mut() {
                    FunctionDependencies::apply_pass(gir, &function_graph);
                }
                Some((diag, ast, table, local_ctxt, linker, function_graph))
            },
            || diag.finish_stage(finish_stage),
        )?;
        function_graph.prune_unused(entry_point.0);
        function_graph.find_sys_calls(&ty_ctxt);

        debug!("Prune unused functions");
        let assoc = (&context, assoc).par_map_assoc(
            |_, _, (diag, ast, table, local_ctxt, mut linker, function_graph)| {
                let graph = function_graph.graph.read().unwrap();
                linker.fn_gir.retain(|tyid, _| graph.contains_node(*tyid));

                Some((diag, ast, table, local_ctxt, linker))
            },
            || diag.finish_stage(finish_stage),
        )?;

        let comp_defs = Arc::new(ComponentDefinitions::new(ty_ctxt.clone()));
        let assoc = (&context, assoc).par_map_assoc(
            |_, _, (diag, ast, table, local_ctxt, linker)| {
                for (_, gir) in linker.fn_gir.iter() {
                    for c in gir.components() {
                        comp_defs.add_component(*c);
                    }
                }
                Some((diag, ast, table, local_ctxt, linker))
            },
            || diag.finish_stage(finish_stage),
        )?;

        info!("Assembling");
        let root_config = context.config().root_config();
        let assembler = Assembler::new(
            root_config.name().to_string(),
            &root_config.bundle_toml_path(),
        );

        let assembler = assembler.write_temp_header().unwrap();

        debug!("Write const data");
        let assoc = (&context, assoc).par_map_assoc(
            |_, src, (_, _, _, local_ctxt, mut linker)| {
                let lexer = lexers.get(&src.id).unwrap();
                let mut gir_consts = BTreeMap::new();
                for (id, gir) in linker.fn_gir.iter_mut() {
                    let consts = ConstEval::apply_pass(gir, (lexer, &assembler));
                    gir_consts.insert(*id, consts);
                }
                Some((local_ctxt, linker, gir_consts))
            },
            || diag.finish_stage(finish_stage),
        )?;
        let assembler = assembler.write_const_data().unwrap();

        debug!("Write component defs");
        let assembler = assembler
            .write_comp_defs(comp_defs.clone_components())
            .unwrap();

        let _ = (&context, assoc).par_map_assoc(
            |_, _, (local_ctxt, mut linker, gir_consts)| {
                for (id, gir) in linker.fn_gir.iter_mut() {
                    let consts = gir_consts.get(id).unwrap();
                    let mut bytecode =
                        CodeGen::apply_pass(gir, (local_ctxt.clone(), comp_defs.clone(), consts));
                    BpPromotion::apply_pass(&mut bytecode, ());
                    NoOp::apply_pass(&mut bytecode, ());

                    assembler.include_function(bytecode);
                }
                Some(())
            },
            || diag.finish_stage(finish_stage),
        )?;
        let assember = assembler.write_bytecode(entry_point.0).unwrap();

        Ok(assember.output().unwrap())
    }
}
