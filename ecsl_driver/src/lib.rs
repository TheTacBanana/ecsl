use anyhow::Result;
use ecsl_assembler::Assembler;
use ecsl_ast_pass::*;
use ecsl_codegen::CodeGen;
use ecsl_context::{Context, MapAssocExt};
use ecsl_diagnostics::{Diagnostics, DiagnosticsExt};
use ecsl_error::{ext::EcslErrorExt, EcslError};
use ecsl_gir_pass::{
    const_eval::ConstEval,
    dead_block::DeadBlocks,
    function_graph::{FunctionDependencies, FunctionGraph},
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

        info!("Compilation took {:?}", start_time.elapsed());

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
            lexers.insert(id, lexer);
        }

        // Closure to map errors with spans to a path and snippet
        let lexer_func = |err: &mut EcslError| {
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
            || diag.finish_stage(lexer_func),
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
        let ty_ctxt = Arc::new(TyCtxt::new());
        let assoc = (&context, assoc).par_map_assoc(
            |ctxt, src, (diag, ast, table)| {
                let local_ctxt = ty_ctxt.new_local_ctxt(src.id, table.clone(), diag.clone());

                validate_ast(ctxt, &ast, diag.clone());
                ast_definitions(&ast, ctxt, local_ctxt.clone());
                casing_warnings(&ast, diag.clone(), table.clone());

                Some((diag, ast, table, local_ctxt))
            },
            || Ok(()),
        )?;

        // Verify imports
        debug!("Verifying imports");
        let assoc = (&context, assoc).par_map_assoc(
            |ctxt, src, (diag, ast, table, local_ctxt)| {
                validate_imports(src, &ctxt, local_ctxt.clone());

                Some((diag, ast, table, local_ctxt))
            },
            || diag.finish_stage(lexer_func),
        )?;

        // Generate TyIr for all Definitions
        info!("Generating TyIr");
        let assoc = (&context, assoc).par_map_assoc(
            |_, _, (diag, ast, table, local_ctxt)| {
                generate_definition_tyir(local_ctxt.clone());

                Some((diag, ast, table, local_ctxt))
            },
            || diag.finish_stage(lexer_func),
        )?;

        // Perform type checking
        info!("Type Checking");
        let assoc = (&context, assoc).par_map_assoc(
            |_, _, (diag, ast, table, local_ctxt)| {
                debug!("Type Checking source file {}", ast.file);
                let girs = ty_check(&ast, local_ctxt.clone());

                Some((diag, ast, table, local_ctxt, girs))
            },
            || diag.finish_stage(lexer_func),
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
            diag.finish_stage(lexer_func)?;
            return Err(());
        };
        ty_ctxt.insert_entry_point(entry_point.0);

        debug!("Building Function Graph");
        let function_graph = Arc::new(FunctionGraph::new());
        let assoc = (&context, assoc).par_map_assoc(
            |_, _, (diag, ast, table, local_ctxt, mut girs)| {
                let function_graph = function_graph.clone();
                for (_, gir) in girs.iter_mut() {
                    DeadBlocks::apply_pass(gir, ());
                    FunctionDependencies::apply_pass(gir, &function_graph);
                }
                Some((diag, ast, table, local_ctxt, girs, function_graph))
            },
            || diag.finish_stage(lexer_func),
        )?;
        function_graph.prune_unused(entry_point.0);

        debug!("Prune unused functions");
        let assoc = (&context, assoc).par_map_assoc(
            |_, _, (diag, ast, table, local_ctxt, mut girs, function_graph)| {
                let graph = function_graph.graph.read().unwrap();
                girs.retain(|_, g| graph.contains_node(g.fn_id()));

                Some((diag, ast, table, local_ctxt, girs))
            },
            || diag.finish_stage(lexer_func),
        )?;

        let root_config = context.config().root_config();
        let assembler = Assembler::new(
            root_config.name().to_string(),
            &root_config.bundle_toml_path(),
        );

        let assembler = assembler.write_temp_header().unwrap();
        let assembler = assembler.write_const_data().unwrap();

        info!("GIR Passes");
        let _ = (&context, assoc).par_map_assoc(
            |_, src, (diag, ast, table, local_ctxt, mut girs)| {
                let lexer = lexers.get(&src.id).unwrap();

                for (_, gir) in girs.iter_mut() {
                    let consts = ConstEval::apply_pass(gir, lexer);

                    let bytecode = CodeGen::apply_pass(gir, (local_ctxt.clone(), consts));
                    assembler.include_function(bytecode);
                }

                Some((diag, ast, table, local_ctxt, girs))
            },
            || diag.finish_stage(lexer_func),
        )?;

        let assember = assembler.write_bytecode(entry_point.0).unwrap();

        Ok(assember.output().unwrap())
    }
}
