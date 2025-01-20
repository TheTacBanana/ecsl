use anyhow::Result;
use ecsl_assembler::Assembler;
use ecsl_ast_validation::{
    ast_definitions, casing_warnings, collect_prelude, include_prelude, validate_ast,
    validate_imports,
};
use ecsl_context::{Context, MapAssocExt};
use ecsl_diagnostics::{Diagnostics, DiagnosticsExt};
use ecsl_error::{ext::EcslErrorExt, EcslError};
use ecsl_parse::parse_file;
use ecsl_ty::{local::LocalTyCtxtExt, TyCtxt};
use log::info;
use std::{collections::BTreeMap, path::PathBuf, sync::Arc};

pub struct Driver;

impl Driver {
    pub fn run(std_path: PathBuf) -> Result<(), ()> {
        let diag = Arc::new(Diagnostics::new());

        _ = Driver::inner(std_path, diag.clone());
        diag.finish_stage(|_| ())?;

        Ok(())
    }

    fn inner(std_path: PathBuf, diag: Arc<Diagnostics>) -> Result<(), ()> {
        info!("Starting compilation");

        // Create the context and load all dependencies of the target program
        info!("Creating context and loading dependencies");
        let path = std::env::current_dir().unwrap();
        let (context, assoc) = match Context::new(path.clone(), std_path, diag.empty_conn()) {
            Ok(ctx) => ctx,
            Err(e) => {
                diag.push_error(e);
                diag.finish_stage(|_| ())?;
                return Ok(());
            }
        };
        diag.finish_stage(|_| ())?;

        // Create lexers externally due to lifetimes
        info!("Lexing source files");
        let mut lexers = BTreeMap::new();
        for (id, src) in context.sources() {
            info!("Lexing source file {}", src.id);
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
        info!("Parsing Files");
        let assoc = (&context, assoc).par_map_assoc(
            |_, src, _| {
                info!("Parsing source file {}", src.id);
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
        info!("Getting prelude from std");
        let prelude = {
            let std_lib = context
                .get_source_file_from_crate(&"lib".into(), context.config().std_id)
                .unwrap();
            let ast = &assoc.assoc.get(&std_lib).unwrap().1;
            collect_prelude(&ast, std_lib)
        };

        // Include the std prelude in all files outside of std
        info!("Including prelude into all files");
        let assoc = (&context, assoc).par_map_assoc(
            |ctxt, src, (diag, mut ast, table)| {
                if !ctxt.in_std(src.id) {
                    info!("Including prelude for source file {}", src.id);

                    let lexer = lexers.get(&prelude.id).unwrap();
                    include_prelude(&prelude, &mut ast, table.clone(), lexer);
                }

                Some((diag, ast, table))
            },
            || Ok(()),
        )?;

        // AST validation, Collect Definitions and Casing Warnings
        info!("Validating AST, Collect Definitions and Casing Warnings");
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
        info!("Verifying imports");
        let _assoc = (&context, assoc).par_map_assoc(
            |ctxt, src, (_, ast, table, local_ctxt)| {
                validate_imports(src, &ctxt, local_ctxt.clone());

                Some((ast, table, local_ctxt))
            },
            || diag.finish_stage(lexer_func),
        )?;

        let assembler = Assembler::new();
        assembler.output(path.clone()).unwrap();

        Ok(())
    }
}
