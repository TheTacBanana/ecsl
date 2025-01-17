use anyhow::Result;
use ecsl_assembler::Assembler;
use ecsl_ast_validation::{ast_definitions, validate_ast, validate_imports};
use ecsl_context::{Context, MapAssocExt};
use ecsl_diagnostics::{Diagnostics, DiagnosticsExt};
use ecsl_error::{ext::EcslErrorExt, EcslError};
use ecsl_parse::parse_file;
use ecsl_ty::{local::LocalTyCtxtExt, TyCtxt};
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
        // Create the context and load all dependencies of the target program
        let path = std::env::current_dir().unwrap();
        let (ctxt, assoc) = match Context::new(path.clone(), std_path, diag.empty_conn()) {
            Ok(ctx) => ctx,
            Err(e) => {
                diag.push_error(e);
                diag.finish_stage(|_| ())?;
                return Ok(());
            }
        };
        diag.finish_stage(|_| ())?;

        // Create lexers externally due to lifetimes
        let mut lexers = BTreeMap::new();
        for (id, src) in ctxt.sources() {
            let lexer = src.lexer();
            lexers.insert(id, lexer);
        }

        // Closure to map errors with spans to a path and snippet
        let lexer_func = |err: &mut EcslError| {
            if let (Some(span), Some(file)) = (err.get_span(), err.get_file()) {
                let source = ctxt.get_source_file(file).unwrap();
                let lexer = lexers.get(&file).unwrap();
                let level = err.level();

                err.with_path(|_| source.path.clone());
                err.with_snippet(|_| source.get_snippet(span, level, &lexer));
            }
        };

        // Parse all files
        let assoc = (&ctxt, assoc).par_map_assoc(
            |src, _| {
                let lexer = lexers.get(&src.id).unwrap();
                let diag = diag.new_conn(src.id);

                let result = parse_file(&src, &lexer, diag.clone());
                if result.ast.is_none() {
                    return None;
                }

                Some((diag, Arc::new(result.ast.unwrap()), Arc::new(result.table)))
            },
            || diag.finish_stage(lexer_func),
        )?;

        // Perform AST validation and collect definitions and imports
        let ty_ctxt = Arc::new(TyCtxt::new());
        let assoc = (&ctxt, assoc).par_map_assoc(
            |src, (diag, ast, table)| {
                let local_ctxt = ty_ctxt.new_local_ctxt(src.id, table.clone(), diag.clone());

                validate_ast(&ast, diag.clone());
                ast_definitions(&ast, local_ctxt.clone());

                Some((diag, ast, table, local_ctxt))
            },
            || Ok(()),
        )?;

        // Verify imports
        let assoc = (&ctxt, assoc).par_map_assoc(
            |src, (_, ast, table, local_ctxt)| {
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
