use anyhow::Result;
use ecsl_assembler::Assembler;
use ecsl_ast_validation::{ast_definitions, validate_ast};
use ecsl_context::{Context, MapAssocExt};
use ecsl_diagnostics::Diagnostics;
use ecsl_error::ext::EcslErrorExt;
use ecsl_parse::parse_file;
use ecsl_ty::{LocalTyCtxtExt, TyCtxt};
use std::{collections::BTreeMap, sync::Arc};

pub struct Driver;

impl Driver {
    pub fn run() -> Result<(), ()> {
        let diag = Arc::new(Diagnostics::new());

        _ = Driver::inner(diag.clone());
        diag.finish_stage()?;

        Ok(())
    }

    fn inner(diag: Arc<Diagnostics>) -> Result<(), ()> {
        // Create the context and load all dependencies of the target program
        let path = std::env::current_dir().unwrap();
        let (ctxt, assoc) = match Context::new(path.clone(), diag.clone()) {
            Ok(ctx) => ctx,
            Err(e) => {
                diag.push_error(e);
                diag.finish_stage()?;
                return Ok(());
            }
        };
        diag.finish_stage()?;

        // Create lexers externally due to lifetimes
        let mut lexers = BTreeMap::new();
        for (id, src) in &ctxt.sources {
            let lexer = src.lexer();
            lexers.insert(id, lexer);
        }

        // Parse all files
        let assoc = (&ctxt, assoc).par_map_assoc_with(&diag, |diag, src, _| {
            let lexer = lexers.get(&src.id).unwrap();

            let result = parse_file(&src, &lexer);

            if result.ast.is_none() || result.errs.len() > 0 {
                for e in result.errs {
                    diag.push_error(e);
                }
                return None;
            }

            Some((Arc::new(result.ast.unwrap()), Arc::new(result.table)))
        })?;
        diag.finish_stage()?;

        // Perform AST validation and collect definitions and imports
        let ty_ctxt = Arc::new(TyCtxt::new());
        let assoc =
            (&ctxt, assoc).par_map_assoc(|src, (ast, table)| {
                let lexer = lexers.get(&src.id).unwrap();

                let local_ctxt = ty_ctxt.new_local_ctxt(src.id, table.clone());

                let errs = validate_ast(&ast);
                for err in errs {
                    diag.push_error(err.with_path(|_| src.path.clone().unwrap()).with_snippet(
                        |e| src.get_snippet(e.get_span().unwrap(), e.level(), &lexer),
                    ));
                }

                let errs = ast_definitions(&ast, local_ctxt.clone());
                for err in errs {
                    diag.push_error(err.with_path(|_| src.path.clone().unwrap()).with_snippet(
                        |e| src.get_snippet(e.get_span().unwrap(), e.level(), &lexer),
                    ));
                }

                Some((ast, table, local_ctxt))
            })?;

        // Verify imports
        let assoc = (&ctxt, assoc).par_map_assoc(|src, (ast, table, local_ctxt)| {
            // let lexer = lexers.get(&src.id).unwrap();

            for i in local_ctxt.imported.read().unwrap().iter() {
                println!("{:?}", i);
            }

            Some((ast, table, local_ctxt))
        })?;

        diag.finish_stage()?;

        let assembler = Assembler::new();
        assembler.output(path.clone()).unwrap();

        Ok(())
    }
}
