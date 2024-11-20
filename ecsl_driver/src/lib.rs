use std::collections::BTreeMap;

use anyhow::Result;
use ecsl_assembler::Assembler;
use ecsl_parse::parse_file;

use ecsl_context::Context;
use ecsl_diagnostics::Diagnostics;

pub struct Driver;

impl Driver {
    pub fn run() -> Result<(), ()> {
        let mut diag = Diagnostics::new();

        // Create the context and load all dependencies of the target program
        let path = std::env::current_dir().unwrap();
        let ctx = Context::new(path.clone(), &mut diag);
        let ctx = match ctx {
            Ok(ctx) => ctx,
            Err(e) => {
                diag.push_error(e);
                diag.finish_stage()?;
                return Ok(());
            }
        };
        let mut diag = diag.finish_stage()?;

        let mut _failed = false;
        let mut parsed = BTreeMap::new();
        for s in ctx.source_files() {
            let lexer = s.lexer();
            let result = parse_file(&s, &lexer);

            if result.ast.is_none() || result.errs.len() > 0 {
                for e in result.errs {
                    diag.push_error(e);
                }
                _failed = true;
            }

            if let Some(ast) = result.ast {
                parsed.insert(ast.file, ast);
            }
        }

        let mut _diag = diag.finish_stage()?;

        let assembler = Assembler::new();
        assembler.output(path.clone()).unwrap();

        Ok(())
    }
}
