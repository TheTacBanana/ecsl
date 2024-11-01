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
        let _diag = diag.finish_stage()?;

        for s in ctx.source_files() {
            parse_file(s);
        }

        let assembler = Assembler::new();
        assembler.output(path.clone()).unwrap();

        Ok(())
    }
}
