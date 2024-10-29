use anyhow::Result;
use ecsl_assembler::Assembler;
use ecsl_error::ext::EcslErrorExt;
use ecsl_lexer::SourceReader;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

use std::path::PathBuf;

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
        let diag = diag.finish_stage()?;

        let assembler = Assembler::new();
        assembler.output(path.clone()).unwrap();

        Ok(())
    }
}
