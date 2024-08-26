use anyhow::Result;

use std::path::{Path, PathBuf};

use ecsl_context::Context;
use ecsl_diagnostics::Diagnostics;
use ecsl_error::ErrorWithPath;

pub struct Driver;

impl Driver {
    pub fn run() {
        let mut diag = Diagnostics::new();

        let path = PathBuf::new();
        let ctx = Context::new(path.clone(), &mut diag);
        let ctx = match ctx {
            Ok(ctx) => ctx,
            Err(e) => {
                diag.push_error(ErrorWithPath::new(e, path));
                let _ = diag.finish_stage();
                return;
            }
        };

        let Ok(diag) = diag.finish_stage() else {
            return;
        };
    }
}
