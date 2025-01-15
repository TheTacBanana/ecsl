use ansi_term::Colour::Red;
use ecsl_error::ext::EcslErrorExt;
use ecsl_error::{EcslError, ErrorLevel};
use ecsl_index::SourceFileID;
use std::io::Write;
use std::sync::Arc;
use std::{sync::RwLock, vec};

pub struct Diagnostics {
    stages: RwLock<Vec<Vec<EcslError>>>,
}

#[derive(Clone)]
pub struct DiagConn {
    diag: Arc<Diagnostics>,
    file: SourceFileID,
}

pub trait DiagnosticsExt {
    fn new_connector(&self, file: SourceFileID) -> DiagConn;
}

impl Diagnostics {
    pub fn new() -> Self {
        Self {
            stages: RwLock::new(vec![Vec::new()]),
        }
    }

    pub fn finish_stage(&self, f: impl Fn(&mut EcslError)) -> Result<(), ()> {
        let fatal = self
            .stages
            .read()
            .unwrap()
            .last()
            .unwrap()
            .iter()
            .any(|e| e.level() == ErrorLevel::Error);

        if fatal {
            self.ok(f).unwrap();
            Err(())
        } else {
            self.stages.write().unwrap().push(Default::default());
            Ok(())
        }
    }

    pub fn push_error(&self, error: EcslError) {
        self.stages
            .write()
            .unwrap()
            .last_mut()
            .unwrap()
            .push(error.into());
    }

    fn ok(&self, f: impl Fn(&mut EcslError)) -> std::io::Result<()> {
        let stdout = std::io::stdout();
        let mut lock = stdout.lock();

        let mut total_fatal = 0;
        for stage in self.stages.write().unwrap().iter_mut() {
            for err in stage.drain(..) {
                let mut err = err;
                if err.level() == ErrorLevel::Error {
                    total_fatal += 1;
                }

                f(&mut err);

                writeln!(lock, "{}", err)?;
            }
        }

        writeln!(
            lock,
            "{}: Failed to compile due to {} error{}",
            Red.paint("Error"),
            total_fatal,
            if total_fatal > 1 { "s" } else { "" }
        )?;

        Ok(())
    }
}

impl DiagConn {
    pub fn push_error(&self, err: EcslError) {
        let err = err.with_file(|_| self.file);
        self.diag.push_error(err);
    }
}

impl DiagnosticsExt for Arc<Diagnostics> {
    fn new_connector(&self, file: SourceFileID) -> DiagConn {
        DiagConn {
            diag: self.clone(),
            file,
        }
    }
}
