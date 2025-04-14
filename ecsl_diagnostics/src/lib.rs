use ansi_term::Colour::Red;
use ecsl_error::ext::EcslErrorExt;
use ecsl_error::{EcslError, ErrorLevel};
use ecsl_index::SourceFileID;
use log::debug;
use std::io::Write;
use std::sync::Arc;
use std::{sync::RwLock, vec};

pub struct Diagnostics {
    stages: RwLock<Vec<Vec<EcslError>>>,
}

#[derive(Clone)]
pub struct DiagConn {
    diag: Arc<Diagnostics>,
    file: Option<SourceFileID>,
}

pub trait DiagnosticsExt {
    fn new_conn(&self, file: SourceFileID) -> DiagConn;
    fn empty_conn(&self) -> DiagConn;
}

impl Diagnostics {
    pub fn new() -> Self {
        Self {
            stages: RwLock::new(vec![Vec::new()]),
        }
    }

    pub fn finish_stage(&self, f: impl Fn(&mut EcslError)) -> Result<(), ()> {
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

                writeln!(lock, "{}", err).unwrap();
            }
        }

        if total_fatal > 0 {
            writeln!(
                lock,
                "{}: Failed to compile due to {} error{}",
                Red.paint("Error"),
                total_fatal,
                if total_fatal > 1 { "s" } else { "" }
            )
            .unwrap();
            Err(())
        } else {
            self.stages.write().unwrap().push(Default::default());
            Ok(())
        }
    }

    pub fn push_error(&self, err: EcslError) {
        debug!("{}", err);
        self.stages
            .write()
            .unwrap()
            .last_mut()
            .unwrap()
            .push(err.into());
    }
}

impl DiagConn {
    pub fn push_error(&self, err: EcslError) {
        if let Some(file) = self.file {
            let err = err.with_file(|_| file);
            self.diag.push_error(err);
        } else {
            self.diag.push_error(err);
        }
    }
}

impl DiagnosticsExt for Arc<Diagnostics> {
    fn new_conn(&self, file: SourceFileID) -> DiagConn {
        DiagConn {
            diag: self.clone(),
            file: Some(file),
        }
    }

    fn empty_conn(&self) -> DiagConn {
        DiagConn {
            diag: self.clone(),
            file: None,
        }
    }
}
