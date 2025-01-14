use std::{sync::RwLock, vec};

use std::io::Write;

use ansi_term::Colour::Red;
use ecsl_error::{EcslError, ErrorLevel};

pub struct Diagnostics {
    stages: RwLock<Vec<Vec<EcslError>>>,
}

impl Diagnostics {
    pub fn new() -> Self {
        Self {
            stages: RwLock::new(vec![Vec::new()]),
        }
    }

    pub fn finish_stage(&self) -> Result<(), ()> {
        let fatal = self
            .stages
            .read()
            .unwrap()
            .last()
            .unwrap()
            .iter()
            .any(|e| e.level() == ErrorLevel::Error);

        if fatal {
            self.ok().unwrap();
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

    fn ok(&self) -> std::io::Result<()> {
        let stdout = std::io::stdout();
        let mut f = stdout.lock();

        let mut total_fatal = 0;
        for stage in self.stages.write().unwrap().iter_mut() {
            for err in stage.drain(..) {
                if err.level() == ErrorLevel::Error {
                    total_fatal += 1;
                }
                writeln!(f, "{}", err)?;
            }
        }

        writeln!(
            f,
            "{}: Failed to compile due to {} error{}",
            Red.paint("Error"),
            total_fatal,
            if total_fatal > 1 { "s" } else { "" }
        )?;

        Ok(())
    }
}
