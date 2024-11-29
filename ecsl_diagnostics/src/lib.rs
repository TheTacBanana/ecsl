use std::{sync::RwLock, vec};

use std::io::Write;

use ansi_term::Colour::Red;
use ecsl_error::{EcslError, ErrorLevel};

pub struct Diagnostics {
    stages: Vec<RwLock<Vec<EcslError>>>,
}

impl Diagnostics {
    pub fn new() -> Self {
        Self {
            stages: vec![Default::default()],
        }
    }

    pub fn finish_stage(mut self) -> Result<Self, ()> {
        let fatal = self
            .stages
            .last()
            .unwrap()
            .write()
            .unwrap()
            .iter()
            .any(|e| e.level() == ErrorLevel::Error);

        if fatal {
            self.ok().unwrap();
            Err(())
        } else {
            self.stages.push(Default::default());
            Ok(self)
        }
    }

    pub fn push_error(&self, error: EcslError) {
        self.stages
            .last()
            .unwrap()
            .write()
            .unwrap()
            .push(error.into());
    }

    fn ok(self) -> std::io::Result<()> {
        let stdout = std::io::stdout();
        let mut f = stdout.lock();

        let mut total_fatal = 0;
        for stage in self.stages {
            for err in stage.write().unwrap().iter() {
                if err.level() == ErrorLevel::Error {
                    total_fatal += 1;
                }
                writeln!(f, "{}", err)?;
            }
        }
        writeln!(
            f,
            "{}: Failed to compile due to {} errors",
            Red.paint("Error"),
            total_fatal
        )?;

        Ok(())
    }
}
