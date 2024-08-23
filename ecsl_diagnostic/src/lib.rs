use std::vec;

use std::io::Write;

use ecsl_error::{CompleteError, ErrorLevel};
use ansi_term::Colour::Red;

pub struct Diagnostics {
    stages: Vec<Vec<CompleteError>>,
}

impl Diagnostics {
    pub fn new() -> Self {
        Self {
            stages: vec![Vec::new()],
        }
    }

    pub fn finish_stage(&mut self) -> Result<(), ()> {
        let fatal = self
            .stages
            .last()
            .unwrap()
            .iter()
            .any(|e| e.level() == ErrorLevel::Error);

        self.stages.push(Vec::new());

        (!fatal).then(|| ()).ok_or(())
    }

    pub fn push_error(&mut self, error: impl Into<CompleteError> ) {
        self.stages.last_mut().unwrap().push(error.into());
    }

    pub fn ok(self) -> std::io::Result<()>{
        let stdout = std::io::stdout();
        let mut f = stdout.lock();

        let mut total_fatal = 0;
        for stage in self.stages {
            for err in stage {
                if err.level() == ErrorLevel::Error {
                    total_fatal += 1;
                }
                writeln!(f, "{}", err)?;
            }
        }
        writeln!(f, "{}: Failed to compile due to {} errors", Red.paint("Error"), total_fatal)?;

        Ok(())
    }
}
