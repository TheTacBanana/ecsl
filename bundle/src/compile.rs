use std::path::PathBuf;

use clap::Args;
use ecsl_driver::Driver;

use crate::cli::CommandTrait;
use anyhow::{Ok, Result};

#[derive(Debug, Args)]
pub struct Compile {
    #[clap(long, short, action, default_value = Compile::default_std_path().into_os_string())]
    std: PathBuf,
}

impl Compile {
    pub fn default_std_path() -> PathBuf {
        "../ecsl_std".into() //TODO: Make proper path
    }
}

impl CommandTrait for Compile {
    fn execute(&mut self) -> Result<()> {
        self.std = std::path::absolute(&self.std).unwrap();
        let _ = Driver::run(self.std.clone());
        Ok(())
    }
}

#[derive(Debug, Clone)]
// , group = "input")]
pub struct StdPath(pub PathBuf);

impl std::default::Default for StdPath {
    fn default() -> Self {
        Self("../ecsl_std".into())
    }
}
