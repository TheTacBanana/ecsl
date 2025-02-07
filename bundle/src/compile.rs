use std::{path::PathBuf, process::Command};

use clap::Args;
use ecsl_driver::Driver;
use log::{debug, info};

use crate::cli::CommandTrait;
use anyhow::Result;

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
    type In = bool;
    type Out = ();

    fn execute(&mut self, run: bool) -> Result<()> {
        self.std = std::path::absolute(&self.std).unwrap();
        debug!("Running Driver");
        let out = Driver::run(self.std.clone());

        if !run {
            return Ok(());
        }

        if let Ok(path) = out {
            info!("Invoking VM");
            Command::new("../zig-out/bin/ecslvm")
                .args([path])
                .spawn()
                .unwrap()
                .wait()
                .unwrap();
        }

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
