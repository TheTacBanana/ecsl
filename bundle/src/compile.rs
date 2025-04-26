use crate::cli::CommandTrait;
use anyhow::Result;
use clap::Args;
use ecsl_driver::Driver;
use log::{debug, info};
use std::{path::PathBuf, process::Command};

#[derive(Debug, Args)]
pub struct Compile {
    #[clap(long, short, action, default_value = Compile::default_std_path().into_os_string())]
    std: PathBuf,
}

impl Compile {
    pub fn default_std_path() -> PathBuf {
        let mut home = homedir::my_home().unwrap().unwrap();
        home.push(".ecsl/ecsl_std");
        home
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
            let status = Command::new("ecslvm")
                .args([path])
                .spawn()
                .unwrap()
                .wait()
                .unwrap();

            if !status.success() {
                std::process::exit(1);
            }
        }

        Ok(())
    }
}
