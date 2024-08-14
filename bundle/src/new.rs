use std::path::{Path, PathBuf};

use clap::Args;
use anyhow::{anyhow, Context, Error, Ok, Result};

use crate::cli::CommandTrait;


#[derive(Debug, Args)]
pub struct New {
    project_name: String,
}

impl CommandTrait for New {
    fn execute(&mut self) -> Result<()>{
        let mut path = PathBuf::new();
        path.push(&self.project_name);

        if path.exists() {
            return Err(anyhow!("Project {:?} already exists", self.project_name));
        }

        Ok(())

    }
}