use std::{io::Write, path::PathBuf};

use anyhow::{anyhow, Ok, Result};
use clap::Args;

use crate::cli::CommandTrait;

#[derive(Debug, Args)]
pub struct New {
    project_name: String,
}

impl New {
    const DEFAULT_BUNDLE_TOML: &'static str = include_str!("Bundle.toml");
    const DEFAULT_MAIN_ECSL: &'static str = include_str!("main.ecsl");
}

impl CommandTrait for New {
    fn execute(&mut self) -> Result<()> {
        let mut root_path = PathBuf::new();
        root_path.push(&self.project_name);

        if root_path.exists() {
            return Err(anyhow!("Project {:?} already exists", self.project_name));
        }

        // Create the project directory
        std::fs::create_dir(root_path.clone())?;

        // Create the Bundle.toml
        {
            let mut bundle_toml = root_path.clone();
            bundle_toml.push("Bundle.toml");
            let mut bundle_toml = std::fs::File::create_new(bundle_toml)?;
            let contents = New::DEFAULT_BUNDLE_TOML.replace("{}", &self.project_name);
            bundle_toml.write_all(contents.as_bytes())?;
        }

        // Create the src directory
        {
            let mut src_dir = root_path.clone();
            src_dir.push("src");
            std::fs::create_dir(src_dir)?
        }

        // Create the main file
        {
            let mut main_ecsl = root_path.clone();
            main_ecsl.push("src/main.ecsl");
            let mut main_ecsl = std::fs::File::create_new(main_ecsl)?;
            main_ecsl.write_all(New::DEFAULT_MAIN_ECSL.as_bytes())?
        }

        Ok(())
    }
}
