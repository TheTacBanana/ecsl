use clap::Args;
use ecsl_driver::Driver;

use crate::cli::CommandTrait;
use anyhow::{Ok, Result};


#[derive(Debug, Args)]
pub struct Compile;

impl CommandTrait for Compile {
    fn execute(&mut self) -> Result<()> {
        Driver::run();
        Ok(())
    }
}