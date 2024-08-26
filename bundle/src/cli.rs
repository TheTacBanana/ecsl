use clap::{Args, Parser, Subcommand};
use anyhow::Result;

use crate::{compile::Compile, new::New};

#[derive(Parser)]
#[command(version, about, long_about = None)]
#[command(propagate_version = true)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Commands,
}

#[derive(Debug, Subcommand)]
pub enum Commands {
    New(New),
    Compile(Compile),
}

pub trait CommandTrait {
    fn execute(&mut self) -> Result<()>;
}

impl CommandTrait for Commands {
    fn execute(&mut self) -> Result<()> {
        match self {
            Commands::New(n) => n.execute(),
            Commands::Compile(b) => b.execute(),
        }
    }
}