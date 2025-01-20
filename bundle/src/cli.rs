use anyhow::Result;
use clap::{Parser, Subcommand};

use crate::{compile::Compile, new::New};

#[derive(Parser)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Commands,
    #[arg(short, long)]
    pub verbose: bool,
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
