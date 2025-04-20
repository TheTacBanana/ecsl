use crate::{compile::Compile, new::New};
use anyhow::Result;
use clap::{Parser, Subcommand};

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
    Build(Compile),
    Run(Compile),
}

pub trait CommandTrait {
    type In;
    type Out;

    fn execute(&mut self, input: Self::In) -> Result<Self::Out>;
}

impl CommandTrait for Commands {
    type In = ();
    type Out = ();

    fn execute(&mut self, _: ()) -> Result<()> {
        match self {
            Commands::New(n) => n.execute(()),
            Commands::Build(b) => b.execute(false),
            Commands::Run(b) => b.execute(true),
        }
    }
}
