use anyhow::Result;
use clap::Parser;
use cli::{Cli, CommandTrait};

pub mod cli;
pub mod compile;
pub mod new;

pub struct Build;

fn main() -> Result<()> {
    let mut args = Cli::parse();
    args.command.execute()
}
