use clap::Parser;
use cli::{Cli, CommandTrait};
use anyhow::Result;

pub mod cli;
pub mod new;

pub struct Build;

fn main() -> Result<()>{
    let mut args = Cli::parse();
    args.command.execute()
}
