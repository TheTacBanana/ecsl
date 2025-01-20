use anyhow::Result;
use clap::Parser;
use cli::{Cli, CommandTrait};
use log::info;

pub mod cli;
pub mod compile;
pub mod new;

fn main() -> Result<()> {
    let mut args = Cli::parse();
    env_logger::builder()
        .filter_level(
            args.verbose
                .then(|| log::LevelFilter::Trace)
                .unwrap_or(log::LevelFilter::Error),
        )
        .format_target(false)
        .format_timestamp(None)
        .init();
    info!("Logger initialised");
    args.command.execute()
}
