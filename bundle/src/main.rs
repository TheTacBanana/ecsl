use anyhow::Result;
use clap::Parser;
use cli::{Cli, CommandTrait};
use log::debug;

pub mod cli;
pub mod compile;
pub mod new;

fn main() -> Result<()> {
    let mut args = Cli::parse();
    env_logger::builder()
        .filter_level(
            args.verbose
                .then(|| log::LevelFilter::Trace)
                .unwrap_or(log::LevelFilter::Info),
        )
        .format_target(false)
        .format_timestamp(None)
        .format_file(args.verbose)
        .format_line_number(args.verbose)
        .init();
    debug!("Logger initialised");
    args.command.execute(())
}
