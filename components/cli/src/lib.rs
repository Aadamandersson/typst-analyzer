mod parse;

use clap::{Parser, Subcommand};

#[derive(Parser)]
struct Args {
    #[command(subcommand)]
    cmd: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Parses source text from stdin and dumps the syntax tree to stdout.
    Parse(parse::Args),
}

pub fn main() -> eyre::Result<()> {
    match Args::parse().cmd {
        Command::Parse(args) => args.run(),
    }
}
