mod lsp_server;
mod parse;

use clap::{Parser, Subcommand};

#[derive(Parser)]
struct Args {
    #[command(subcommand)]
    subcmd: Option<Command>,
}

#[derive(Subcommand)]
enum Command {
    /// Parse from stdin and dump the syntax tree to stdout.
    Parse(parse::Parse),
}

pub fn main() -> eyre::Result<()> {
    match Args::parse().subcmd {
        Some(subcmd) => match subcmd {
            Command::Parse(cmd) => cmd.run(),
        },
        None => lsp_server::run(),
    }
}
