use std::io;

use clap::Parser;

#[derive(Parser)]
pub(crate) struct Args {}

impl Args {
    pub(crate) fn run(&self) -> eyre::Result<()> {
        let src = io::read_to_string(io::stdin())?;
        let (root, _) = syntax::parser::parse(&src);
        println!("{:#?}", root);
        Ok(())
    }
}
