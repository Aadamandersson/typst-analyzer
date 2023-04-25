use std::io;

use clap::Args;

#[derive(Args)]
pub(crate) struct Parse;

impl Parse {
    pub(crate) fn run(&self) -> eyre::Result<()> {
        let src = io::read_to_string(io::stdin())?;
        let (root, _) = syntax::parser::parse(&src);
        println!("{:#?}", root);
        Ok(())
    }
}
