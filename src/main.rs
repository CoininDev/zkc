mod ast;
mod newast;
mod tokenizer;
use std::fs;

use tokenizer::*;

use crate::newast::Program;

fn main() {
    let args: Vec<String> = std::env::args().collect();

    let file = match args.iter().nth(1) {
        Some(n) => n,
        None => panic!("Please, enter a file to read"),
    };

    dbg!(file);

    let content =
        fs::read_to_string(file).expect("Error reading the file. Are you at the right path?");
    let tokens: TokenList = content.into();

    dbg!(&tokens);

    let program: Program = tokens.into();

    dbg!(program);
}
