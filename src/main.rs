use std::io;

#[macro_use]
mod macros;
mod corner;
mod document;
mod error;
pub mod evaluator;
mod expressions;
pub mod lexer;
mod operators;
pub mod parser;
pub mod points;
pub mod statement;
mod values;

use parser::LexerExt;

fn main() -> Result<(), error::Error> {
    let stdin = io::stdin();
    let parser = lexer::Lexer::new(stdin.lock()).into_parser();
    let mut evaluator = evaluator::Evaluator::new();
    evaluator.evaluate_all(parser)?;
    let stdout = io::stdout();
    println!(r#"<?xml version="1.0" encoding="utf-8" ?>"#);
    svg::write(&mut stdout.lock(), &evaluator.create_document().compile())
        .map_err(error::EvaluatorError::Io)?;
    Ok(())
}
