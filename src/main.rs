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
    let mut document = document::Document::new();
    evaluator.draw_routes(&mut document);
    evaluator.draw_stops(&mut document);
    evaluator.set_view_box(&mut document);
    let document = document.compile();
    let stdout = io::stdout();
    println!(r#"<?xml version="1.0" encoding="utf-8" ?>"#);
    println!(r#"<?xml-stylesheet type="text/css" href="test.css"?>"#);
    svg::write(&mut stdout.lock(), &document).map_err(error::EvaluatorError::Io)?;
    Ok(())
}
