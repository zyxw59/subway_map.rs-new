use std::io;

#[macro_use]
mod macros;
mod corner;
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
    //println!("{:#?}", evaluator);
    let stdout = io::stdout();
    println!(
        r#"<?xml version="1.0" encoding="utf-8" ?>
<?xml-stylesheet type="text/css" href="test.css"?>
<svg width="1100" height="1200"
viewBox="-200, 0 900, 1200"
xmlns="http://www.w3.org/2000/svg"
xmlns:xlink="http://www.w3.org/1999/xlink">
<g fill="none" stroke-width="8">"#
    );
    evaluator
        .draw_routes(&mut stdout.lock())
        .map_err(error::EvaluatorError::Io)?;
    println!("</g>");
    evaluator
        .draw_points(&mut stdout.lock())
        .map_err(error::EvaluatorError::Io)?;
    println!("</svg>");
    Ok(())
}
