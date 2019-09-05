#[macro_use]
mod macros;
mod error;
pub mod evaluator;
mod expressions;
pub mod lexer;
mod operators;
pub mod parser;
pub mod points;
pub mod statement;
mod values;

fn main() {
    println!("Hello, world!");
}
