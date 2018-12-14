#[macro_use]
mod macros;
mod error;
mod expressions;
pub mod lexer;
mod operators;
pub mod parser;
pub mod tables;
mod values;

fn main() {
    println!("Hello, world!");
}
