extern crate regex_syntax;

#[macro_use]
mod macros;
mod expressions;
pub mod lexer;
mod operators;
pub mod parser;

fn main() {
    println!("Hello, world!");
}
