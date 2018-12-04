extern crate failure;
#[macro_use]
extern crate failure_derive;
extern crate regex_syntax;

#[macro_use]
mod macros;
mod error;
mod expressions;
pub mod lexer;
mod operators;
pub mod parser;
pub mod tables;

fn main() {
    println!("Hello, world!");
}
