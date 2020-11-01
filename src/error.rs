use std::io;
use std::result;

use thiserror::Error;

use crate::expressions::Variable;
use crate::lexer::Token;
use crate::values::Value;

pub type Result<T> = result::Result<T, Error>;

#[derive(Error, Debug)]
pub enum Error {
    #[error("The lexer encountered an error: {0}")]
    Lexer(#[from] LexerError),
    #[error("The parser encountered an error: {0}")]
    Parser(#[from] ParserError),
    #[error("The evaluator encountered an error: {0}")]
    Evaluator(#[from] EvaluatorError),
}

#[derive(Error, Debug)]
pub enum ParserError {
    #[error("Unexpected end of input on line {0}")]
    EndOfInput(usize),
    #[error("Unexpected token {0:?} on line {1}")]
    Token(Token, usize),
    #[error("Unclosed parentheses starting on line {0}")]
    Parentheses(usize),
    #[error(
        "Too many items in parenthesized list starting on line {1} (got {0}, expected 1 or 2)"
    )]
    ParenList(usize, usize),
    #[error("Repeated argument {0} to function {1} on line {2}")]
    Argument(String, String, usize),
}

#[derive(Error, Debug)]
pub enum EvaluatorError {
    #[error("A math error ({0}) occured on line {1}")]
    Math(#[source] MathError, usize),
    #[error("Point ({0}) redefined on line {1} (originally defined on line {2}")]
    PointRedefinition(Variable, usize, usize),
    #[error("Route ({0}) redefined on line {1} (originally defined on line {2}")]
    RouteRedefinition(Variable, usize, usize),
    #[error("An IO error ({0}) occurred during output")]
    Io(#[from] io::Error),
    #[error("An error ({0}) occurred during debug output")]
    DebugOutput(#[from] serde_json::Error),
}

#[derive(Error, Debug)]
pub enum MathError {
    #[error("A type error occured (expected {0:?}, got {1:?})")]
    Type(Type, Type),
    #[error("Division by zero")]
    DivisionByZero,
    #[error("Intersection of parallel lines")]
    ParallelIntersection,
    #[error("Domain error")]
    Domain,
    #[error("Undefined variable {0:?}")]
    Variable(Variable),
    #[error("Undefined function {0:?}")]
    Function(Variable),
    #[error("Incorrect number of arguments to function (expected {0}, got {1})")]
    Arguments(usize, usize),
}

#[derive(Debug)]
pub enum Type {
    Number,
    Point,
    Line,
}

impl From<Value> for Type {
    fn from(expr: Value) -> Type {
        match expr {
            Value::Number(_) => Type::Number,
            Value::Point(..) => Type::Point,
            Value::Line(..) => Type::Line,
        }
    }
}

#[derive(Error, Debug)]
pub enum LexerError {
    #[error("Unterminated string at line {0}")]
    UnterminatedString(usize),
    #[error("Invalid UTF-8 at line {0}")]
    Unicode(usize),
    #[error("An IO error ({0}) occured whil reading line {1}")]
    Io(#[source] io::Error, usize),
}

impl LexerError {
    pub fn from_io(err: io::Error, line: usize) -> LexerError {
        match err.kind() {
            io::ErrorKind::InvalidData => LexerError::Unicode(line),
            _ => LexerError::Io(err, line),
        }
    }
}
