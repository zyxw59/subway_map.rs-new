use std::io;
use std::result;

use failure::Fail;

use crate::expressions::Variable;
use crate::lexer::Token;
use crate::values::Value;

pub type Result<T> = result::Result<T, Error>;

#[derive(Fail, Debug)]
pub enum Error {
    #[fail(display = "The lexer encountered an error: {}", _0)]
    Lexer(#[cause] LexerError),
    #[fail(display = "The parser encountered an error: {}", _0)]
    Parser(#[cause] ParserError),
}

impl From<ParserError> for Error {
    fn from(err: ParserError) -> Error {
        Error::Parser(err)
    }
}

impl From<LexerError> for Error {
    fn from(err: LexerError) -> Error {
        Error::Lexer(err)
    }
}

#[derive(Fail, Debug)]
pub enum ParserError {
    #[fail(display = "Unexpected end of input on line {}", _0)]
    EndOfInput(usize),
    #[fail(display = "Unexpected token {:?} on line {}", _0, _1)]
    Token(Token, usize),
    #[fail(display = "Unclosed parentheses starting on line {}", _0)]
    Parentheses(usize),
    #[fail(
        display = "Too many items in parenthesized list starting on line {} (got {}, expected 1 or 2)",
        _1, _0
    )]
    ParenList(usize, usize),
    #[fail(display = "A math error ({}) occured on line {}", _0, _1)]
    Math(#[cause] MathError, usize),
}

impl ParserError {
    pub fn type_error(expected: Type, got: Type, line: usize) -> ParserError {
        ParserError::Math(MathError::Type(expected, got), line)
    }
}

#[derive(Fail, Debug)]
pub enum MathError {
    #[fail(display = "A type error occured (expected {:?}, got {:?})", _0, _1)]
    Type(Type, Type),
    #[fail(display = "Division by zero")]
    DivisionByZero,
    #[fail(display = "Undefined variable {:?}", _0)]
    Variable(Variable),
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
        }
    }
}

#[derive(Fail, Debug)]
pub enum LexerError {
    #[fail(display = "Unterminated string at line {}", _0)]
    UnterminatedString(usize),
    #[fail(display = "Invalid UTF-8 at line {}", _0)]
    Unicode(usize),
    #[fail(display = "An IO error ({}) occured whil reading line {}", _0, _1)]
    Io(#[cause] io::Error, usize),
}

impl LexerError {
    pub fn from_io(err: io::Error, line: usize) -> LexerError {
        match err.kind() {
            io::ErrorKind::InvalidData => LexerError::Unicode(line),
            _ => LexerError::Io(err, line),
        }
    }
}
