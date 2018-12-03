use std::io;
use std::result;

use failure::Fail;

pub type Result<T> = result::Result<T, Error>;

#[derive(Fail, Debug)]
pub enum Error {
    #[fail(display = "The lexer encountered an error: {}", _0)]
    Lexer(#[cause] LexerError),
    #[fail(display = "The parser encountered an error: {}", _0)]
    Parser(#[cause] ParserError),
    #[fail(display = "A math error occured: {}", _0)]
    Math(#[cause] MathError),
}

impl From<MathError> for Error {
    fn from(err: MathError) -> Error {
        Error::Math(err)
    }
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
pub enum MathError {
    #[fail(display = "A type error occured")]
    Type,
    #[fail(display = "Division by zero")]
    DivisionByZero,
}

#[derive(Fail, Debug)]
pub enum ParserError {
    #[fail(display = "Unexpected end of input on line {}", _0)]
    EndOfInput(usize),
    #[fail(display = "Unexpected token on line {}", _0)]
    Token(usize),
    #[fail(display = "Unclosed parentheses")]
    Parentheses,
}

#[derive(Fail, Debug)]
pub enum LexerError {
    #[fail(display = "Unterminated string at line {}", _0)]
    UnterminatedString(usize),
    #[fail(display = "Invalid UTF-8 at line {}", _0)]
    Unicode(usize),
    #[fail(
        display = "An IO error {} occured whil reading line {}",
        _0,
        _1
    )]
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
