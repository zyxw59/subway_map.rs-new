use std::error;
use std::io::BufRead;

use regex_syntax::is_word_character;

type Error = Box<dyn error::Error>;
type EResult<T> = Result<T, Error>;

pub struct Lexer<R> {
    input: R,
    str_buffer: String,
    vec_buffer: Vec<char>,
    pos: usize,
    line: usize,
}

impl<R: BufRead> Lexer<R> {
    pub fn new(input: R) -> Lexer<R> {
        Lexer {
            input,
            str_buffer: String::new(),
            vec_buffer: Vec::new(),
            pos: 0,
            line: 0,
        }
    }

    fn fill_buffer(&mut self) -> EResult<usize> {
        self.str_buffer.clear();
        self.vec_buffer.clear();
        self.input.read_line(&mut self.str_buffer)?;
        self.vec_buffer.extend(self.str_buffer.chars());
        self.pos = 0;
        self.line += 1;
        Ok(self.vec_buffer.len())
    }

    /// Returns the current character, calling `fill_buffer` as necessary.
    ///
    /// If the buffer is still empty after `fill_buffer`, returns `None`, indicating end of input.
    fn get(&mut self) -> EResult<Option<char>> {
        Ok(match self.vec_buffer.get(self.pos).cloned() {
            // if we've got a character, here it is
            Some(c) => Some(c),
            // if we don't, check if we can get a new buffer
            None => {
                self.fill_buffer()?;
                // and return the new character, if any
                self.vec_buffer.get(self.pos).cloned()
            }
        })
    }

    fn skip_whitespace(&mut self) -> EResult<()> {
        // increment as long as we've got whitespace
        while let Some(CharCat::Whitespace) = self.get()?.map(CharCat::from) {
            self.pos += 1;
        }
        Ok(())
    }

    fn parse_word(&mut self) -> EResult<Token> {
        let mut word = String::new();
        while let Some(c) = self.get()? {
            if is_word_character(c) {
                word.push(c);
                self.pos += 1;
            } else {
                break;
            }
        }
        Ok(Token::Tag(word))
    }

    fn parse_string(&mut self) -> EResult<Token> {
        self.pos += 1;
        let mut s = String::new();
        while let Some(c) = self.get()? {
            self.pos += 1;
            match c {
                '"' => return Ok(Token::String(s)),
                '\\' => {
                    match self.get()? {
                        Some('n') => s.push('\n'),
                        Some(c) => s.push(c),
                        None => Err("Trailing `\\`")?,
                    }
                    self.pos += 1;
                }
                c => s.push(c),
            }
        }
        Err("Unterminated string".into())
    }

    fn parse_dot(&mut self) -> EResult<Token> {
        match self.vec_buffer.get(self.pos + 1).cloned().map(CharCat::from) {
            Some(CharCat::Number) => self.parse_number(),
            _ => self.parse_other(CharCat::Dot),
        }
    }

    fn parse_number(&mut self) -> EResult<Token> {
        let mut mantissa = 0_u64;
        let mut sigfigs = 0_u8;
        let mut exponent = 0_i32;
        let mut has_decimal = false;
        // parse the first 17 sigfigs. any more sigfigs after that are irrelevant
        while sigfigs < 17 {
            if let Some(c) = self.get()? {
                match c.into() {
                    CharCat::Number => {
                        self.pos += 1;
                        mantissa *= 10;
                        mantissa += c as u64 - '0' as u64;
                        sigfigs += 1;
                        // if we've matched a decimal, but we're still matching sigfigs, decrease
                        // the exponent
                        if has_decimal {
                            exponent -= 1;
                        }
                    }
                    // ignore underscores
                    CharCat::Underscore => {
                        self.pos += 1;
                    }
                    // if we haven't matched a decimal yet, well now we have. otherwise it's a
                    // second dot, and we should quit
                    CharCat::Dot if !has_decimal => {
                        self.pos += 1;
                        has_decimal = true;
                    }
                    _ => break,
                }
            } else {
                break;
            }
        }
        while let Some(c) = self.get()? {
            match c.into() {
                CharCat::Number => {
                    self.pos += 1;
                    // if we haven't matched a decimal yet, we're adding insignificant digits
                    // before the decimal, so we're increasing the exponent
                    if !has_decimal {
                        exponent += 1;
                    }
                }
                // ignore underscores
                CharCat::Underscore => {
                    self.pos += 1;
                }
                // if we haven't matched a decimal yet, well now we have. otherwise it's a
                // second dot, and we should quit
                CharCat::Dot if !has_decimal => {
                    self.pos += 1;
                    has_decimal = true;
                }
                _ => break,
            }
        }

        if mantissa == 0 {
            Ok(Token::Number(0.0))
        } else if exponent == 0 {
            return Ok(Token::Number(mantissa as f64));
        } else {
            Ok(Token::Number(10.0_f64.powi(exponent) * mantissa as f64))
        }
    }

    fn parse_other(&mut self, cat: CharCat) -> EResult<Token> {
        let mut tag = String::new();
        while let Some(c) = self.get()? {
            if CharCat::from(c) == cat {
                self.pos += 1;
                tag.push(c);
            } else {
                break;
            }
        }
        Ok(Token::Tag(tag))
    }
}

impl<R: BufRead> Iterator for Lexer<R> {
    type Item = EResult<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        itry!(self.skip_whitespace());
        itry!(self.get()).map(|c| {
            match c.into() {
                CharCat::Letter | CharCat::Underscore => self.parse_word(),
                CharCat::Number => self.parse_number(),
                CharCat::Dot => self.parse_dot(),
                CharCat::Quote => self.parse_string(),
                CharCat::Singleton => Ok(Token::singleton(c).unwrap()),
                cat => self.parse_other(cat),
            }
        })
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum CharCat {
    Letter,
    Underscore,
    Number,
    Dot,
    Quote,
    Singleton,
    LeftBracket,
    RightBracket,
    Whitespace,
    Other,
}

impl From<char> for CharCat {
    fn from(c: char) -> CharCat {
        use self::CharCat::*;
        match c {
            '0'..='9' => Number,
            '.' => Dot,
            '_' => Underscore,
            '"' => Quote,
            '(' | ')' | ',' | ';' => Singleton,
            '[' => LeftBracket,
            ']' => RightBracket,
            c if is_word_character(c) => Letter,
            c if c.is_whitespace() => Whitespace,
            _ => Other,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    /// A tag
    Tag(String),
    /// A literal number
    Number(f64),
    /// A literal string
    String(String),
    /// A left parenthesis
    LeftParen,
    /// A right parenthesis
    RightParen,
    /// A comma
    Comma,
    /// A semicolon
    Semicolon,
}

impl Token {
    fn singleton(c: char) -> Option<Token> {
        match c {
            '(' => Some(Token::LeftParen),
            ')' => Some(Token::RightParen),
            ',' => Some(Token::Comma),
            ';' => Some(Token::Semicolon),
            _ => None,
        }
    }
}
