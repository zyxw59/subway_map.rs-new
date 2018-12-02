use std::cmp;
use std::error;
use std::ops;

use expressions::Expression;
use lexer::Token;

type Error = Box<dyn error::Error>;
type EResult<T> = Result<T, Error>;

macro_rules! impl_ord {
    ( $name:tt<$($args:tt),*> ) => {
        impl<$($args),*> cmp::PartialOrd<usize> for $name<$($args),*> {
            fn partial_cmp(&self, other: &usize) -> Option<cmp::Ordering> {
                self.precedence.partial_cmp(other)
            }
        }

        impl<$($args),*> cmp::PartialEq<usize> for $name<$($args),*> {
            fn eq(&self, other: &usize) -> bool {
                self.precedence == *other
            }
        }
    }
}

pub struct BinaryOperator<'a> {
    pub precedence: usize,
    function: &'a dyn Fn(Expression, Expression) -> EResult<Expression>,
}

impl<'a> BinaryOperator<'a> {
    pub fn apply(&self, lhs: Expression, rhs: Expression) -> EResult<Expression> {
        (self.function)(lhs, rhs)
    }

    pub fn from_builtin(token: &Token, precedence: usize) -> Option<BinaryOperator<'static>> {
        BinaryOperator::from_builtin_core(token).filter(|op| op >= &precedence)
    }

    fn from_builtin_core(token: &Token) -> Option<BinaryOperator<'static>> {
        match token {
            Token::Tag(tag) => match tag.as_ref() {
                "+" => Some(BinaryOperator {
                    precedence: 1,
                    function: &ops::Add::add,
                }),
                "-" => Some(BinaryOperator {
                    precedence: 1,
                    function: &ops::Sub::sub,
                }),
                "*" => Some(BinaryOperator {
                    precedence: 2,
                    function: &ops::Mul::mul,
                }),
                "/" => Some(BinaryOperator {
                    precedence: 2,
                    function: &ops::Div::div,
                }),
                _ => None,
            },
            _ => None,
        }
    }
}

impl_ord! { BinaryOperator<'a> }

pub struct UnaryOperator<'a> {
    pub precedence: usize,
    function: &'a dyn Fn(Expression) -> EResult<Expression>,
}

impl<'a> UnaryOperator<'a> {
    pub fn apply(&self, argument: Expression) -> EResult<Expression> {
        (self.function)(argument)
    }

    pub fn from_builtin(token: &Token) -> Option<UnaryOperator<'static>> {
        match token {
            Token::Tag(tag) => match tag.as_ref() {
                "-" => Some(UnaryOperator {
                    precedence: 2,
                    function: &ops::Neg::neg,
                }),
                _ => None,
            },
            _ => None,
        }
    }
}
