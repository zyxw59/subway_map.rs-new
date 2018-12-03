use std::ops;

use expressions::{Expression, Result};
use lexer::Token;

pub struct BinaryOperator<'a> {
    pub precedence: usize,
    function: &'a dyn Fn(Expression, Expression) -> Result,
}

impl<'a> BinaryOperator<'a> {
    pub fn apply(&self, lhs: Expression, rhs: Expression) -> Result {
        (self.function)(lhs, rhs)
    }

    pub fn from_builtin(token: &Token, precedence: usize) -> Option<BinaryOperator<'static>> {
        BinaryOperator::from_builtin_core(token).filter(|op| op.precedence >= precedence)
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

pub struct UnaryOperator<'a> {
    pub precedence: usize,
    function: &'a dyn Fn(Expression) -> Result,
}

impl<'a> UnaryOperator<'a> {
    pub fn apply(&self, argument: Expression) -> Result {
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
