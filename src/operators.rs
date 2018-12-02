use std::cmp;
use std::error;
use std::ops;

use expressions::Expression;
use lexer::Token;

type Error = Box<dyn error::Error>;
type EResult<T> = Result<T, Error>;

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

impl<'a> cmp::PartialOrd<usize> for BinaryOperator<'a> {
    fn partial_cmp(&self, other: &usize) -> Option<cmp::Ordering> {
        self.precedence.partial_cmp(other)
    }
}

impl<'a> cmp::PartialEq<usize> for BinaryOperator<'a> {
    fn eq(&self, other: &usize) -> bool {
        self.precedence == *other
    }
}

impl<'a, 'b> cmp::PartialOrd<BinaryOperator<'a>> for BinaryOperator<'b> {
    fn partial_cmp(&self, other: &BinaryOperator<'a>) -> Option<cmp::Ordering> {
        self.precedence.partial_cmp(&other.precedence)
    }
}

impl<'a, 'b> cmp::PartialEq<BinaryOperator<'a>> for BinaryOperator<'b> {
    fn eq(&self, other: &BinaryOperator<'a>) -> bool {
        self.precedence == other.precedence
    }
}
