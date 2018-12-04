use expressions::{Expression, Result};
use lexer::Token;
use tables::Table;

mod builtins {
    use std::ops;

    use super::{BinaryOperator, UnaryOperator};

    macro_rules! bin_op {
        ($name:ident ( $prec:expr, $fun:path )) => {
            pub const $name: BinaryOperator<'static> = BinaryOperator {
                precedence: $prec,
                function: &$fun,
            };
        };
    }

    bin_op!{ADD(1, ops::Add::add)}
    bin_op!{SUB(1, ops::Sub::sub)}
    bin_op!{MUL(2, ops::Mul::mul)}
    bin_op!{DIV(2, ops::Div::div)}

    macro_rules! unary_op {
        ($name:ident ( $prec:expr, $fun:path )) => {
            pub const $name: UnaryOperator<'static> = UnaryOperator {
                precedence: $prec,
                function: &$fun,
            };
        };
    }

    unary_op!{NEG(2, ops::Neg::neg)}
}

pub struct Builtins;

impl<K> Table<K, BinaryOperator<'static>> for Builtins
where
    K: ?Sized + AsRef<str>,
{
    fn get(&self, key: &K) -> Option<&'static BinaryOperator<'static>> {
        match key.as_ref() {
            "+" => Some(&builtins::ADD),
            "-" => Some(&builtins::SUB),
            "*" => Some(&builtins::MUL),
            "/" => Some(&builtins::DIV),
            _ => None,
        }
    }
}

impl<K> Table<K, UnaryOperator<'static>> for Builtins
where
    K: ?Sized + AsRef<str>,
{
    fn get(&self, key: &K) -> Option<&'static UnaryOperator<'static>> {
        match key.as_ref() {
            "-" => Some(&builtins::NEG),
            _ => None,
        }
    }
}

pub struct BinaryOperator<'a> {
    pub precedence: usize,
    function: &'a dyn Fn(Expression, Expression) -> Result,
}

impl<'a> BinaryOperator<'a> {
    pub fn apply(&self, lhs: Expression, rhs: Expression) -> Result {
        (self.function)(lhs, rhs)
    }

    pub fn from_builtin(token: &Token) -> Option<&'static BinaryOperator<'static>> {
        match token {
            Token::Tag(tag) => Builtins.get(tag),
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

    pub fn from_builtin(token: &Token) -> Option<&'static UnaryOperator<'static>> {
        match token {
            Token::Tag(tag) => Builtins.get(tag),
            _ => None,
        }
    }
}
