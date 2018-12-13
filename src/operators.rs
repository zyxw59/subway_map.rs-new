use error::MathError;
use expressions::Expression;
use tables::Table;
use values::Value;

type EResult<T> = Result<T, MathError>;

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

pub struct BinaryBuiltins;

impl<K> Table<K, BinaryOperator<'static>> for BinaryBuiltins
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

pub struct UnaryBuiltins;

impl<K> Table<K, UnaryOperator<'static>> for UnaryBuiltins
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
    function: &'a dyn Fn(Value, Value) -> EResult<Value>,
}

impl<'a> BinaryOperator<'a> {
    pub fn apply(&self, lhs: Value, rhs: Value) -> EResult<Value> {
        (self.function)(lhs, rhs)
    }

    pub fn expression(&'static self, lhs: Expression, rhs: Expression) -> EResult<Expression> {
        match (lhs, rhs) {
            (Expression::Value(lhs), Expression::Value(rhs)) => self.apply(lhs, rhs).map(Expression::Value),
            (lhs, rhs) => Ok(Expression::BinaryOperator(self, Box::new((lhs, rhs)))),
        }
    }
}

pub struct UnaryOperator<'a> {
    pub precedence: usize,
    function: &'a dyn Fn(Value) -> EResult<Value>,
}

impl<'a> UnaryOperator<'a> {
    pub fn apply(&self, argument: Value) -> EResult<Value> {
        (self.function)(argument)
    }

    pub fn expression(&'static self, argument: Expression) -> EResult<Expression> {
        match argument {
            Expression::Value(argument) => self.apply(argument).map(Expression::Value),
            argument => Ok(Expression::UnaryOperator(self, Box::new(argument))),
        }
    }
}
