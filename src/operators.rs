use crate::error::MathError;
use crate::expressions::Expression;
use crate::tables::Table;
use crate::values::Value;

type EResult<T> = Result<T, MathError>;

enum Precedence {
    /// Comparison operators, such as `==`, `>`, `<>`, etc.
    Comparison,
    /// Additive operators, such as `+`, `-`, `++`, etc.
    Additive,
    /// Multiplicative operators, such as `*`, `/`, `&`, etc., as well as unary minus.
    Multiplicative,
}

mod builtins {
    use std::ops;

    use crate::values::Value;

    use super::{BinaryOperator, Precedence, UnaryOperator};

    macro_rules! bin_op {
        ($name:ident ( $prec:ident, $fun:path )) => {
            pub const $name: BinaryOperator<'static> = BinaryOperator {
                precedence: Precedence::$prec as usize,
                function: &$fun,
            };
        };
    }

    bin_op! {ADD(Additive, ops::Add::add)}
    bin_op! {SUB(Additive, ops::Sub::sub)}
    bin_op! {HYPOT(Additive, Value::hypot)}
    bin_op! {HYPOT_SUB(Additive, Value::hypot_sub)}
    bin_op! {MUL(Multiplicative, ops::Mul::mul)}
    bin_op! {DIV(Multiplicative, ops::Div::div)}

    macro_rules! unary_op {
        ($name:ident ( $prec:ident, $fun:path )) => {
            pub const $name: UnaryOperator<'static> = UnaryOperator {
                precedence: Precedence::$prec as usize,
                function: &$fun,
            };
        };
    }

    unary_op! {NEG(Multiplicative, ops::Neg::neg)}
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
            "++" => Some(&builtins::HYPOT),
            "+-+" => Some(&builtins::HYPOT_SUB),
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
            (Expression::Value(lhs), Expression::Value(rhs)) => {
                self.apply(lhs, rhs).map(Expression::Value)
            }
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
