use std::result;

use crate::error::{MathError, Type};
use crate::operators::{BinaryOperator, UnaryOperator};
use crate::values::Value;

pub type Result = result::Result<Expression, MathError>;

pub struct Function {}

pub enum Expression {
    Value(Value),
    Point(Box<(Expression, Expression)>),
    BinaryOperator(&'static BinaryOperator<'static>, Box<(Expression, Expression)>),
    UnaryOperator(&'static UnaryOperator<'static>, Box<Expression>),
    Function(Function, Vec<Expression>),
}

impl Expression {
    pub fn point(x: Expression, y: Expression) -> Result {
        use self::Expression::Value;
        use self::Value::*;
        match (x, y) {
            (Value(Number(x)), Value(Number(y))) => Ok(Value(Point(x, y))),
            (Value(Number(_)), Value(y)) => Err(MathError::Type(Type::Number, y.into())),
            (Value(x), _) => Err(MathError::Type(Type::Number, x.into())),
            (x, y) => Ok(Expression::Point(Box::new((x, y)))),
        }
    }

    #[cfg(test)]
    pub fn unwrap_value(self) -> Value {
        match self {
            Expression::Value(value) => value,
            _ => panic!("`unwrap_value` called on non-value `Expression`"),
        }
    }
}
