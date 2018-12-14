use crate::error::{MathError, Type};
use crate::operators::{BinaryOperator, UnaryOperator};
use crate::values::Value;

pub type EResult<T> = Result<T, MathError>;

pub struct Function {}

impl Function {
    fn apply(self, args: Vec<Expression>) -> EResult<Value> {
        unimplemented!();
    }
}

#[derive(Debug)]
pub struct Variable {}

pub enum Expression {
    Value(Value),
    Point(Box<(Expression, Expression)>),
    BinaryOperator(
        &'static BinaryOperator<'static>,
        Box<(Expression, Expression)>,
    ),
    UnaryOperator(&'static UnaryOperator<'static>, Box<Expression>),
    Function(Function, Vec<Expression>),
    Variable(Variable),
}

impl Expression {
    pub fn point(x: Expression, y: Expression) -> EResult<Expression> {
        use self::Expression::Value;
        use self::Value::*;
        match (x, y) {
            (Value(Number(x)), Value(Number(y))) => Ok(Value(Point(x, y))),
            (Value(Number(_)), Value(y)) => Err(MathError::Type(Type::Number, y.into())),
            (Value(x), _) => Err(MathError::Type(Type::Number, x.into())),
            (x, y) => Ok(Expression::Point(Box::new((x, y)))),
        }
    }

    pub fn evaluate(self) -> EResult<Value> {
        Ok(match self {
            Expression::Value(v) => v,
            Expression::Point(p) => {
                let (x, y) = *p;
                Value::point(x.evaluate()?, y.evaluate()?)?
            }
            Expression::BinaryOperator(op, args) => {
                let (lhs, rhs) = *args;
                op.apply(lhs.evaluate()?, rhs.evaluate()?)?
            }
            Expression::UnaryOperator(op, arg) => op.apply(arg.evaluate()?)?,
            Expression::Function(func, args) => func.apply(args)?,
            Expression::Variable(v) => Err(MathError::Variable(v))?,
        })
    }

    /// Finds the first `Variable` in the expression tree, or `None` if there are none.
    pub fn find_variable(self) -> Option<Variable> {
        use self::Expression::*;
        match self {
            Value(_) => None,
            Point(p) | BinaryOperator(_, p) => {
                let (x, y) = *p;
                x.find_variable().or_else(|| y.find_variable())
            }
            UnaryOperator(_, x) => x.find_variable(),
            Function(_, v) => v.into_iter().filter_map(|x| x.find_variable()).next(),
            Variable(v) => Some(v),
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
