use std::collections::HashMap;

use crate::error::{MathError, Type};
use crate::operators::{BinaryOperator, UnaryOperator};
use crate::tables::{Chain, Table};
use crate::values::Value;

pub type EResult<T> = Result<T, MathError>;

pub struct Function {
    pub args: HashMap<Variable, usize>,
    pub expression: Box<Expression>,
}

impl Function {
    fn apply(&self, args: &Vec<Expression>, vars: &impl Table<String, Value>) -> EResult<Value> {
        let locals = ArgTable {
            order: &self.args,
            args: args
                .iter()
                .map(|arg| arg.evaluate(vars))
                .collect::<EResult<Vec<Value>>>()?,
        };
        self.expression.evaluate(&Chain(&locals, vars))
    }
}

struct ArgTable<'a> {
    order: &'a HashMap<Variable, usize>,
    args: Vec<Value>,
}

impl Table<Variable, Value> for ArgTable<'_> {
    fn get(&self, key: &Variable) -> Option<&Value> {
        self.order.get(key).and_then(|&i| self.args.get(i))
    }
}

pub type Variable = String;

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

    pub fn evaluate(&self, vars: &impl Table<String, Value>) -> EResult<Value> {
        Ok(match self {
            Expression::Value(v) => *v,
            Expression::Point(p) => {
                let (x, y) = p.as_ref();
                Value::point(x.evaluate(vars)?, y.evaluate(vars)?)?
            }
            Expression::BinaryOperator(op, args) => {
                let (lhs, rhs) = args.as_ref();
                op.apply(lhs.evaluate(vars)?, rhs.evaluate(vars)?)?
            }
            Expression::UnaryOperator(op, arg) => op.apply(arg.evaluate(vars)?)?,
            Expression::Function(func, args) => func.apply(args, vars)?,
            Expression::Variable(var) => match vars.get(&var) {
                None => Err(MathError::Variable(var.clone()))?,
                Some(val) => *val,
            },
        })
    }
}
