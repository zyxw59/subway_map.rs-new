use std::collections::HashMap;

use crate::error::MathError;
use crate::operators::{BinaryOperator, UnaryOperator};
use crate::tables::{Chain, Table};
use crate::values::Value;

pub type EResult<T> = Result<T, MathError>;

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub args: HashMap<Variable, usize>,
    pub expression: Expression,
}

impl Function {
    fn apply(
        &self,
        args: &[Expression],
        vars: &impl Table<Variable, Value>,
        funcs: &impl Table<Variable, Function>,
    ) -> EResult<Value> {
        let expected = self.args.len();
        let got = args.len();
        if expected != got {
            return Err(MathError::Arguments(expected, got));
        }
        let locals = ArgTable {
            order: &self.args,
            args: args
                .iter()
                .map(|arg| arg.evaluate(vars, funcs))
                .collect::<EResult<Vec<Value>>>()?,
        };
        self.expression.evaluate(&Chain(&locals, vars), funcs)
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

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Value(Value),
    Point(Box<(Expression, Expression)>),
    BinaryOperator(
        &'static BinaryOperator<'static>,
        Box<(Expression, Expression)>,
    ),
    UnaryOperator(&'static UnaryOperator<'static>, Box<Expression>),
    Function(Variable, Vec<Expression>),
    Variable(Variable),
}

impl Expression {
    pub fn evaluate(
        &self,
        vars: &impl Table<Variable, Value>,
        funcs: &impl Table<Variable, Function>,
    ) -> EResult<Value> {
        Ok(match self {
            Expression::Value(v) => *v,
            Expression::Point(p) => {
                let (x, y) = p.as_ref();
                Value::point(x.evaluate(vars, funcs)?, y.evaluate(vars, funcs)?)?
            }
            Expression::BinaryOperator(op, args) => {
                let (lhs, rhs) = args.as_ref();
                op.apply(lhs.evaluate(vars, funcs)?, rhs.evaluate(vars, funcs)?)?
            }
            Expression::UnaryOperator(op, arg) => op.apply(arg.evaluate(vars, funcs)?)?,
            Expression::Function(func_name, args) => match funcs.get(&func_name) {
                None => Err(MathError::Function(func_name.clone()))?,
                Some(func) => func.apply(args, vars, funcs)?,
            },
            Expression::Variable(var) => match vars.get(&var) {
                None => Err(MathError::Variable(var.clone()))?,
                Some(val) => *val,
            },
        })
    }
}
