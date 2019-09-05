use std::collections::HashMap;
use std::convert::TryFrom;

use crate::error::{EvaluatorError, MathError, Result as EResult};
use crate::expressions::{Function, Variable};
use crate::points::PointCollection;
use crate::statement::{PointStatement, Statement, StatementKind};
use crate::values::{Point, Value};

pub trait EvaluationContext {
    fn get_variable(&self, name: &str) -> Option<Value>;

    fn get_function(&self, name: &str) -> Option<&Function>;
}

#[derive(Default)]
pub struct Evaluator {
    variables: HashMap<Variable, Value>,
    functions: HashMap<Variable, Function>,
    points: PointCollection,
}

impl Evaluator {
    fn new() -> Evaluator {
        Evaluator {
            variables: HashMap::new(),
            functions: HashMap::new(),
            points: PointCollection::new(),
        }
    }

    fn evaluate_all(&mut self, parser: impl Iterator<Item = EResult<Statement>>) -> EResult<()> {
        for statement in parser {
            self.evaluate(statement?)?;
        }
        Ok(())
    }

    fn evaluate(&mut self, Statement { statement, line }: Statement) -> EResult<()> {
        match statement {
            StatementKind::Null => {}
            StatementKind::Variable(name, expr) => {
                let value = expr
                    .evaluate(self)
                    .map_err(|err| EvaluatorError::Math(err, line))?;
                self.variables.insert(name, value);
            }
            StatementKind::Function(name, function) => {
                self.functions.insert(name, function);
            }
            StatementKind::Point(PointStatement::Single(name, expr)) => {
                // points can't be redefined, since lines are defined in terms of them
                if let Some(original_line) = self.points.get_point_line_number(&name) {
                    return Err(EvaluatorError::PointRedefinition(name, line, original_line).into());
                }
                let value = expr
                    .evaluate(self)
                    .and_then(Point::try_from)
                    .map_err(|err| EvaluatorError::Math(err, line))?;
                self.variables.insert(name.clone(), value.into());
                self.points.insert_point(name, value, line);
            }
            StatementKind::Point(PointStatement::Spaced {
                from,
                spaced,
                points,
            }) => {
                let spacing = spaced
                    .evaluate(self)
                    .and_then(Point::try_from)
                    .map_err(|err| EvaluatorError::Math(err, line))?;
                let from_point = if let Some(from_point) = self.points.get_point(&from) {
                    from_point
                } else {
                    return Err(EvaluatorError::Math(MathError::Variable(from), line).into());
                };
                let mut distance = 0.0;
                let mut distances = Vec::new();
                for (multiplier, name) in points {
                    // check for redefinition of point
                    if let Some(original_line) = self.points.get_point_line_number(&name) {
                        return Err(
                            EvaluatorError::PointRedefinition(name, line, original_line).into()
                        );
                    }
                    distance += if let Some(expr) = multiplier {
                        expr.evaluate(self)
                            .and_then(f64::try_from)
                            .map_err(|err| EvaluatorError::Math(err, line))?
                    } else {
                        1.0
                    };
                    let point = from_point + distance * spacing;
                    self.variables.insert(name.clone(), point.into());
                    distances.push((name, distance));
                }
                self.points.new_line(from, spacing, distances, line);
            }
            _ => {
                unimplemented!();
            }
        }
        Ok(())
    }
}

impl EvaluationContext for Evaluator {
    fn get_variable(&self, name: &str) -> Option<Value> {
        self.variables
            .get(name)
            .copied()
            .or_else(|| self.points.get_point(name).map(Value::from))
    }

    fn get_function(&self, name: &str) -> Option<&Function> {
        self.functions.get(name)
    }
}

impl EvaluationContext for () {
    fn get_variable(&self, _name: &str) -> Option<Value> {
        None
    }

    fn get_function(&self, _name: &str) -> Option<&Function> {
        None
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::parser::LexerExt;
    use crate::values::{Point, Value};

    use super::Evaluator;

    #[test]
    fn variables_set() {
        let parser = Lexer::new("x = 1;".as_bytes()).into_parser();
        let mut evaluator = Evaluator::new();
        evaluator.evaluate_all(parser).unwrap();
        assert_eq!(evaluator.variables.get("x"), Some(&Value::Number(1.0)));
    }

    #[test]
    fn variables_get() {
        let parser = Lexer::new("x = 1; z = x * 2;".as_bytes()).into_parser();
        let mut evaluator = Evaluator::new();
        evaluator.evaluate_all(parser).unwrap();
        assert_eq!(evaluator.variables.get("x"), Some(&Value::Number(1.0)));
        assert_eq!(evaluator.variables.get("z"), Some(&Value::Number(2.0)));
    }

    #[test]
    fn functions() {
        let parser = Lexer::new("fn f(x) = x + 1; y = f(3);".as_bytes()).into_parser();
        let mut evaluator = Evaluator::new();
        evaluator.evaluate_all(parser).unwrap();
        assert_eq!(evaluator.variables.get("y"), Some(&Value::Number(4.0)));
    }

    #[test]
    fn functions_2() {
        let parser = Lexer::new("fn f(x, y) = x * y; z = f(3, 2);".as_bytes()).into_parser();
        let mut evaluator = Evaluator::new();
        evaluator.evaluate_all(parser).unwrap();
        assert_eq!(evaluator.variables.get("z"), Some(&Value::Number(6.0)));
    }

    #[test]
    fn point_single() {
        let parser = Lexer::new("point a = (1, 1);".as_bytes()).into_parser();
        let mut evaluator = Evaluator::new();
        evaluator.evaluate_all(parser).unwrap();
        assert_eq!(evaluator.variables.get("a"), Some(&Value::Point(1.0, 1.0)));
        assert_eq!(evaluator.points.get_point("a"), Some(Point(1.0, 1.0)));
    }
}
