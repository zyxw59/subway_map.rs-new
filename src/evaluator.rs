use std::collections::HashMap;
use std::convert::TryFrom;

use crate::error::{EvaluatorError, Result as EResult};
use crate::expressions::{Function, Variable};
use crate::points::PointCollection;
use crate::statement::{PointStatement, Statement, StatementKind};
use crate::tables::TableMut;
use crate::values::{Point, Value};

#[derive(Default)]
pub struct Evaluator<V = HashMap<Variable, Value>, F = HashMap<Variable, Function>> {
    variables: V,
    functions: F,
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
}

impl<V, F> Evaluator<V, F>
where
    V: TableMut<Variable, Value>,
    F: TableMut<Variable, Function>,
{
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
                    .evaluate(&self.variables, &self.functions)
                    .map_err(|err| EvaluatorError::Math(err, line))?;
                self.variables.insert(name, value);
            }
            StatementKind::Function(name, function) => {
                self.functions.insert(name, function);
            }
            StatementKind::Point(PointStatement::Single(name, expr)) => {
                if let Some(original_line) = self.points.get_point_line_number(&name) {
                    return Err(EvaluatorError::PointRedefinition(name, line, original_line).into());
                }
                let value = expr
                    .evaluate(&self.variables, &self.functions)
                    .and_then(Point::try_from)
                    .map_err(|err| EvaluatorError::Math(err, line))?;
                self.variables.insert(name.clone(), value.into());
                self.points.insert_point(name, value, line);
            }
            _ => {
                unimplemented!();
            }
        }
        Ok(())
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
