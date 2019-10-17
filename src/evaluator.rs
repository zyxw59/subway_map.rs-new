use std::collections::HashMap;
use std::convert::TryFrom;

use crate::error::{EvaluatorError, MathError, Result as EResult, Type};
use crate::expressions::{Function, Variable};
use crate::points::PointCollection;
use crate::statement::{Segment, Statement, StatementKind};
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
    routes: Vec<Route>,
}

impl Evaluator {
    fn new() -> Evaluator {
        Evaluator {
            variables: HashMap::new(),
            functions: HashMap::new(),
            points: PointCollection::new(),
            routes: Vec::new(),
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
                // named points can't be redefined, since lines are defined in terms of them
                if let Some(original_line) = self.points.get_point_line_number(&name) {
                    return Err(EvaluatorError::PointRedefinition(name, line, original_line).into());
                }
                let value = expr
                    .evaluate(self)
                    .map_err(|err| EvaluatorError::Math(err, line))?;
                self.variables.insert(name, value);
            }
            StatementKind::Function(name, function) => {
                self.functions.insert(name, function);
            }
            StatementKind::PointSingle(name, expr) => {
                // named points can't be redefined, since lines are defined in terms of them
                if let Some(original_line) = self.points.get_point_line_number(&name) {
                    return Err(EvaluatorError::PointRedefinition(name, line, original_line).into());
                }
                let value = expr
                    .evaluate(self)
                    .and_then(Point::try_from)
                    .map_err(|err| EvaluatorError::Math(err, line))?;
                self.points.insert_point(name, value, line);
            }
            StatementKind::PointSpaced {
                from,
                spaced,
                points,
            } => {
                let spacing = spaced
                    .evaluate(self)
                    .and_then(Point::try_from)
                    .map_err(|err| EvaluatorError::Math(err, line))?;
                if !self.points.contains(&from) {
                    return Err(EvaluatorError::Math(MathError::Variable(from), line).into());
                }
                let mut total_distance = 0.0;
                let mut distances = Vec::new();
                for (multiplier, name) in points {
                    // check for redefinition of point
                    if let Some(original_line) = self.points.get_point_line_number(&name) {
                        return Err(
                            EvaluatorError::PointRedefinition(name, line, original_line).into()
                        );
                    }
                    total_distance += multiplier
                        .map(|expr| expr.evaluate(self).and_then(f64::try_from))
                        .unwrap_or(Ok(1.0))
                        .map_err(|err| EvaluatorError::Math(err, line))?;
                    distances.push((name, total_distance));
                }
                self.points.new_line(from, spacing, distances, line);
            }
            StatementKind::PointBetween {
                from,
                to: (to_multiplier, to),
                points,
            } => {
                if !self.points.contains(&from) {
                    return Err(EvaluatorError::Math(MathError::Variable(from), line).into());
                }
                if !self.points.contains(&to) {
                    return Err(EvaluatorError::Math(MathError::Variable(to), line).into());
                }
                let mut total_distance = 0.0;
                let mut distances = Vec::new();
                for (multiplier, name) in points {
                    // check for redefinition of point
                    if let Some(original_line) = self.points.get_point_line_number(&name) {
                        return Err(
                            EvaluatorError::PointRedefinition(name, line, original_line).into()
                        );
                    }
                    total_distance += multiplier
                        .map(|expr| expr.evaluate(self).and_then(f64::try_from))
                        .unwrap_or(Ok(1.0))
                        .map_err(|err| EvaluatorError::Math(err, line))?;
                    distances.push((name, total_distance));
                }
                total_distance += to_multiplier
                    .map(|expr| expr.evaluate(self).and_then(f64::try_from))
                    .unwrap_or(Ok(1.0))
                    .map_err(|err| EvaluatorError::Math(err, line))?;
                self.points.extend_line(
                    from,
                    to,
                    distances
                        .into_iter()
                        .map(|(name, distance)| (name, distance / total_distance)),
                    line,
                );
            }
            StatementKind::Route {
                name,
                style,
                segments,
            } => {
                let width = style
                    .as_ref()
                    // if style is present, get the appropriate line_sep
                    .and_then(|style| self.get_variable(&format!("line_sep.{}", style)))
                    // if line_sep.style isn't set, or if style isn't set, get the default line_sep
                    .or_else(|| self.get_variable("line_sep"))
                    // convert value to number
                    .and_then(Value::as_number)
                    // if it wasn't found, or wasn't a number, default to 1
                    .unwrap_or(1.0);
                for Segment { start, end, offset } in &segments {
                    let offset = match offset
                        .evaluate(self)
                        .map_err(|err| EvaluatorError::Math(err, line))?
                    {
                        // if offset evaluates to a number, coerce it to an integer
                        // TODO: add warning if it's not an integer
                        Value::Number(x) => x as isize,
                        // if offset evaluates to something else (i.e. a point), raise a type error
                        value => {
                            return Err(EvaluatorError::Math(
                                MathError::Type(Type::Number, value.into()),
                                line,
                            )
                            .into());
                        }
                    };
                    self.points
                        .add_segment(start, end, offset, width)
                        .map_err(|name| {
                            EvaluatorError::Math(MathError::Variable(name.into()), line)
                        })?;
                }
                self.routes.push(Route {
                    name,
                    style,
                    segments,
                });
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

#[derive(Debug)]
struct Route {
    name: Variable,
    style: Option<Variable>,
    segments: Vec<Segment>,
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::parser::LexerExt;
    use crate::values::{Point, Value};

    use super::{EvaluationContext, Evaluator};

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
        assert_eq!(evaluator.get_variable("a"), Some(Value::Point(1.0, 1.0)));
    }

    macro_rules! points_multiple {
        ($str:expr,
         $first:ident: ($first_x:expr, $first_y:expr),
         $($name:ident: ($x:expr, $y:expr)),*;
         $last:ident: ($last_x:expr, $last_y:expr)) => {
            let parser = Lexer::new($str.as_bytes()).into_parser();
            let mut evaluator = Evaluator::new();
            evaluator.evaluate_all(parser).unwrap();
            assert_eq!(
                evaluator.points.get_points_of_line(stringify!($first), stringify!($last)),
                Some(vec![
                     Point($first_x as f64, $first_y as f64),
                     $(Point($x as f64, $y as f64)),*,
                     Point($last_x as f64, $last_y as f64)
                ]),
            );
            for (name, value) in &[
                 (stringify!($first), Value::Point($first_x as f64, $first_y as f64)),
                 $((stringify!($name), Value::Point($x as f64, $y as f64))),*,
                 (stringify!($last), Value::Point($last_x as f64, $last_y as f64)),
            ] {
                assert_eq!(evaluator.get_variable(name), Some(*value));
            }
        }
    }

    #[test]
    fn points_spaced() {
        points_multiple!(
            "point a = (1, 1); points from a spaced (1, 1): (0.5) b, c, d, (0.5) e;",
            a: (1, 1),
            b: (1.5, 1.5),
            c: (2.5, 2.5),
            d: (3.5, 3.5);
            e: (4, 4)
        );
    }

    #[test]
    fn points_between() {
        points_multiple!(
            "point a = (1, 1); point e = (4, 4); points from a to (0.5) e: (0.5) b, c, d;",
            a: (1, 1),
            b: (1.5, 1.5),
            c: (2.5, 2.5),
            d: (3.5, 3.5);
            e: (4, 4)
        );
    }

    #[test]
    fn points_between_2() {
        points_multiple!(
            "point a = (1, 1);
             point e = (4, 4);
             points from a to (0.5) e: (0.5) b, c, d;
             points from a to e: f, g;",
            a: (1, 1),
            b: (1.5, 1.5),
            f: (2, 2),
            c: (2.5, 2.5),
            g: (3, 3),
            d: (3.5, 3.5);
            e: (4, 4)
        );
    }
}
