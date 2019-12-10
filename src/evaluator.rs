use std::collections::HashMap;
use std::convert::TryFrom;
use std::io::{Result as IoResult, Write};

use crate::error::{EvaluatorError, MathError, Result as EResult};
use crate::expressions::{Function, Variable};
use crate::points::PointCollection;
use crate::statement::{Statement, StatementKind};
use crate::values::{Point, Value};

pub trait EvaluationContext {
    fn get_variable(&self, name: &str) -> Option<Value>;

    fn get_function(&self, name: &str) -> Option<&Function>;
}

#[derive(Default, Debug)]
pub struct Evaluator {
    variables: HashMap<Variable, Value>,
    functions: HashMap<Variable, Function>,
    points: PointCollection,
}

impl Evaluator {
    pub fn new() -> Evaluator {
        Default::default()
    }

    pub fn evaluate_all(
        &mut self,
        parser: impl Iterator<Item = EResult<Statement>>,
    ) -> EResult<()> {
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
                self.points.insert_point(name, value, line)?;
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
                let points = points
                    .into_iter()
                    .map(|(multiplier, name)| {
                        multiplier
                            .map(|expr| expr.evaluate(self).and_then(f64::try_from))
                            .unwrap_or(Ok(1.0))
                            .map_err(|err| EvaluatorError::Math(err, line))
                            .map(|distance| (name, distance))
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                self.points.new_line(from, spacing, points, line)?;
            }
            StatementKind::PointBetween {
                from,
                to: (to_multiplier, to),
                points,
            } => {
                let mut total_distance = 0.0;
                let points = points
                    .into_iter()
                    .map(|(multiplier, name)| {
                        total_distance += multiplier
                            .map(|expr| expr.evaluate(self).and_then(f64::try_from))
                            .unwrap_or(Ok(1.0))
                            .map_err(|err| EvaluatorError::Math(err, line))?;
                        Ok((name, total_distance))
                    })
                    .collect::<Result<Vec<_>, EvaluatorError>>()?;
                total_distance += to_multiplier
                    .map(|expr| expr.evaluate(self).and_then(f64::try_from))
                    .unwrap_or(Ok(1.0))
                    .map_err(|err| EvaluatorError::Math(err, line))?;
                self.points.extend_line(
                    from,
                    to,
                    points
                        .into_iter()
                        .map(|(name, distance)| (name, distance / total_distance)),
                    line,
                )?;
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
                let route = self.points.insert_route_get_id(name, width, style, line)?;
                for segment in segments {
                    // if offset evaluates to a number, coerce it to an integer
                    // TODO: add warning if it's not an integer
                    let offset = segment
                        .offset
                        .evaluate(self)
                        .and_then(f64::try_from)
                        .map_err(|err| EvaluatorError::Math(err, line))?
                        as isize;
                    self.points
                        .add_segment(route, &segment.start, &segment.end, offset)
                        .map_err(|name| {
                            EvaluatorError::Math(MathError::Variable(name.into()), line)
                        })?;
                }
            }
            StatementKind::Stop(stop) => {
                self.points.add_stop(stop, line)?;
            }
        }
        Ok(())
    }

    pub fn draw_routes(&self, f: &mut impl Write) -> IoResult<()> {
        let line_sep = self
            .variables
            .get("line_sep")
            .copied()
            .and_then(Value::as_number)
            .unwrap_or(1.0);
        let inner_radius = self
            .variables
            .get("inner_radius")
            .copied()
            .and_then(Value::as_number)
            .unwrap_or(0.0);
        self.points.draw_routes(line_sep, inner_radius, f)
    }

    pub fn draw_stops(&self, f: &mut impl Write) -> IoResult<()> {
        let line_sep = self
            .variables
            .get("line_sep")
            .copied()
            .and_then(Value::as_number)
            .unwrap_or(1.0);
        self.points.draw_stops(line_sep, f)
    }

    pub fn draw_points(&self, f: &mut impl Write) -> IoResult<()> {
        for p in self.points.point_iter() {
            writeln!(f, "<circle cx=\"{:.4}\" cy=\"{:.4}\" r=\"1\" />", p.0, p.1)?;
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
