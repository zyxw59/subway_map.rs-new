use std::ops;
use std::result;

use error::{MathError, Type};

pub type Result = result::Result<Expression, MathError>;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Expression {
    Number(f64),
    Point(f64, f64),
}

impl ops::Add for Expression {
    type Output = Result;

    fn add(self, rhs: Expression) -> Result {
        use self::Expression::*;
        Ok(match (self, rhs) {
            (Number(a), Number(b)) => Number(a + b),
            (Point(x1, y1), Point(x2, y2)) => Point(x1 + x2, y1 + y2),
            _ => Err(MathError::Type(self.into(), rhs.into()))?,
        })
    }
}

impl ops::Sub for Expression {
    type Output = Result;

    fn sub(self, rhs: Expression) -> Result {
        use self::Expression::*;
        Ok(match (self, rhs) {
            (Number(a), Number(b)) => Number(a - b),
            (Point(x1, y1), Point(x2, y2)) => Point(x1 - x2, y1 - y2),
            _ => Err(MathError::Type(self.into(), rhs.into()))?,
        })
    }
}

impl ops::Mul for Expression {
    type Output = Result;

    fn mul(self, rhs: Expression) -> Result {
        use self::Expression::*;
        Ok(match (self, rhs) {
            (Number(a), Number(b)) => Number(a * b),
            (Number(a), Point(x, y)) => Point(a * x, a * y),
            (Point(x, y), Number(a)) => Point(a * x, a * y),
            (Point(x1, y1), Point(x2, y2)) => Number(x1 * x2 + y1 * y2),
        })
    }
}

impl ops::Div for Expression {
    type Output = Result;

    fn div(self, rhs: Expression) -> Result {
        use self::Expression::*;
        Ok(match (self, rhs) {
            (_, Number(x)) if x == 0.0 => Err(MathError::DivisionByZero)?,
            (Number(a), Number(b)) => Number(a / b),
            (Point(x, y), Number(a)) => Point(x / a, y / a),
            _ => Err(MathError::Type(Type::Number, rhs.into()))?,
        })
    }
}

impl ops::Neg for Expression {
    type Output = Result;

    fn neg(self) -> Result {
        use self::Expression::*;
        Ok(match self {
            Number(x) => Number(-x),
            Point(x, y) => Point(-x, -y),
        })
    }
}
