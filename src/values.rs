use std::ops;
use std::result;

use crate::error::{MathError, Type};

pub type Result = result::Result<Value, MathError>;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Value {
    Number(f64),
    Point(f64, f64),
}

impl Value {
    pub fn point(x: Value, y: Value) -> Result {
        use self::Value::*;
        match (x, y) {
            (Number(x), Number(y)) => Ok(Point(x, y)),
            (Number(_), y) => Err(MathError::Type(Type::Number, y.into())),
            (x, _) => Err(MathError::Type(Type::Number, x.into())),
        }
    }

    pub fn hypot(self, other: Value) -> Result {
        use self::Value::*;
        match (self, other) {
            (Number(x), Number(y)) => Ok(Number(x.hypot(y))),
            (Number(_), y) => Err(MathError::Type(Type::Number, y.into())),
            (x, _) => Err(MathError::Type(Type::Number, x.into())),
        }
    }

    pub fn hypot_sub(self, other: Value) -> Result {
        use self::Value::*;
        match (self, other) {
            (Number(x), Number(y)) => {
                // adopted from the algorithm for `hypot` given on [wikipedia][0]
                //
                // [0]: https://en.wikipedia.org/wiki/Hypot#Pseudocode
                let x = x.abs();
                let y = y.abs();
                if y > x {
                    Err(MathError::Domain)
                } else {
                    // x == 0.0 implies y == 0.0, since y > x
                    if y == 0.0 {
                        Ok(Number(x))
                    } else {
                        let t = y / x;
                        Ok(Number(x * (1.0 - t * t).sqrt()))
                    }
                }
            }
            (Number(_), y) => Err(MathError::Type(Type::Number, y.into())),
            (x, _) => Err(MathError::Type(Type::Number, x.into())),
        }
    }
}

impl ops::Add for Value {
    type Output = Result;

    fn add(self, rhs: Value) -> Result {
        use self::Value::*;
        Ok(match (self, rhs) {
            (Number(a), Number(b)) => Number(a + b),
            (Point(x1, y1), Point(x2, y2)) => Point(x1 + x2, y1 + y2),
            _ => Err(MathError::Type(self.into(), rhs.into()))?,
        })
    }
}

impl ops::Sub for Value {
    type Output = Result;

    fn sub(self, rhs: Value) -> Result {
        use self::Value::*;
        Ok(match (self, rhs) {
            (Number(a), Number(b)) => Number(a - b),
            (Point(x1, y1), Point(x2, y2)) => Point(x1 - x2, y1 - y2),
            _ => Err(MathError::Type(self.into(), rhs.into()))?,
        })
    }
}

impl ops::Mul for Value {
    type Output = Result;

    fn mul(self, rhs: Value) -> Result {
        use self::Value::*;
        Ok(match (self, rhs) {
            (Number(a), Number(b)) => Number(a * b),
            (Number(a), Point(x, y)) => Point(a * x, a * y),
            (Point(x, y), Number(a)) => Point(a * x, a * y),
            (Point(x1, y1), Point(x2, y2)) => Number(x1 * x2 + y1 * y2),
        })
    }
}

impl ops::Div for Value {
    type Output = Result;

    fn div(self, rhs: Value) -> Result {
        use self::Value::*;
        Ok(match (self, rhs) {
            (_, Number(x)) if x == 0.0 => Err(MathError::DivisionByZero)?,
            (Number(a), Number(b)) => Number(a / b),
            (Point(x, y), Number(a)) => Point(x / a, y / a),
            _ => Err(MathError::Type(Type::Number, rhs.into()))?,
        })
    }
}

impl ops::Neg for Value {
    type Output = Result;

    fn neg(self) -> Result {
        use self::Value::*;
        Ok(match self {
            Number(x) => Number(-x),
            Point(x, y) => Point(-x, -y),
        })
    }
}
