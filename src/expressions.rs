use std::ops;

use error::{MathError, Result as EResult};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Expression {
    Number(f64),
    Point(f64, f64),
}

impl ops::Add for Expression {
    type Output = EResult<Expression>;

    fn add(self, rhs: Expression) -> EResult<Expression> {
        use self::Expression::*;
        Ok(match (self, rhs) {
            (Number(a), Number(b)) => Number(a + b),
            (Point(x1, y1), Point(x2, y2)) => Point(x1 + x2, y1 + y2),
            _ => Err(MathError::Type)?,
        })
    }
}

impl ops::Sub for Expression {
    type Output = EResult<Expression>;

    fn sub(self, rhs: Expression) -> EResult<Expression> {
        use self::Expression::*;
        Ok(match (self, rhs) {
            (Number(a), Number(b)) => Number(a - b),
            (Point(x1, y1), Point(x2, y2)) => Point(x1 - x2, y1 - y2),
            _ => Err(MathError::Type)?,
        })
    }
}

impl ops::Mul for Expression {
    type Output = EResult<Expression>;

    fn mul(self, rhs: Expression) -> EResult<Expression> {
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
    type Output = EResult<Expression>;

    fn div(self, rhs: Expression) -> EResult<Expression> {
        use self::Expression::*;
        Ok(match (self, rhs) {
            (_, Number(x)) if x == 0.0 => Err(MathError::DivisionByZero)?,
            (Number(a), Number(b)) => Number(a / b),
            (Point(x, y), Number(a)) => Point(x / a, y / a),
            _ => Err(MathError::Type)?,
        })
    }
}

impl ops::Neg for Expression {
    type Output = EResult<Expression>;

    fn neg(self) -> EResult<Expression> {
        use self::Expression::*;
        Ok(match self {
            Number(x) => Number(-x),
            Point(x, y) => Point(-x, -y),
        })
    }
}
