use std::error;
use std::ops;

type Error = Box<dyn error::Error>;
type EResult<T> = Result<T, Error>;

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
            _ => Err("Type error: tried to add a number to a point")?,
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
            (Number(_), Point(..)) => Err("Type error: tried to subtract a point from a number")?,
            (Point(..), Number(_)) => Err("Type error: tried to subtract a number from a point")?,
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
            (_, Number(x)) if x == 0.0 => Err("Division by zero error")?,
            (Number(a), Number(b)) => Number(a / b),
            (Point(x, y), Number(a)) => Point(x / a, y / a),
            (Point(..), Point(..)) => Err("Type error: tried to divide a point by a point")?,
            (Number(_), Point(..)) => Err("Type error: tried to divide a number by a point")?,
        })
    }
}
