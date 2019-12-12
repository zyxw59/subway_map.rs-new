use std::convert::TryFrom;
use std::fmt;
use std::ops;
use std::result;

use svg::node::element::path::Parameters;

use crate::error::{MathError, Type};

pub type Result = result::Result<Value, MathError>;

macro_rules! numeric_fn {
    (($x:ident, $y:ident) => $val:expr) => {
        numeric_fn!(($x, $y) as ($x, $y) => $val)
    };
    ($x:ident => $val:expr) => {
        numeric_fn!(($x) as $x => $val)
    };
    (($ex:expr, $ey:expr) as ($x:ident, $y:ident) => $val:expr) => {
        match ($ex, $ey) {
            (Value::Number($x), Value::Number($y)) => $val,
            (Value::Number(_), y) => Err(MathError::Type(Type::Number, y.into())),
            (x, _) => Err(MathError::Type(Type::Number, x.into())),
        }
    };
    (($ex:expr) as $x:ident => $val:expr) => {
        match $ex {
            Value::Number($x) => $val,
            x => Err(MathError::Type(Type::Number, x.into())),
        }
    };
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Point(pub f64, pub f64);

impl Point {
    pub fn norm(self) -> f64 {
        self.0.hypot(self.1)
    }

    pub fn norm2(self) -> f64 {
        self * self
    }

    /// Returns a unit vector in the direction of the point.
    pub fn unit(self) -> Point {
        self / self.norm()
    }

    /// Rotates `self` 90 degrees clockwise.
    pub fn perp(self) -> Point {
        Point(self.1, -self.0)
    }

    /// Positive if `other` is clockwise of `self`.
    pub fn cross(self, other: Point) -> f64 {
        self.0 * other.1 - self.1 * other.0
    }

    /// Fused multiply-add. Computes `(self * a) + b with only one rounding error, yielding a more
    /// accurate result than an unfused multiply-add.
    pub fn mul_add(self, a: f64, b: Point) -> Point {
        Point(self.0.mul_add(a, b.0), self.1.mul_add(a, b.1))
    }

    /// Constructs a new point equal to `a * self + b * self.perp()`.
    pub fn basis(self, a: f64, b: f64) -> Point {
        self.mul_add(a, b * self.perp())
    }
}

impl fmt::Display for Point {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:.4},{:.4}", self.0, self.1)
    }
}

impl ops::Add for Point {
    type Output = Point;

    fn add(self, other: Point) -> Point {
        Point(self.0 + other.0, self.1 + other.1)
    }
}

impl ops::Sub for Point {
    type Output = Point;

    fn sub(self, other: Point) -> Point {
        Point(self.0 - other.0, self.1 - other.1)
    }
}

impl ops::Neg for Point {
    type Output = Point;

    fn neg(self) -> Point {
        Point(-self.0, -self.1)
    }
}

impl ops::Mul for Point {
    type Output = f64;

    fn mul(self, other: Point) -> f64 {
        self.0 * other.0 + self.1 * other.1
    }
}

impl ops::Mul<f64> for Point {
    type Output = Point;

    fn mul(self, other: f64) -> Point {
        Point(self.0 * other, self.1 * other)
    }
}

impl ops::Mul<Point> for f64 {
    type Output = Point;

    fn mul(self, other: Point) -> Point {
        Point(self * other.0, self * other.1)
    }
}

impl ops::Div<f64> for Point {
    type Output = Point;

    fn div(self, other: f64) -> Point {
        Point(self.0 / other, self.1 / other)
    }
}

impl TryFrom<Value> for Point {
    type Error = MathError;

    fn try_from(value: Value) -> result::Result<Point, MathError> {
        match value {
            Value::Point(x, y) => Ok(Point(x, y)),
            _ => Err(MathError::Type(Type::Point, value.into())),
        }
    }
}

impl TryFrom<Value> for f64 {
    type Error = MathError;

    fn try_from(value: Value) -> result::Result<f64, MathError> {
        match value {
            Value::Number(x) => Ok(x),
            _ => Err(MathError::Type(Type::Number, value.into())),
        }
    }
}

impl From<Point> for Value {
    fn from(point: Point) -> Value {
        Value::Point(point.0, point.1)
    }
}

impl From<Point> for Parameters {
    fn from(point: Point) -> Parameters {
        Parameters::from((point.0, point.1))
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Value {
    Number(f64),
    Point(f64, f64),
    Line(Point, Point),
}

impl Value {
    pub fn as_number(self) -> Option<f64> {
        match self {
            Value::Number(x) => Some(x),
            _ => None,
        }
    }

    pub fn point(x: Value, y: Value) -> Result {
        numeric_fn!((x, y) => Ok(Value::Point(x, y)))
    }

    pub fn hypot(self, other: Value) -> Result {
        numeric_fn!((self, other) as (x, y) => Ok(Value::Number(x.hypot(y))))
    }

    pub fn hypot_sub(self, other: Value) -> Result {
        numeric_fn!((self, other) as (x, y) => {
            let x = x.abs();
            let y = y.abs();
            if y > x {
                Err(MathError::Domain)
            } else {
                Ok(Value::Number((x*x - y*y).sqrt()))
            }
        })
    }

    pub fn pow(self, other: Value) -> Result {
        numeric_fn!((self, other) as (x, y) => Ok(Value::Number(x.powf(y))))
    }

    /// Cosine of the number in degrees
    pub fn cos(self) -> Result {
        numeric_fn!((self) as x => Ok(Value::Number(cos_deg(x))))
    }

    /// Sine of the number in degrees
    pub fn sin(self) -> Result {
        numeric_fn!((self) as x => Ok(Value::Number(sin_deg(x))))
    }

    /// Unit vector in the given direction in degrees, with 0 being up the page, and increasing
    /// clockwise.
    pub fn dir(self) -> Result {
        numeric_fn!((self) as x => Ok(Value::Point(sin_deg(x), -cos_deg(x))))
    }

    /// Angle of given vector in degrees
    pub fn angle(self) -> Result {
        use self::Value::*;
        match self {
            Point(x, y) => Ok(Number(y.atan2(x).to_degrees())),
            x => Err(MathError::Type(Type::Point, x.into())),
        }
    }

    /// x value of the given point
    pub fn xpart(self) -> Result {
        use self::Value::*;
        match self {
            Point(x, _) => Ok(Number(x)),
            x => Err(MathError::Type(Type::Point, x.into())),
        }
    }

    /// y value of the given point
    pub fn ypart(self) -> Result {
        use self::Value::*;
        match self {
            Point(_, y) => Ok(Number(y)),
            x => Err(MathError::Type(Type::Point, x.into())),
        }
    }

    /// Line between two points
    pub fn line_between(self, rhs: Value) -> Result {
        match (self, rhs) {
            (Value::Point(x1, y1), Value::Point(x2, y2)) => {
                Ok(Value::Line(Point(x1, y1), Point(x2 - x1, y2 - y1)))
            }
            _ => Err(MathError::Type(Type::Point, self.into())),
        }
    }

    /// Line from point and vector
    pub fn line_vector(self, rhs: Value) -> Result {
        match (self, rhs) {
            (Value::Point(x1, y1), Value::Point(x2, y2)) => {
                Ok(Value::Line(Point(x1, y1), Point(x2, y2)))
            }
            _ => Err(MathError::Type(Type::Point, self.into())),
        }
    }

    pub fn intersect(self, rhs: Value) -> Result {
        use self::Value::*;
        match (self, rhs) {
            (Line(p1, d1), Line(p2, d2)) => {
                Ok((d1.mul_add((p2 - p1).cross(d2) / d1.cross(d2), p1)).into())
            }
            _ => Err(MathError::Type(Type::Line, self.into())),
        }
    }

    pub fn eq(self, other: Value) -> Result {
        use self::Value::*;
        match (self, other) {
            (Number(x), Number(y)) => Ok(Value::from(float_eq(x, y))),
            (Point(x1, y1), Point(x2, y2)) => Ok(Value::from(float_eq(x1, x2) && float_eq(y1, y2))),
            _ => Err(MathError::Type(self.into(), other.into())),
        }
    }

    pub fn ne(self, other: Value) -> Result {
        use self::Value::*;
        match (self, other) {
            (Number(x), Number(y)) => Ok(Value::from(!float_eq(x, y))),
            (Point(x1, y1), Point(x2, y2)) => {
                Ok(Value::from(!float_eq(x1, x2) || !float_eq(y1, y2)))
            }
            _ => Err(MathError::Type(self.into(), other.into())),
        }
    }

    pub fn lt(self, other: Value) -> Result {
        use self::Value::*;
        match (self, other) {
            (Number(x), Number(y)) => Ok(Value::from(x < y)),
            _ => Err(MathError::Type(Type::Number, self.into())),
        }
    }

    pub fn le(self, other: Value) -> Result {
        use self::Value::*;
        match (self, other) {
            (Number(x), Number(y)) => Ok(Value::from(x <= y)),
            _ => Err(MathError::Type(Type::Number, self.into())),
        }
    }

    pub fn gt(self, other: Value) -> Result {
        use self::Value::*;
        match (self, other) {
            (Number(x), Number(y)) => Ok(Value::from(x > y)),
            _ => Err(MathError::Type(Type::Number, self.into())),
        }
    }

    pub fn ge(self, other: Value) -> Result {
        use self::Value::*;
        match (self, other) {
            (Number(x), Number(y)) => Ok(Value::from(x >= y)),
            _ => Err(MathError::Type(Type::Number, self.into())),
        }
    }

    pub fn max(self, other: Value) -> Result {
        use self::Value::*;
        match (self, other) {
            (Number(x), Number(y)) => Ok(Number(x.max(y))),
            (Point(x1, y1), Point(x2, y2)) => Ok(Point(x1.max(x2), y1.max(y2))),
            (Line(..), _) => Err(MathError::Type(Type::Number, Type::Line)),
            (_, _) => Err(MathError::Type(self.into(), other.into())),
        }
    }

    pub fn min(self, other: Value) -> Result {
        use self::Value::*;
        match (self, other) {
            (Number(x), Number(y)) => Ok(Number(x.min(y))),
            (Point(x1, y1), Point(x2, y2)) => Ok(Point(x1.min(x2), y1.min(y2))),
            (Line(..), _) => Err(MathError::Type(Type::Number, Type::Line)),
            (_, _) => Err(MathError::Type(self.into(), other.into())),
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
            _ => return Err(MathError::Type(self.into(), rhs.into())),
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
            _ => return Err(MathError::Type(self.into(), rhs.into())),
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
            _ => return Err(MathError::Type(self.into(), rhs.into())),
        })
    }
}

impl ops::Div for Value {
    type Output = Result;

    fn div(self, rhs: Value) -> Result {
        use self::Value::*;
        Ok(match (self, rhs) {
            (_, Number(x)) if x == 0.0 => return Err(MathError::DivisionByZero),
            (Number(a), Number(b)) => Number(a / b),
            (Point(x, y), Number(a)) => Point(x / a, y / a),
            _ => return Err(MathError::Type(Type::Number, rhs.into())),
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
            _ => return Err(MathError::Type(Type::Number, self.into())),
        })
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Value {
        Value::Number(f64::from(b as u8))
    }
}

fn cos_deg(x: f64) -> f64 {
    // since cos is even, we can take |x|, which guarantees that |x| % 360 is nonnegative
    let x = x.abs() % 360.0;
    if float_eq(x, 0.0) {
        1.0
    } else if float_eq(x, 180.0) {
        -1.0
    } else if float_eq(x, 90.0) || float_eq(x, 270.0) {
        0.0
    } else {
        x.to_radians().cos()
    }
}

fn sin_deg(x: f64) -> f64 {
    let x = x % 360.0;
    if float_eq(x, 0.0) || float_eq(x, 180.0) || float_eq(x, -180.0) {
        0.0
    } else if float_eq(x, 90.0) || float_eq(x, -270.0) {
        1.0
    } else if float_eq(x, -90.0) || float_eq(x, 270.0) {
        -1.0
    } else {
        x.to_radians().sin()
    }
}

pub fn float_eq(x: f64, y: f64) -> bool {
    (x - y).abs() < ::std::f64::EPSILON
}

#[cfg(test)]
mod tests {
    macro_rules! assert_eval {
        (($($expr:tt)+), ($($val:expr),*)) => {{
            let expr = expression!($($expr)+);
            assert_eq!(expr.evaluate(&()).unwrap(), value!($($val),*));
        }};
        (($($expr:tt)+), $val:expr) => {{
            let expr = expression!($($expr)+);
            assert_eq!(expr.evaluate(&()).unwrap(), value!($val));
        }};
    }

    #[test]
    fn basic_arithmetic() {
        // 1 + 2 * 3 + 4 == 11
        assert_eval!(("+", ("+", 1, ("*", 2, 3)), 4), 11)
    }

    #[test]
    fn basic_arithmetic_2() {
        // 1 - 2 * 3 + 4 == -1
        assert_eval!(("+", ("-", 1, ("*", 2, 3)), 4), -1);
    }

    #[test]
    fn basic_arithmetic_3() {
        // 1 - 3 / 2 * 5 == -6.5
        assert_eval!(("-", 1, ("*", ("/", 3, 2), 5)), -6.5);
    }

    #[test]
    fn hypot() {
        // 3 ++ 4 == 5
        assert_eval!(("++", 3, 4), 5);
    }

    #[test]
    fn hypot_sub() {
        // 5 +-+ 3 == 4
        assert_eval!(("+-+", 5, 3), 4);
    }

    #[test]
    fn pow() {
        // 3 ^ 4 == 81
        assert_eval!(("^", 3, 4), 81);
    }

    #[test]
    fn points() {
        // (1, 2) + (3, 4) == (4, 6)
        assert_eval!(("+", (@1, 2), (@3, 4)), (4, 6));
    }

    #[test]
    fn dot_product() {
        // (1, 2) * (3, 4) == 11
        assert_eval!(("*", (@1, 2), (@3, 4)), 11);
    }

    #[test]
    fn scalar_product() {
        // 3 * (1, 2) == (3, 6)
        assert_eval!(("*", 3, (@1, 2)), (3, 6));
    }

    #[test]
    fn angle() {
        // angle (3, 3) == 45
        assert_eval!(("angle", (@3, 3)), 45);
    }

    #[test]
    fn unary_minus() {
        // 3 * -2 == -6
        assert_eval!(("*", 3, ("-", 2)), -6);
    }

    #[test]
    fn unary_minus_2() {
        // -2 * 3 == -6
        assert_eval!(("*", ("-", 2), 3), -6);
    }

    #[test]
    fn unary_minus_3() {
        // -(1, 2) * (3, 4) == -11
        assert_eval!(("-", ("*", (@1, 2), (@3, 4))), -11);
    }

    #[test]
    fn unary_cos() {
        // cos 90 == 0
        assert_eval!(("cos", 90), 0);
    }

    #[test]
    fn unary_sin() {
        // sin 90 == 1
        assert_eval!(("sin", 90), 1);
    }

    #[test]
    fn intersect() {
        assert_eval!(("&", ("<>", (@1, 2), (@3, 4)), ("<>", (@1, 4), (@3, 2))), (2, 3))
    }
}
