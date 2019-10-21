use std::fmt;

use crate::values::Point;

/// The information required to draw a rounded corner.
pub struct Corner {
    /// The vertex of the corner (as if it weren't rounded.
    pub corner: Point,
    /// The radius of the corner.
    pub radius: f64,
    /// Whether the arc runs clockwise (`true`) or counterclockwise (`false`).
    pub sweep: bool,
    /// The start point of the arc.
    pub start: Point,
    /// The end point of the arc.
    pub end: Point,
}

impl Corner {
    /// Constructs a new `Corner` with the given parameters.
    pub fn new(from: Point, corner: Point, to: Point, radius: f64) -> Corner {
        let in_dir = (from - corner).unit();
        let out_dir = (to - corner).unit();
        let dot = in_dir * out_dir;
        let corner_distance = ((1.0 + dot) / (1.0 - dot)).sqrt() * radius;
        Corner {
            corner,
            radius,
            // positive cross product => clockwise; negative cross product => counterclockwise
            sweep: in_dir.cross(out_dir) < 0.0,
            start: in_dir.mul_add(corner_distance, corner),
            end: out_dir.mul_add(corner_distance, corner),
        }
    }

    /// Offsets the values of a `Corner` by the specified parallel distances.
    pub fn offset(self, offset_in: f64, offset_out: f64) -> Corner {
        let in_dir = (self.start - self.corner).unit();
        let out_dir = (self.end - self.corner).unit();
        let offset = in_dir.basis(
            -offset_in.mul_add(in_dir * out_dir, offset_out) / in_dir.cross(out_dir),
            offset_in,
        );
        Corner {
            corner: self.corner + offset,
            start: self.start + offset,
            end: self.end + offset,
            ..self
        }
    }

    /// Constructs a `Corner` representing a 180-degree turn around the corner.
    ///
    /// Because the line is traversed in reverse on the way out, `offset_out` is oriented in the
    /// opposite direction as `offset_in`.
    pub fn u_turn(from: Point, corner: Point, offset_in: f64, offset_out: f64) -> Corner {
        let perp = (from - corner).unit().perp();
        Corner {
            corner,
            radius: (offset_in + offset_out).abs() / 2.0,
            sweep: offset_in < -offset_out,
            start: perp.mul_add(offset_in, corner),
            end: perp.mul_add(-offset_out, corner),
        }
    }
}

impl fmt::Display for Corner {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            " L {start} A {radius:.4},{radius:.4} 0 0 {sweep} {end}",
            start = self.start,
            radius = self.radius,
            sweep = self.sweep as u8,
            end = self.end,
        )
    }
}

/// The information required to draw a shift between two offsets on the same line as a cubic Bézier
/// curve.
pub struct ParallelShift(pub Point, pub Point, pub Point, pub Point);

impl ParallelShift {
    pub fn new(offset_in: f64, offset_out: f64, dir: Point, at: Point) -> ParallelShift {
        let dir = dir.unit();
        let delta = (offset_out - offset_in).abs();
        ParallelShift(
            at + dir.basis(-delta, -offset_in),
            at + dir.basis(0.0, -offset_in),
            at + dir.basis(0.0, -offset_out),
            at + dir.basis(delta, -offset_out),
        )
    }
}

impl fmt::Display for ParallelShift {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, " L {} C {} {} {}", self.0, self.1, self.2, self.3)
    }
}
