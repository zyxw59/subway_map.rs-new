use std::borrow::Borrow;
use std::collections::HashMap;
use std::hash::Hash;

use crate::expressions::Variable;
use crate::values::Point;

#[derive(Default)]
pub struct PointCollection {
    points: HashMap<Variable, PointInfo>,
}

impl PointCollection {
    pub fn new() -> PointCollection {
        Default::default()
    }

    pub fn contains(&self, k: &str) -> bool {
        self.points.contains_key(k)
    }

    pub fn get_point(&self, k: &str) -> Option<Point> {
        self.points.get(k).map(|info| info.value)
    }

    pub fn get_point_line_number(&self, k: &str) -> Option<usize> {
        self.points.get(k).map(|info| info.line_number)
    }

    pub fn insert_point(&mut self, name: Variable, value: Point, line_number: usize) {
        self.points.insert(name, PointInfo { value, line_number });
    }
}

struct PointInfo {
    value: Point,
    line_number: usize,
}
