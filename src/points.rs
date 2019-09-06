use std::collections::{HashMap, HashSet};

use itertools::Itertools;

use crate::expressions::Variable;
use crate::values::Point;

#[derive(Default)]
pub struct PointCollection {
    points: Vec<PointInfo>,
    point_ids: HashMap<Variable, PointId>,
    lines: Vec<Line>,
    pairs: HashMap<(PointId, PointId), LineId>,
}

impl PointCollection {
    pub fn new() -> PointCollection {
        Default::default()
    }

    pub fn contains(&self, k: &str) -> bool {
        self.point_ids.contains_key(k)
    }

    fn get_point_info(&self, k: &str) -> Option<&PointInfo> {
        self.point_ids.get(k).and_then(|id| self.points.get(*id))
    }

    pub fn get_point(&self, k: &str) -> Option<Point> {
        self.get_point_info(k).map(|info| info.value)
    }

    pub fn get_point_line_number(&self, k: &str) -> Option<usize> {
        self.get_point_info(k).map(|info| info.line_number)
    }

    fn insert_point_get_id(&mut self, name: Variable, value: Point, line_number: usize) -> PointId {
        let id = self.points.len();
        self.points.push(PointInfo::new(value, id, line_number));
        self.point_ids.insert(name, id);
        id
    }

    fn get_line(&self, p1: PointId, p2: PointId) -> Option<&Line> {
        self.pairs.get(&(p1, p2)).and_then(|&id| self.lines.get(id))
    }

    /// Returns a `LineId` so that `self` isn't mutably borrowed
    fn get_or_insert_line(&mut self, p1: PointId, p2: PointId) -> LineId {
        if let Some(&line_id) = self.pairs.get(&(p1, p2)) {
            line_id
        } else {
            let line_id = self.lines.len();
            self.add_pair(p1, p2, line_id);
            let p1 = &self.points[p1];
            let p2 = &self.points[p2];
            let new_line = Line {
                direction: p2.value - p1.value,
                origin: p1.value,
                points: vec![
                    LinePoint {
                        distance: 0.0,
                        id: p1.id,
                    },
                    LinePoint {
                        distance: 1.0,
                        id: p2.id,
                    },
                ],
            };
            self.lines.push(new_line);
            line_id
        }
    }

    /// Returns a vector of the points of the line, in the same order as the specified points.
    #[cfg(test)]
    pub fn get_points_of_line(&self, p1: &str, p2: &str) -> Option<Vec<Point>> {
        let p1 = self.get_point_info(p1)?;
        let p2 = self.get_point_info(p2)?;
        let line = self.get_line(p1.id, p2.id)?;
        if line.distance(p1.value) < line.distance(p2.value) {
            Some(line.points.iter().map(|p| line.point(p.distance)).collect())
        } else {
            Some(
                line.points
                    .iter()
                    .rev()
                    .map(|p| line.point(p.distance))
                    .collect(),
            )
        }
    }

    pub fn insert_point(&mut self, name: Variable, value: Point, line_number: usize) {
        self.insert_point_get_id(name, value, line_number);
    }

    fn add_pair(&mut self, p1: PointId, p2: PointId, line: LineId) {
        self.pairs.insert((p1, p2), line);
        self.pairs.insert((p2, p1), line);
        self.points[p1].lines.insert(line);
        self.points[p2].lines.insert(line);
    }

    pub fn new_line(
        &mut self,
        start_point: Variable,
        direction: Point,
        points: impl IntoIterator<Item = (Variable, f64)>,
        line_number: usize,
    ) {
        let line_id = self.lines.len();
        let start_id = *self.point_ids.get(&start_point).unwrap();
        let origin = self.points[start_id].value;
        let mut line = Line {
            origin,
            direction,
            points: Vec::new(),
        };
        line.points.push(LinePoint {
            id: start_id,
            distance: 0.0,
        });
        line.points
            .extend(points.into_iter().map(|(name, distance)| {
                let point = distance * direction + origin;
                let id = self.insert_point_get_id(name, point, line_number);
                LinePoint { id, distance }
            }));
        for (&p1, &p2) in line.points.iter().tuple_combinations() {
            self.add_pair(p1.id, p2.id, line_id);
        }
        self.lines.push(line);
    }

    /// Extends the line specified by the given points, with intermediate points indicated as a
    /// fraction of the distance between the given points.
    pub fn extend_line(
        &mut self,
        start_point: Variable,
        end_point: Variable,
        points: impl IntoIterator<Item = (Variable, f64)>,
        line_number: usize,
    ) {
        let start_id = self.point_ids[&start_point];
        let end_id = self.point_ids[&end_point];
        let line_id = self.get_or_insert_line(start_id, end_id);
        let start = self.points[start_id].value;
        let end = self.points[end_id].value;
        let direction = end - start;
        let (start_distance, distance_scale) = {
            let line = &self.lines[line_id];
            (
                line.distance(start),
                line.distance(end) - line.distance(start),
            )
        };
        dbg!(start_distance, distance_scale);
        let mut new_points = Vec::new();
        for (name, distance) in points {
            let id = self.insert_point_get_id(name, distance * direction + start, line_number);
            new_points.push(LinePoint {
                id,
                distance: distance * distance_scale + start_distance,
            });
        }
        let points = self.lines[line_id]
            .points
            .iter()
            .merge(new_points.iter())
            .copied()
            .collect::<Vec<_>>();
        for (&p1, &p2) in points.iter().tuple_combinations() {
            self.add_pair(p1.id, p2.id, line_id);
        }
        self.lines[line_id].points = points;
    }
}

#[derive(Clone, Debug)]
struct PointInfo {
    value: Point,
    id: PointId,
    lines: HashSet<LineId>,
    line_number: usize,
}

impl PointInfo {
    pub fn new(value: Point, id: PointId, line_number: usize) -> PointInfo {
        PointInfo {
            value,
            id,
            line_number,
            lines: HashSet::new(),
        }
    }
}

type PointId = usize;

#[derive(Clone, Debug)]
struct Line {
    direction: Point,
    origin: Point,
    points: Vec<LinePoint>,
}

impl Line {
    /// Calculate the distance along the line for the specified point.
    pub fn distance(&self, p: Point) -> f64 {
        self.relative_distance(self.origin, p)
    }

    /// Calculate the distance between the two points along the line.
    pub fn relative_distance(&self, p1: Point, p2: Point) -> f64 {
        (p2 - p1) * self.direction / self.direction.norm2()
    }

    /// Calculate the location of a point a given distance along the line.
    pub fn point(&self, distance: f64) -> Point {
        distance * self.direction + self.origin
    }
}

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
struct LinePoint {
    distance: f64,
    id: PointId,
}

type LineId = usize;
