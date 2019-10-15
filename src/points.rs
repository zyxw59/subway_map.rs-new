use std::cmp::Ordering;
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
        self.get_point_info(k).map(|info| info.info.value)
    }

    pub fn get_point_line_number(&self, k: &str) -> Option<usize> {
        self.get_point_info(k).map(|info| info.info.line_number)
    }

    /// This is an associated function so that it can work with partial borrows of `self`
    fn insert_point_get_id(
        points: &mut Vec<PointInfo>,
        point_ids: &mut HashMap<Variable, PointId>,
        name: Variable,
        value: Point,
        line_number: usize,
    ) -> PointId {
        let id = points.len();
        points.push(PointInfo::new(value, id, line_number));
        point_ids.insert(name, id);
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
            let new_line = Line::from_pair(p1.info, p2.info);
            self.lines.push(new_line);
            line_id
        }
    }

    /// Returns a vector of the points of the line, in the same order as the specified points.
    #[cfg(test)]
    pub fn get_points_of_line(&self, p1: &str, p2: &str) -> Option<Vec<Point>> {
        let p1 = self.get_point_info(p1)?.info;
        let p2 = self.get_point_info(p2)?.info;
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
        Self::insert_point_get_id(
            &mut self.points,
            &mut self.point_ids,
            name,
            value,
            line_number,
        );
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
        let origin = self.points[start_id].info;
        let mut line = Line::from_origin_direction(origin, direction);
        line.points
            .extend(points.into_iter().map(|(name, distance)| {
                let point = distance * direction + origin.value;
                let id = Self::insert_point_get_id(
                    &mut self.points,
                    &mut self.point_ids,
                    name,
                    point,
                    line_number,
                );
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
        let start = self.points[start_id].info.value;
        let end = self.points[end_id].info.value;
        let direction = end - start;
        let (start_distance, distance_scale) = {
            let line = &self.lines[line_id];
            (line.distance(start), line.relative_distance(start, end))
        };
        let self_points = &mut self.points;
        let self_point_ids = &mut self.point_ids;
        let new_points = points
            .into_iter()
            .map(|(name, distance)| {
                let id = Self::insert_point_get_id(
                    self_points,
                    self_point_ids,
                    name,
                    distance * direction + start,
                    line_number,
                );
                LinePoint {
                    id,
                    distance: distance * distance_scale + start_distance,
                }
            })
            .merge(self.lines[line_id].points.iter().copied())
            .collect::<Vec<_>>();
        for (&p1, &p2) in new_points.iter().tuple_combinations() {
            self.add_pair(p1.id, p2.id, line_id);
        }
        self.lines[line_id].points = new_points;
    }

    /// Registers a given segment with the relevant line.
    pub fn add_segment<'a>(
        &mut self,
        start_point: &'a str,
        end_point: &'a str,
        offset: isize,
        width: f64,
    ) -> Result<(), &'a str> {
        let p1 = self.get_point_info(start_point).ok_or(start_point)?.info;
        let p2 = self.get_point_info(end_point).ok_or(end_point)?.info;
        let line_id = self.get_or_insert_line(p1.id, p2.id);
        self.lines[line_id].add_segment(p1, p2, offset, width);
        Ok(())
    }
}

#[derive(Clone, Debug)]
struct PointInfo {
    info: PointInfoLite,
    lines: HashSet<LineId>,
}

#[derive(Clone, Copy, Debug)]
struct PointInfoLite {
    value: Point,
    id: PointId,
    line_number: usize,
}

impl PointInfo {
    pub fn new(value: Point, id: PointId, line_number: usize) -> PointInfo {
        PointInfo {
            info: PointInfoLite {
                value,
                id,
                line_number,
            },
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
    segments: Vec<Segment>,
}

impl Line {
    /// Create a new line between the two points.
    pub fn from_pair(p1: PointInfoLite, p2: PointInfoLite) -> Line {
        Line {
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
            segments: Vec::new(),
        }
    }

    /// Create a new line with the given origin and direction.
    pub fn from_origin_direction(origin: PointInfoLite, direction: Point) -> Line {
        Line {
            origin: origin.value,
            direction,
            points: vec![LinePoint {
                distance: 0.0,
                id: origin.id,
            }],
            segments: Vec::new(),
        }
    }

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

    /// Returns a `LinePoint` corresponding to the given `PointInfoLite`.
    fn line_point(&self, point: PointInfoLite) -> LinePoint {
        LinePoint {
            distance: self.distance(point.value),
            id: point.id,
        }
    }

    /// Registers the given segment with the line.
    pub fn add_segment(
        &mut self,
        p1: PointInfoLite,
        p2: PointInfoLite,
        mut offset: isize,
        width: f64,
    ) {
        let mut p1 = self.line_point(p1);
        let mut p2 = self.line_point(p2);
        if p1 > p2 {
            // switch the order
            std::mem::swap(&mut p1, &mut p2);
            offset = -offset;
        }
        // make immutable again
        let (p1, p2, offset) = (p1, p2, offset);

        let start_seg = self
            .segments
            .binary_search_by(|seg| seg.partial_cmp(&p1).unwrap());
        let end_seg = self
            .segments
            .binary_search_by(|seg| seg.partial_cmp(&p2).unwrap());
        let start = start_seg.unwrap_or_else(|err| err);
        let end = end_seg.unwrap_or_else(|err| err);
        let pre_split = match start_seg {
            Ok(start) => self.segments[start].split(p1),
            Err(start) => {
                let end_point = if let Some(next_seg) = self.segments.get(start + 1) {
                    next_seg.start.min(p2)
                } else {
                    p2
                };
                Some(Segment::new(p1, end_point, offset, width))
            }
        };
        let post_split = match end_seg {
            Ok(end) => self.segments[end]
                .split(p2)
                .map(|seg| seg.update_new(offset, width)),
            Err(end) => {
                let start_point = if let Some(prev_seg) = self.segments.get(end - 1) {
                    prev_seg.start.max(p1)
                } else {
                    p1
                };
                Some(Segment::new(start_point, p2, offset, width))
            }
        };
        // update the intermediate segments
        for seg in &mut self.segments[start..end] {
            seg.update(offset, width);
        }
        // insert the new segments if necessary
        if let Some(post_split) = post_split {
            self.segments.insert(end + 1, post_split);
        }
        if let Some(pre_split) = pre_split {
            self.segments.insert(start, pre_split);
        }
    }
}

#[derive(Clone, Debug)]
struct Segment {
    start: LinePoint,
    end: LinePoint,
    pos_offsets: Vec<Option<f64>>,
    neg_offsets: Vec<Option<f64>>,
}

impl Segment {
    pub fn new(start: LinePoint, end: LinePoint, offset: isize, width: f64) -> Segment {
        Segment {
            start,
            end,
            pos_offsets: Vec::new(),
            neg_offsets: Vec::new(),
        }
        .update_new(offset, width)
    }

    /// Split `self`, leaving the post-split segment in `self`'s place, returning the segment to
    /// be inserted before `self`. If the split point is at one of the end points of `self` return
    /// `None`, as no new segment needs to be inserted.
    fn split(&mut self, point: LinePoint) -> Option<Segment> {
        if point == self.start || point == self.end {
            None
        } else {
            // the split is in the middle
            let mut pre_split = self.clone();
            pre_split.end = point;
            self.start = point;
            Some(pre_split)
        }
    }

    /// Update the segment with the given `offset` and `width`, returning the new segment
    fn update_new(mut self, offset: isize, width: f64) -> Segment {
        self.update(offset, width);
        self
    }

    /// Update the segment with the given `offset` and `width`.
    fn update(&mut self, offset: isize, width: f64) {
        if offset >= 0 {
            self.pos_offsets
                .resize_with((offset as usize) + 1, Default::default);
            self.pos_offsets[offset as usize] =
                Some(self.pos_offsets[offset as usize].map_or(width, |old| old.max(width)));
        } else {
            let offset = !offset;
            self.neg_offsets
                .resize_with((offset as usize) + 1, Default::default);
            self.neg_offsets[offset as usize] =
                Some(self.neg_offsets[offset as usize].map_or(width, |old| old.max(width)));
        }
    }
}

impl PartialEq<LinePoint> for Segment {
    fn eq(&self, other: &LinePoint) -> bool {
        *other >= self.start && *other <= self.end
    }
}

impl PartialOrd<LinePoint> for Segment {
    fn partial_cmp(&self, other: &LinePoint) -> Option<Ordering> {
        if *other < self.start {
            Some(Ordering::Less)
        } else if *other > self.end {
            Some(Ordering::Greater)
        } else {
            Some(Ordering::Equal)
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct LinePoint {
    distance: f64,
    id: PointId,
}

/// `LinePoint`s are equal iff they have the same `id`, regardless of their `distance` value.
impl PartialEq for LinePoint {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for LinePoint {}

/// If two `LinePoint`s have different `id`s, but the same `distance` value, they are ordered by
/// their `id`. This is arbitrary, but necessary for compatibility with `Eq`.
impl Ord for LinePoint {
    fn cmp(&self, other: &Self) -> Ordering {
        if self == other {
            Ordering::Equal
        } else if self.distance < other.distance {
            Ordering::Less
        } else if other.distance < self.distance {
            Ordering::Greater
        } else {
            self.id.cmp(&other.id)
        }
    }
}

impl PartialOrd for LinePoint {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

type LineId = usize;
