use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};

use itertools::Itertools;

use crate::corner::{Corner, ParallelShift};
use crate::evaluator;
use crate::expressions::Variable;
use crate::values::Point;

#[derive(Default, Debug)]
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

    pub fn point_iter(&self) -> impl Iterator<Item = &Point> {
        self.points.iter().map(|info| &info.info.value)
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
        segment: &'a evaluator::Segment,
        width: f64,
    ) -> Result<(), &'a str> {
        let p1 = self
            .get_point_info(&segment.start)
            .ok_or(segment.start.as_ref())?
            .info;
        let p2 = self
            .get_point_info(&segment.end)
            .ok_or(segment.end.as_ref())?
            .info;
        let line_id = self.get_or_insert_line(p1.id, p2.id);
        self.lines[line_id].add_segment(p1, p2, segment.offset, width);
        Ok(())
    }

    pub fn parallel_shifts(
        &self,
        segment: &evaluator::Segment,
        default_width: f64,
    ) -> impl Iterator<Item = ParallelShift> + '_ {
        // this is only called on segments which have already been added, so we can be sure that
        // all the points mentioned are valid.
        let start_id = self.point_ids[&segment.start];
        let end_id = self.point_ids[&segment.end];
        let start = self.points[start_id].info;
        let end = self.points[end_id].info;
        let line = self.get_line(start_id, end_id).unwrap();
        let (reverse, segments) = line.segments_between(start, end);
        let offset = segment.offset;
        segments.tuple_windows().filter_map(move |(prev, next)| {
            let offset_in = prev.calculate_offset(offset, reverse, default_width);
            let offset_out = next.calculate_offset(offset, reverse, default_width);
            if crate::values::float_eq(offset_in, offset_out) {
                None
            } else {
                let (dir, at) = if reverse {
                    (-line.direction, line.point(prev.start.distance))
                } else {
                    (line.direction, line.point(prev.end.distance))
                };
                Some(ParallelShift::new(offset_in, offset_out, dir, at))
            }
        })
    }

    pub fn parallel_shift(
        &self,
        segment_in: &evaluator::Segment,
        segment_out: &evaluator::Segment,
        default_width: f64,
    ) -> Option<ParallelShift> {
        let start_id = self.point_ids[&segment_in.start];
        let end_id = self.point_ids[&segment_in.end];
        let start = self.points[start_id].info;
        let end = self.points[end_id].info;
        let line = self.get_line(start_id, end_id).unwrap();
        let (reverse, prev, next) = line.segments_at(start, end);
        let offset_in = prev.calculate_offset(segment_in.offset, reverse, default_width);
        let offset_out = next.calculate_offset(segment_out.offset, reverse, default_width);
        if crate::values::float_eq(offset_in, offset_out) {
            None
        } else {
            let dir = if reverse {
                -line.direction
            } else {
                line.direction
            };
            Some(ParallelShift::new(offset_in, offset_out, dir, start.value))
        }
    }

    pub fn u_turn(
        &self,
        segment_in: &evaluator::Segment,
        segment_out: &evaluator::Segment,
        default_width: f64,
    ) -> Corner {
        let start_id = self.point_ids[&segment_in.start];
        let end_id = self.point_ids[&segment_in.end];
        let start = self.points[start_id].info;
        let end = self.points[end_id].info;
        let line = self.get_line(start_id, end_id).unwrap();
        let (reverse, seg) = line.get_segment(start, end);
        let offset_in = seg.calculate_offset(segment_in.offset, reverse, default_width);
        let offset_out = seg.calculate_offset(segment_out.offset, !reverse, default_width);
        Corner::u_turn(start.value, end.value, offset_in, offset_out)
    }

    pub fn corner(
        &self,
        segment_in: &evaluator::Segment,
        segment_out: &evaluator::Segment,
        default_width: f64,
        inner_radius: f64,
    ) -> Corner {
        let start_id = self.point_ids[&segment_in.start];
        let corner_id = self.point_ids[&segment_in.end];
        let end_id = self.point_ids[&segment_out.end];
        let start = self.points[start_id].info;
        let corner = self.points[corner_id].info;
        let end = self.points[end_id].info;
        let line_in = self.get_line(start_id, corner_id).unwrap();
        let line_out = self.get_line(corner_id, end_id).unwrap();
        let (reverse_in, seg_in) = line_in.get_segment(start, corner);
        let (reverse_out, seg_out) = line_out.get_segment(end, corner);
        // true => to the left (line_in is right of end)
        // false => to the righht (line_in is left of end)
        let turn_in = line_in.right_of(end.value);
        let (offset_in, radius_in) =
            seg_in.calculate_offset_radius(segment_in.offset, reverse_in, turn_in, default_width);
        let turn_out = line_out.right_of(start.value);
        let (offset_out, radius_out) = seg_out.calculate_offset_radius(
            segment_out.offset,
            !reverse_out,
            turn_out,
            default_width,
        );
        Corner::new(
            start.value,
            corner.value,
            end.value,
            radius_in.max(radius_out) + inner_radius,
        )
        .offset(offset_in, offset_out)
    }

    pub fn segment_start(&self, segment: &evaluator::Segment, default_width: f64) -> Point {
        self.segment_start_or_end(segment, true, default_width)
    }

    pub fn segment_end(&self, segment: &evaluator::Segment, default_width: f64) -> Point {
        self.segment_start_or_end(segment, false, default_width)
    }

    fn segment_start_or_end(
        &self,
        segment: &evaluator::Segment,
        is_start: bool,
        default_width: f64,
    ) -> Point {
        let start_id = self.point_ids[&segment.start];
        let end_id = self.point_ids[&segment.end];
        let mut start = self.points[start_id].info;
        let mut end = self.points[end_id].info;
        let line = self.get_line(start_id, end_id).unwrap();
        if is_start {
            std::mem::swap(&mut start, &mut end);
        }
        let (mut reverse, seg) = line.get_segment(start, end);
        // if `is_start`, we reversed the start and end points already, so flip `reverse`
        reverse ^= is_start;
        let mut offset = seg.calculate_offset(segment.offset, reverse, default_width);
        // we want absoolute offset.
        if reverse {
            offset = -offset;
        }
        line.direction.unit().perp().mul_add(-offset, end.value)
    }

    pub fn are_collinear(&self, p1: &str, p2: &str, p3: &str) -> Collinearity {
        // this is only called on segments which have already been added, so we can be sure that
        // all the points mentioned are valid.
        let p1_id = self.point_ids[p1];
        let p2_id = self.point_ids[p2];
        let p3_id = self.point_ids[p3];
        if self.pairs[&(p1_id, p2_id)] != self.pairs[&(p2_id, p3_id)] {
            Collinearity::NotColinear
        } else {
            let line = self.get_line(p1_id, p2_id).unwrap();
            let p1 = line.line_point(self.points[p1_id].info);
            let p2 = line.line_point(self.points[p2_id].info);
            let p3 = line.line_point(self.points[p3_id].info);
            if p1 <= p2 && p2 <= p3 || p3 <= p2 && p2 <= p1 {
                Collinearity::Sequential
            } else {
                Collinearity::NotSequential
            }
        }
    }
}

/// The result of `are_collinear`.
#[derive(Clone, Copy, Debug)]
pub enum Collinearity {
    /// The three points are collinear, and in order.
    Sequential,
    /// The three points are collinear, but not in order.
    NotSequential,
    /// The three points are not collinear.
    NotColinear,
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

        let start_seg = self.segments.binary_search_by(|seg| seg.cmp(&p1));
        let end_seg = self.segments.binary_search_by(|seg| seg.cmp(&p2));
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
                let start_point = if end > 0 {
                    self.segments[end - 1].end.max(p1)
                } else {
                    p1
                };
                if start_point == p2 {
                    None
                } else {
                    Some(Segment::new(start_point, p2, offset, width))
                }
            }
        };
        let start = start_seg.unwrap_or_else(|err| err);
        let end = end_seg.unwrap_or_else(|err| err);
        // update the intermediate segments
        for seg in &mut self.segments[start..end] {
            seg.update(offset, width);
        }
        // insert the new segments if necessary
        // NOTE: followup to check for bugs
        if post_split != pre_split {
            if let Some(post_split) = post_split {
                self.segments.insert(end, post_split);
            }
        }
        if let Some(pre_split) = pre_split {
            self.segments.insert(start, pre_split);
        }
    }

    fn segments_between(
        &self,
        start: PointInfoLite,
        end: PointInfoLite,
    ) -> (bool, Box<dyn Iterator<Item = &Segment> + '_>) {
        let mut start = self.line_point(start);
        let mut end = self.line_point(end);
        let reverse = start > end;
        if reverse {
            std::mem::swap(&mut start, &mut end);
        }
        // make immutable again
        let (start, end) = (start, end);
        // this is only called with points forming a segment which has been added, so they must
        // match some segment.
        let mut start_seg = self
            .segments
            .binary_search_by(|seg| seg.cmp(&start))
            .unwrap();
        if self.segments[start_seg].end == start {
            // we matched the end of the previous segment
            start_seg += 1;
        }
        let end_seg = self
            .segments
            .binary_search_by(|seg| seg.cmp(&end))
            .unwrap_or_else(|err| err)
            - 1;
        if reverse {
            (
                reverse,
                Box::new(self.segments[start_seg..=end_seg].iter().rev()),
            )
        } else {
            (reverse, Box::new(self.segments[start_seg..=end_seg].iter()))
        }
    }

    /// Returns the segments on either side of `end`
    fn segments_at(&self, start: PointInfoLite, end: PointInfoLite) -> (bool, &Segment, &Segment) {
        let start = self.line_point(start);
        let end = self.line_point(end);
        let reverse = start > end;
        let idx = self.segments.binary_search_by(|seg| seg.cmp(&end)).unwrap();
        if reverse {
            (reverse, &self.segments[idx + 1], &self.segments[idx])
        } else {
            (reverse, &self.segments[idx], &self.segments[idx + 1])
        }
    }

    /// Returns the segment ending at `end`
    fn get_segment(&self, start: PointInfoLite, end: PointInfoLite) -> (bool, &Segment) {
        let start = self.line_point(start);
        let end = self.line_point(end);
        let reverse = start > end;
        let mut idx = self
            .segments
            .binary_search_by(|seg| seg.cmp(&end))
            .unwrap_or_else(|err| err);
        if !reverse {
            idx -= 1
        };
        (reverse, &self.segments[idx])
    }

    /// True if the line is to the right of the specified point (looking in the direction of
    /// `line.direction`.
    fn right_of(&self, point: Point) -> bool {
        self.direction.cross(point - self.origin) < 0.0
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

    pub fn calculate_offset(&self, offset: isize, reverse: bool, default_width: f64) -> f64 {
        if offset == 0 {
            0.0
        } else {
            let offset = if reverse { -offset } else { offset };
            let zero_offset = self
                .pos_offsets
                .get(0)
                .unwrap_or(&None)
                .unwrap_or(default_width);
            let value: f64 = if offset >= 0 {
                let offset = offset as usize;
                zero_offset / 2.0
                    + self.pos_offsets[1..offset]
                        .iter()
                        .map(|width| width.unwrap_or(default_width))
                        .sum::<f64>()
                    + self.pos_offsets[offset].unwrap_or(default_width) / 2.0
            } else {
                let offset = !offset as usize;
                -zero_offset / 2.0
                    - self.neg_offsets[..offset]
                        .iter()
                        .map(|width| width.unwrap_or(default_width))
                        .sum::<f64>()
                    - self.neg_offsets[offset].unwrap_or(default_width) / 2.0
            };
            if reverse {
                -value
            } else {
                value
            }
        }
    }

    pub fn calculate_offset_radius(
        &self,
        offset: isize,
        reverse: bool,
        turn_dir: bool,
        default_width: f64,
    ) -> (f64, f64) {
        let offset = self.calculate_offset(offset, reverse, default_width);
        // offset relative to the line
        let abs_offset = if reverse { -offset } else { offset };
        let radius = if turn_dir {
            abs_offset - self.offset_min(default_width)
        } else {
            self.offset_max(default_width) - abs_offset
        };
        (offset, radius)
    }

    fn offset_max(&self, default_width: f64) -> f64 {
        if self.pos_offsets.len() > 0 {
            self.calculate_offset((self.pos_offsets.len() - 1) as isize, false, default_width)
        } else {
            let mut acc = default_width / 2.0;
            let mut n = 0;
            for w in &self.neg_offsets {
                if let Some(w) = w {
                    acc += w / 2.0;
                    break;
                } else {
                    n += 1;
                }
            }
            acc + n as f64 * default_width
        }
    }

    fn offset_min(&self, default_width: f64) -> f64 {
        if self.neg_offsets.len() > 0 {
            self.calculate_offset(!(self.neg_offsets.len() - 1) as isize, false, default_width)
        } else {
            let mut acc = self
                .pos_offsets
                .get(0)
                .unwrap_or(&None)
                .unwrap_or(default_width)
                / 2.0;
            let mut n = 0;
            for w in &self.neg_offsets {
                if let Some(w) = w {
                    acc += w / 2.0;
                    break;
                } else {
                    n += 1;
                }
            }
            acc + n as f64 * default_width
        }
    }

    /// Split `self`, leaving the post-split segment in `self`'s place, returning the segment to
    /// be inserted before `self`. If the split point is equal to `self.start`, return `None`, as
    /// no new segment needs to be inserted.
    fn split(&mut self, point: LinePoint) -> Option<Segment> {
        if point == self.start {
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
            let offset = offset as usize;
            if offset >= self.pos_offsets.len() {
                self.pos_offsets.resize_with(offset + 1, Default::default);
            }
            self.pos_offsets[offset] =
                Some(self.pos_offsets[offset].map_or(width, |old| old.max(width)));
        } else {
            let offset = !offset as usize;
            if offset >= self.neg_offsets.len() {
                self.neg_offsets.resize_with(offset + 1, Default::default);
            }
            self.neg_offsets[offset] =
                Some(self.neg_offsets[offset].map_or(width, |old| old.max(width)));
        }
    }

    fn cmp(&self, other: &LinePoint) -> Ordering {
        if self.end <= *other {
            Ordering::Less
        } else if self.start > *other {
            Ordering::Greater
        } else {
            Ordering::Equal
        }
    }
}

impl PartialEq for Segment {
    fn eq(&self, other: &Self) -> bool {
        self.start == other.start && self.end == other.end
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
