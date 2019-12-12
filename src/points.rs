use std::cmp::Ordering;
use std::collections::{hash_map::Entry, BTreeSet, HashMap, HashSet};
use std::ops::{Index, IndexMut};

use itertools::Itertools;
use svg::node::{
    element::{path::Data, Path, Use},
    Node,
};

use crate::corner::{Corner, ParallelShift};
use crate::document::Document;
use crate::error::{EvaluatorError, MathError};
use crate::expressions::Variable;
use crate::statement;
use crate::values::Point;

#[derive(Default, Debug)]
pub struct PointCollection {
    points: Vec<PointInfo>,
    point_ids: HashMap<Variable, PointId>,
    lines: Vec<Line>,
    pairs: HashMap<(PointId, PointId), LineId>,
    routes: Vec<Route>,
    route_ids: HashMap<Variable, RouteId>,
    stops: Vec<Stop>,
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
        self.point_ids
            .get(k)
            .and_then(|&PointId(id)| self.points.get(id))
    }

    pub fn get_point(&self, k: &str) -> Option<Point> {
        self.get_point_info(k).map(|info| info.info.value)
    }

    pub fn get_point_line_number(&self, k: &str) -> Option<usize> {
        self.get_point_info(k).map(|info| info.info.line_number)
    }

    /// Inserts a new, empty route, and returns the id of that route; or returns an error if a
    /// route with that name has already been defined.
    pub fn insert_route_get_id(
        &mut self,
        name: Variable,
        width: f64,
        styles: Vec<Variable>,
        line_number: usize,
    ) -> Result<RouteId, EvaluatorError> {
        match self.route_ids.entry(name) {
            Entry::Occupied(e) => {
                let &id = e.get();
                Err(EvaluatorError::RouteRedefinition(
                    e.key().clone(),
                    line_number,
                    self[id].line_number,
                ))
            }
            Entry::Vacant(e) => {
                let id = RouteId(self.routes.len());
                self.routes
                    .push(Route::new(e.key().clone(), width, styles, line_number));
                e.insert(id);
                Ok(id)
            }
        }
    }

    fn insert_point_get_id(
        &mut self,
        name: Variable,
        value: Point,
        line_number: usize,
    ) -> Result<PointId, EvaluatorError> {
        match self.point_ids.entry(name) {
            Entry::Occupied(e) => {
                let &id = e.get();
                Err(EvaluatorError::PointRedefinition(
                    e.key().clone(),
                    line_number,
                    self[id].info.line_number,
                ))
            }
            Entry::Vacant(e) => {
                let id = PointId(self.points.len());
                self.points.push(PointInfo::new(value, id, line_number));
                e.insert(id);
                Ok(id)
            }
        }
    }

    /// Returns a `LineId` so that `self` isn't mutably borrowed
    fn get_or_insert_line(&mut self, p1: PointId, p2: PointId) -> LineId {
        if let Some(&line_id) = self.pairs.get(&(p1, p2)) {
            line_id
        } else {
            let line_id = LineId(self.lines.len());
            self.add_pair(p1, p2, line_id);
            let p1 = &self[p1];
            let p2 = &self[p2];
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
        let line = self
            .pairs
            .get(&(p1.id, p2.id))
            .and_then(|&LineId(id)| self.lines.get(id))?;
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

    /// Inserts the point, or if the point already exists, return a point redefinition error.
    pub fn insert_point(
        &mut self,
        name: Variable,
        value: Point,
        line_number: usize,
    ) -> Result<(), EvaluatorError> {
        self.insert_point_get_id(name, value, line_number)
            .map(|_| ())
    }

    fn add_pair(&mut self, p1: PointId, p2: PointId, line: LineId) {
        self.pairs.insert((p1, p2), line);
        self.pairs.insert((p2, p1), line);
        self[p1].lines.insert(line);
        self[p2].lines.insert(line);
    }

    /// Creates a new line from a start point, a direction, and (name, distance) pairs along the
    /// line for new points to create.
    ///
    /// Because only one existing point is referenced, this necessarily creates a new line.
    pub fn new_line(
        &mut self,
        start_point: Variable,
        direction: Point,
        points: impl IntoIterator<Item = (Variable, f64)>,
        line_number: usize,
    ) -> Result<(), EvaluatorError> {
        let line_id = LineId(self.lines.len());
        let &start_id = self
            .point_ids
            .get(&start_point)
            .ok_or(EvaluatorError::Math(
                MathError::Variable(start_point),
                line_number,
            ))?;
        let origin = self[start_id].info;
        let mut line = Line::from_origin_direction(origin, direction);
        let mut total_distance = 0.0;
        for (name, distance) in points {
            total_distance += distance;
            let point = total_distance * direction + origin.value;
            let id = self.insert_point_get_id(name, point, line_number)?;
            line.points.insert(LinePoint {
                id,
                distance: total_distance,
            });
        }
        for (&p1, &p2) in line.points.iter().tuple_combinations() {
            self.add_pair(p1.id, p2.id, line_id);
        }
        self.lines.push(line);
        Ok(())
    }

    /// Extends the line specified by the given points, with intermediate points indicated as a
    /// fraction of the distance between the given points.
    pub fn extend_line(
        &mut self,
        start_point: Variable,
        end_point: Variable,
        points: impl IntoIterator<Item = (Variable, f64)>,
        line_number: usize,
    ) -> Result<(), EvaluatorError> {
        let start_id = self.point_ids[&start_point];
        let end_id = self.point_ids[&end_point];
        let line_id = self.get_or_insert_line(start_id, end_id);
        let start = self[start_id].info.value;
        let end = self[end_id].info.value;
        let direction = end - start;
        let (start_distance, distance_scale) = {
            let line = &self[line_id];
            (line.distance(start), line.relative_distance(start, end))
        };
        for (name, distance) in points {
            let id = self.insert_point_get_id(name, distance * direction + start, line_number)?;
            self[line_id].points.insert(LinePoint {
                id,
                distance: distance * distance_scale + start_distance,
            });
        }
        for (&p1, &p2) in self.lines[line_id.0].points.iter().tuple_combinations() {
            self.pairs.insert((p1.id, p2.id), line_id);
            self.pairs.insert((p2.id, p1.id), line_id);
            self.points[p1.id.0].lines.insert(line_id);
            self.points[p2.id.0].lines.insert(line_id);
        }
        Ok(())
    }

    /// Appends a given segment to the given route.
    pub fn add_segment<'a>(
        &mut self,
        route: RouteId,
        start: &'a str,
        end: &'a str,
        offset: isize,
    ) -> Result<(), &'a str> {
        let p1 = self.get_point_info(start).ok_or(start)?.info;
        let p2 = self.get_point_info(end).ok_or(end)?.info;
        let line_id = self.get_or_insert_line(p1.id, p2.id);
        self[route].add_segment(p1.id, p2.id, offset);
        let width = self[route].width;
        self[line_id].add_segment(p1, p2, offset, width, route);
        Ok(())
    }

    pub fn add_stop(
        &mut self,
        statement::Stop {
            point,
            styles,
            routes,
            label,
        }: statement::Stop,
        line_number: usize,
    ) -> Result<(), EvaluatorError> {
        let point = *self.point_ids.get(&point).ok_or(EvaluatorError::Math(
            MathError::Variable(point),
            line_number,
        ))?;
        let routes = routes
            .map(|vec| {
                vec.into_iter()
                    .map(|rte| {
                        self.route_ids
                            .get(&rte)
                            .copied()
                            .ok_or(EvaluatorError::Math(MathError::Variable(rte), line_number))
                    })
                    .collect::<Result<Vec<_>, _>>()
            })
            .transpose()?;
        self.stops.push(Stop {
            point,
            style: styles.join(" "),
            routes,
            label,
        });
        Ok(())
    }

    pub fn draw_stops(&self, default_width: f64, document: &mut Document) {
        for stop in &self.stops {
            self.draw_stop(stop, default_width, document);
        }
    }

    fn draw_stop(&self, stop: &Stop, default_width: f64, document: &mut Document) {
        // just put a circle at the stop for now
        // TODO: More complicated stop icons
        // TODO: Stop labels
        // step 0: get the point info from the stop
        let point = &self[stop.point];
        // step 1: get the locations of all the points to place stop markers.
        let mut points_and_routes: Vec<(Point, RouteId)> = Vec::new();
        for &line_id in &point.lines {
            let line = &self[line_id];
            let segments = line.get_segments_containing_point(point.info);
            for seg in segments.iter().flatten() {
                for &(route_id, offset) in &seg.routes {
                    if stop
                        .routes
                        .as_ref()
                        .map(|routes| routes.contains(&route_id))
                        .unwrap_or(true)
                    {
                        let offset = seg.calculate_offset(offset, false, default_width);
                        let dir = line.direction;
                        points_and_routes.push((
                            (-dir.unit().perp()).mul_add(offset, point.info.value),
                            route_id,
                        ));
                    }
                }
            }
        }
        // step 2: draw the stop icons
        for (point, route) in points_and_routes {
            let route = &self[route];
            let marker = Use::new()
                .set("href", "#stop")
                .set("x", point.0)
                .set("y", point.1)
                .set(
                    "class",
                    format!("stop {} {} {}", route.name, stop.style, route.style),
                );
            document.add_stop(marker);
        }
    }

    pub fn draw_routes(&self, default_width: f64, inner_radius: f64, document: &mut Document) {
        for route in &self.routes {
            let path = self.route_to_path(route, default_width, inner_radius);
            document.add_route(&route.name, &route.style, path);
        }
    }

    fn route_to_path(&self, route: &Route, default_width: f64, inner_radius: f64) -> Path {
        let mut path = Path::new()
            .set("id", format!("route-{}", route.name))
            .set("class", format!("route {}", route.style));
        if let Some(segment) = route.segments.first() {
            // the start of the route
            let mut data = Data::new().move_to(self.segment_start(segment, default_width));
            for (current, next) in route.segments.iter().tuple_windows() {
                // process `current` in the loop; `next` will be handled on the next iteration, or
                // after the loop, for the last segment.
                for shift in self.parallel_shifts(current, default_width) {
                    data = shift.apply(data);
                }
                if current.end == next.start {
                    match self.are_collinear(current.start, current.end, next.end) {
                        Collinearity::Sequential => {
                            // parallel shift
                            if let Some(shift) = self.parallel_shift(current, next, default_width) {
                                data = shift.apply(data);
                            }
                        }
                        Collinearity::NotSequential => {
                            if current.offset == -next.offset {
                                // no turn, just a straight line ending.
                                data = data.line_to(self.segment_end(current, default_width));
                            } else {
                                // u-turn
                                data = self.u_turn(current, next, default_width).apply(data);
                            }
                        }
                        Collinearity::NotCollinear => {
                            // corner
                            data = self
                                .corner(current, next, default_width, inner_radius)
                                .apply(data);
                        }
                    }
                } else {
                    // end this line, and move to the start of the next.
                    data = data
                        .line_to(self.segment_end(current, default_width))
                        .move_to(self.segment_start(next, default_width));
                }
            }
            let current = route.segments.last().unwrap();
            for shift in self.parallel_shifts(current, default_width) {
                data = shift.apply(data);
            }
            data = data.line_to(self.segment_end(current, default_width));
            path.assign("d", data);
        }
        path
    }

    pub fn parallel_shifts(
        &self,
        segment: &RouteSegment,
        default_width: f64,
    ) -> impl Iterator<Item = ParallelShift> + '_ {
        // this is only called on segments which have already been added, so we can be sure that
        // all the points mentioned are valid.
        let start_id = segment.start;
        let end_id = segment.end;
        let start = self[start_id].info;
        let end = self[end_id].info;
        let line = &self[(start_id, end_id)];
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
        segment_in: &RouteSegment,
        segment_out: &RouteSegment,
        default_width: f64,
    ) -> Option<ParallelShift> {
        let start_id = segment_in.start;
        let end_id = segment_in.end;
        let start = self[start_id].info;
        let end = self[end_id].info;
        let line = &self[(start_id, end_id)];
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
            Some(ParallelShift::new(offset_in, offset_out, dir, end.value))
        }
    }

    pub fn u_turn(
        &self,
        segment_in: &RouteSegment,
        segment_out: &RouteSegment,
        default_width: f64,
    ) -> Corner {
        let start_id = segment_in.start;
        let end_id = segment_in.end;
        let start = self[start_id].info;
        let end = self[end_id].info;
        let line = &self[(start_id, end_id)];
        let (reverse, seg) = line.get_segment(start, end);
        let offset_in = seg.calculate_offset(segment_in.offset, reverse, default_width);
        let offset_out = seg.calculate_offset(segment_out.offset, !reverse, default_width);
        Corner::u_turn(start.value, end.value, offset_in, offset_out)
    }

    pub fn corner(
        &self,
        segment_in: &RouteSegment,
        segment_out: &RouteSegment,
        default_width: f64,
        inner_radius: f64,
    ) -> Corner {
        let start_id = segment_in.start;
        let corner_id = segment_in.end;
        let end_id = segment_out.end;
        let start = self[start_id].info;
        let corner = self[corner_id].info;
        let end = self[end_id].info;
        let line_in = &self[(start_id, corner_id)];
        let line_out = &self[(corner_id, end_id)];
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

    pub fn segment_start(&self, segment: &RouteSegment, default_width: f64) -> Point {
        self.segment_start_or_end(segment, true, default_width)
    }

    pub fn segment_end(&self, segment: &RouteSegment, default_width: f64) -> Point {
        self.segment_start_or_end(segment, false, default_width)
    }

    fn segment_start_or_end(
        &self,
        segment: &RouteSegment,
        is_start: bool,
        default_width: f64,
    ) -> Point {
        let start_id = segment.start;
        let end_id = segment.end;
        let mut start = self[start_id].info;
        let mut end = self[end_id].info;
        let line = &self[(start_id, end_id)];
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

    pub fn are_collinear(&self, p1: PointId, p2: PointId, p3: PointId) -> Collinearity {
        // this is only called on segments which have already been added, so we can be sure that
        // all the points mentioned are valid.
        if self.pairs[&(p1, p2)] != self.pairs[&(p2, p3)] {
            Collinearity::NotCollinear
        } else {
            let line = &self[(p1, p2)];
            let p1 = line.line_point(self[p1].info);
            let p2 = line.line_point(self[p2].info);
            let p3 = line.line_point(self[p3].info);
            if p1 <= p2 && p2 <= p3 || p3 <= p2 && p2 <= p1 {
                Collinearity::Sequential
            } else {
                Collinearity::NotSequential
            }
        }
    }
}

impl Index<PointId> for PointCollection {
    type Output = PointInfo;

    fn index(&self, PointId(idx): PointId) -> &PointInfo {
        &self.points[idx]
    }
}

impl IndexMut<PointId> for PointCollection {
    fn index_mut(&mut self, PointId(idx): PointId) -> &mut PointInfo {
        &mut self.points[idx]
    }
}

impl Index<LineId> for PointCollection {
    type Output = Line;

    fn index(&self, LineId(idx): LineId) -> &Line {
        &self.lines[idx]
    }
}

impl IndexMut<LineId> for PointCollection {
    fn index_mut(&mut self, LineId(idx): LineId) -> &mut Line {
        &mut self.lines[idx]
    }
}

impl Index<(PointId, PointId)> for PointCollection {
    type Output = Line;

    fn index(&self, (p1, p2): (PointId, PointId)) -> &Line {
        &self[self.pairs[&(p1, p2)]]
    }
}

impl Index<RouteId> for PointCollection {
    type Output = Route;

    fn index(&self, RouteId(idx): RouteId) -> &Route {
        &self.routes[idx]
    }
}

impl IndexMut<RouteId> for PointCollection {
    fn index_mut(&mut self, RouteId(idx): RouteId) -> &mut Route {
        &mut self.routes[idx]
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
    NotCollinear,
}

#[derive(Clone, Debug)]
pub struct PointInfo {
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

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct PointId(usize);

#[derive(Clone, Debug)]
pub struct Line {
    direction: Point,
    origin: Point,
    points: BTreeSet<LinePoint>,
    segments: Vec<Segment>,
}

impl Line {
    /// Create a new line between the two points.
    fn from_pair(p1: PointInfoLite, p2: PointInfoLite) -> Line {
        let mut points = BTreeSet::new();
        points.insert(LinePoint {
            distance: 0.0,
            id: p1.id,
        });
        points.insert(LinePoint {
            distance: 1.0,
            id: p2.id,
        });
        Line {
            direction: p2.value - p1.value,
            origin: p1.value,
            points,
            segments: Vec::new(),
        }
    }

    /// Create a new line with the given origin and direction.
    fn from_origin_direction(origin: PointInfoLite, direction: Point) -> Line {
        let mut points = BTreeSet::new();
        points.insert(LinePoint {
            distance: 0.0,
            id: origin.id,
        });
        Line {
            origin: origin.value,
            direction,
            points,
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
    fn add_segment(
        &mut self,
        p1: PointInfoLite,
        p2: PointInfoLite,
        mut offset: isize,
        width: f64,
        route_id: RouteId,
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
                Some(Segment::new(p1, end_point, offset, width, route_id))
            }
        };
        let post_split = match end_seg {
            Ok(end) => self.segments[end]
                .split(p2)
                .map(|seg| seg.update_new(offset, width, route_id)),
            Err(end) => {
                let start_point = if end > 0 {
                    self.segments[end - 1].end.max(p1)
                } else {
                    p1
                };
                if start_point == p2 {
                    None
                } else {
                    Some(Segment::new(start_point, p2, offset, width, route_id))
                }
            }
        };
        let start = start_seg.unwrap_or_else(|err| err);
        let end = end_seg.unwrap_or_else(|err| err);
        // update the intermediate segments
        for seg in &mut self.segments[start..end] {
            seg.update(offset, width, route_id);
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
            (reverse, &self.segments[idx], &self.segments[idx - 1])
        } else {
            (reverse, &self.segments[idx - 1], &self.segments[idx])
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

    fn get_segments_containing_point(&self, point: PointInfoLite) -> [Option<&Segment>; 2] {
        let point = self.line_point(point);
        let idx = self
            .segments
            .binary_search_by(|seg| seg.cmp(&point))
            .unwrap_or_else(|idx| idx);
        [
            // the first segment returned is the segment ending at `point`, if it exists
            if idx > 0 && self.segments[idx - 1].end == point {
                Some(&self.segments[idx - 1])
            } else {
                None
            },
            // the second segment returned is the segment with
            // `segment.start <= point < segment.end`, if it exists
            if idx < self.segments.len() && self.segments[idx] == point {
                Some(&self.segments[idx])
            } else {
                None
            },
        ]
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
    routes: Vec<(RouteId, isize)>,
    pos_offsets: Vec<Option<f64>>,
    neg_offsets: Vec<Option<f64>>,
}

impl Segment {
    pub fn new(
        start: LinePoint,
        end: LinePoint,
        offset: isize,
        width: f64,
        route_id: RouteId,
    ) -> Segment {
        Segment {
            start,
            end,
            routes: Vec::new(),
            pos_offsets: Vec::new(),
            neg_offsets: Vec::new(),
        }
        .update_new(offset, width, route_id)
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
        (offset, radius.abs())
    }

    fn offset_max(&self, default_width: f64) -> f64 {
        if !self.pos_offsets.is_empty() {
            self.calculate_offset((self.pos_offsets.len() - 1) as isize, false, default_width)
        } else {
            let mut n = 0;
            for w in &self.neg_offsets {
                if w.is_some() {
                    break;
                } else {
                    n += 1;
                }
            }
            f64::from(n) * default_width
        }
    }

    fn offset_min(&self, default_width: f64) -> f64 {
        if !self.neg_offsets.is_empty() {
            self.calculate_offset(!(self.neg_offsets.len() - 1) as isize, false, default_width)
        } else if self.pos_offsets[0].is_none() {
            0.0
        } else {
            let mut n = 0;
            for w in &self.pos_offsets {
                if w.is_some() {
                    break;
                } else {
                    n += 1;
                }
            }
            f64::from(n) * default_width
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
    fn update_new(mut self, offset: isize, width: f64, route_id: RouteId) -> Segment {
        self.update(offset, width, route_id);
        self
    }

    /// Update the segment with the given `offset` and `width`.
    fn update(&mut self, offset: isize, width: f64, route_id: RouteId) {
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
        self.routes.push((route_id, offset));
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

impl PartialEq<LinePoint> for Segment {
    fn eq(&self, other: &LinePoint) -> bool {
        self.cmp(other) == Ordering::Equal
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

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct LineId(usize);

#[derive(Clone, Debug)]
pub struct Route {
    /// The name of the route.
    name: Variable,
    /// The width of the route line.
    width: f64,
    /// The style (if any) for the route.
    style: String,
    /// The segments making up the route.
    segments: Vec<RouteSegment>,
    /// The number of the line where the route is defined.
    line_number: usize,
}

impl Route {
    fn new(name: Variable, width: f64, style: Vec<Variable>, line_number: usize) -> Route {
        Route {
            name,
            width,
            style: style.join(" "),
            segments: Vec::new(),
            line_number,
        }
    }

    fn add_segment(&mut self, start: PointId, end: PointId, offset: isize) {
        self.segments.push(RouteSegment { start, end, offset });
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct RouteId(usize);

/// A segment of a route.
#[derive(Clone, Copy, Debug)]
pub struct RouteSegment {
    pub start: PointId,
    pub end: PointId,
    pub offset: isize,
}

/// A stop.
#[derive(Clone, Debug)]
pub struct Stop {
    /// The location of the stop.
    pub point: PointId,
    /// The style of the stop.
    pub style: String,
    /// The set of routes which stop at the stop, or `None` if all lines stop.
    pub routes: Option<Vec<RouteId>>,
    /// The label.
    pub label: Option<statement::Label>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn extend_line_in_order() {
        let mut points = PointCollection::new();
        points.insert_point("A".into(), Point(0.0, 0.0), 0).unwrap();
        points.insert_point("B".into(), Point(2.0, 0.0), 1).unwrap();
        points
            .extend_line(
                "A".into(),
                "B".into(),
                vec![("C".into(), 0.1), ("D".into(), 0.5)],
                2,
            )
            .unwrap();
        let a_id = points.point_ids["A"];
        let b_id = points.point_ids["B"];
        let line = &points[(a_id, b_id)];
        assert_eq!(line.direction, Point(2.0, 0.0));
        assert_eq!(
            line.points.iter().map(|p| p.distance).collect::<Vec<_>>(),
            &[0.0, 0.1, 0.5, 1.0]
        );
    }

    #[test]
    fn extend_line_in_reverse_order() {
        let mut points = PointCollection::new();
        points.insert_point("A".into(), Point(0.0, 0.0), 0).unwrap();
        points.insert_point("B".into(), Point(2.0, 0.0), 1).unwrap();
        points
            .extend_line(
                "A".into(),
                "B".into(),
                vec![("C".into(), 0.5), ("D".into(), 0.1)],
                2,
            )
            .unwrap();
        let a_id = points.point_ids["A"];
        let b_id = points.point_ids["B"];
        let line = &points[(a_id, b_id)];
        assert_eq!(line.direction, Point(2.0, 0.0));
        assert_eq!(
            line.points.iter().map(|p| p.distance).collect::<Vec<_>>(),
            &[0.0, 0.1, 0.5, 1.0]
        );
    }

    #[test]
    fn new_line() {
        let mut points = PointCollection::new();
        points.insert_point("A".into(), Point(0.0, 0.0), 0).unwrap();
        points
            .new_line(
                "A".into(),
                Point(2.0, 0.0),
                vec![("B".into(), 1.0), ("C".into(), 0.5), ("D".into(), 1.0)],
                2,
            )
            .unwrap();
        let a_id = points.point_ids["A"];
        let b_id = points.point_ids["B"];
        let line = &points[(a_id, b_id)];
        assert_eq!(line.direction, Point(2.0, 0.0));
        assert_eq!(
            line.points.iter().map(|p| p.distance).collect::<Vec<_>>(),
            &[0.0, 1.0, 1.5, 2.5]
        );
    }

    #[test]
    fn segments_containing_point() {
        let mut points = PointCollection::new();
        points.insert_point("A".into(), Point(0.0, 0.0), 0).unwrap();
        points
            .new_line(
                "A".into(),
                Point(2.0, 0.0),
                vec![("B".into(), 1.0), ("C".into(), 0.5), ("D".into(), 1.0)],
                2,
            )
            .unwrap();
        let route = points
            .insert_route_get_id("red".into(), 1.0, Vec::new(), 3)
            .unwrap();
        points.add_segment(route, "A", "B", 0).unwrap();
        points.add_segment(route, "B", "D", 0).unwrap();
        let a_id = points.point_ids["A"];
        let b_id = points.point_ids["B"];
        let c_id = points.point_ids["C"];
        let d_id = points.point_ids["D"];
        let line = &points[(a_id, b_id)];
        assert_eq!(
            line.get_segments_containing_point(points[a_id].info),
            [None, Some(&line.segments[0])]
        );
        assert_eq!(
            line.get_segments_containing_point(points[b_id].info),
            [Some(&line.segments[0]), Some(&line.segments[1])]
        );
        assert_eq!(
            line.get_segments_containing_point(points[c_id].info),
            [None, Some(&line.segments[1])]
        );
        assert_eq!(
            line.get_segments_containing_point(points[d_id].info),
            [Some(&line.segments[1]), None]
        );
    }
}
