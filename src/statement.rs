use std::convert::TryFrom;

use crate::expressions::{Expression, Function, Variable};

/// A statement, annotated with a line number.
#[derive(Clone, Debug)]
pub struct Statement {
    /// The statement.
    pub statement: StatementKind,
    /// The line number.
    pub line: usize,
}

/// A statement.
#[derive(Clone, Debug, PartialEq)]
pub enum StatementKind {
    /// An empty statement.
    Null,
    /// A function declaration.
    Function(Variable, Function),
    /// A variable assignment.
    Variable(Variable, Expression),
    /// A declaration of one or more points.
    Point(PointStatement),
    /// A declaration of a line.
    Line {
        /// The name of the line
        name: Variable,
        /// The style of the line
        style: Option<Variable>,
        /// The route of the line
        route: Vec<Segment>,
    },
    /// A declaration of a stop.
    Stop {
        /// The location of the stop.
        point: Variable,
        /// The style of the stop.
        style: Option<Variable>,
        /// The set of lines which stop at the stop, or `None` if all lines stop.
        lines: Option<Vec<Variable>>,
        /// The label.
        label: Option<Label>,
    },
}

/// A statement declaring one or more points.
#[derive(Clone, Debug, PartialEq)]
pub enum PointStatement {
    /// A declaration of a single point.
    Single(Variable, Expression),
    /// A declaration of a sequence of points, using the `from` ... `spaced` syntax.
    Spaced {
        from: Variable,
        spaced: Expression,
        points: Vec<(Option<Expression>, Variable)>,
    },
    /// A declaration of a sequence of points, using the `from` ... `to` syntax.
    Between {
        from: Variable,
        to: (Option<Expression>, Variable),
        points: Vec<(Option<Expression>, Variable)>,
    },
}

/// A declaration of a line.
#[derive(Clone, Debug, PartialEq)]
pub struct LineStatement {
    /// The name of the line
    pub name: Variable,
    /// The style of the line
    pub style: Option<Variable>,
    /// The route of the line
    pub route: Vec<Segment>,
}

/// A segment in a route.
#[derive(Clone, Debug, PartialEq)]
pub struct Segment {
    /// The start point of the segment.
    pub start: Variable,
    /// The end point of the segment.
    pub end: Variable,
    /// The offset of the segment.
    pub offset: Expression,
}

/// A label for a stop.
#[derive(Clone, Debug, PartialEq)]
pub struct Label {
    /// The label text.
    pub text: String,
    /// The positioning of the label.
    pub position: LabelPosition,
}

/// The position of a stop label.
#[derive(Clone, Debug, PartialEq)]
pub enum LabelPosition {
    End,
    Above,
    Below,
    Left,
    Right,
}

impl TryFrom<String> for LabelPosition {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        match value.as_ref() {
            "end" => Ok(LabelPosition::End),
            "above" => Ok(LabelPosition::Above),
            "below" => Ok(LabelPosition::Below),
            "left" => Ok(LabelPosition::Left),
            "right" => Ok(LabelPosition::Right),
            _ => Err(value),
        }
    }
}
