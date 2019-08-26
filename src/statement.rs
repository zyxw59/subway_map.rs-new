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
#[derive(Clone, Debug)]
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
    Line(LineStatement),
    /// A declaration of a stop.
    Stop(StopStatement),
}

/// A statement declaring one or more points.
#[derive(Clone, Debug)]
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
        to: Variable,
        points: Vec<(Option<Expression>, Variable)>,
    },
}

/// A declaration of a line.
#[derive(Clone, Debug)]
pub struct LineStatement {
    /// The name of the line
    pub name: Variable,
    /// The style of the line
    pub style: Option<Variable>,
    /// The route of the line
    pub route: Vec<Segment>,
}

/// A segment in a route.
#[derive(Clone, Debug)]
pub struct Segment {
    /// The start point of the segment.
    pub start: Variable,
    /// The end point of the segment.
    pub end: Variable,
    /// The offset of the segment.
    pub offset: Expression,
}

/// A declaration of a stop.
#[derive(Clone, Debug)]
pub struct StopStatement {
    /// The location of the stop.
    pub point: Variable,
    /// The style of the stop.
    pub style: Variable,
    /// The set of lines which stop at the stop, or `None` if all lines stop.
    pub lines: Option<Vec<Variable>>,
    /// The label.
    pub label: Option<Label>,
}

/// A label for a stop.
#[derive(Clone, Debug)]
pub struct Label {
    /// The label text.
    pub text: String,
    /// The positioning of the label.
    pub position: StopPosition,
}

/// The position of a stop label.
#[derive(Clone, Debug)]
pub enum StopPosition {
    End,
    Above,
    Below,
    Left,
    Right,
}
