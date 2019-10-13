/// `try!` macro, but it returns `Some(Err(err))` instead of `Err(err)` for compatibility with
/// iterators.
macro_rules! itry {
    ($expr:expr) => {
        match $expr {
            Ok(val) => val,
            Err(err) => return Some(Err(err.into())),
        }
    };
    ($expr:expr,) => {
        itry!($expr)
    };
}

/// `try!` macro, but it unwraps `Option<Result<T, E>>` to `Option<T>`, early-returning
/// `Err(_)` values.
macro_rules! try_opt {
    ($expr:expr) => {
        match $expr {
            Some(Ok(val)) => Some(val),
            Some(Err(err)) => return Err(err.into()),
            None => None,
        }
    };
    ($expr:expr,) => {
        try_opt!($expr)
    };
}

/// `try!` macro, but it unwraps `Option<Result<T, E>>`, propagating `Some(Err(_))` and `None`
/// values
macro_rules! itry_opt {
    ($expr:expr) => {
        match $expr {
            Some(Ok(val)) => val,
            Some(Err(err)) => return Some(Err(err.into())),
            None => return None,
        }
    };
    ($expr:expr,) => {
        itry_opt!($expr)
    };
}

#[cfg(test)]
macro_rules! token {
    (.) => {
        $crate::lexer::Token::Dot
    };
    ((l)) => {
        $crate::lexer::Token::LeftParen
    };
    ((r)) => {
        $crate::lexer::Token::RightParen
    };
    (,) => {
        $crate::lexer::Token::Comma
    };
    (;) => {
        $crate::lexer::Token::Semicolon
    };
    (=) => {
        $crate::lexer::Token::Equal
    };
    (#$tag:expr) => {
        $crate::lexer::Token::Tag(String::from($tag))
    };
    ($value:expr) => {
        $crate::lexer::Token::Number($value as f64)
    };
    (@$str:expr) => {
        $crate::lexer::Token::String(String::from($str))
    };
}

#[cfg(test)]
/// Macro for building Expressions
macro_rules! expression {
    ($op:tt, ($($x:tt)+), ($($y:tt)+)) => {{
        $crate::expressions::Expression::BinaryOperator(
            $crate::operators::BinaryBuiltins.get($op).unwrap(),
            Box::new((expression!($($x)+), expression!($($y)+))),
        )
    }};
    ($op:tt, ($($x:tt)+), $y:expr) => {
        expression!($op, ($($x)+), ($y))
    };
    ($op:tt, ($($x:tt)+)) => {{
        $crate::expressions::Expression::UnaryOperator(
            $crate::operators::UnaryBuiltins.get($op).unwrap(),
            Box::new(expression!($($x)+)),
        )
    }};
    ($op:tt, $x:expr, ($($y:tt)+)) => {
        expression!($op, ($x), ($($y)+))
    };
    ($op:tt, $x:expr, $y:expr) => {
        expression!($op, ($x), ($y))
    };
    ($op:tt, $x:expr) => {
        expression!($op, ($x))
    };
    ($fn:tt[$(($($x:tt)+)),*]) => {
        $crate::expressions::Expression::Function(
            $crate::expressions::Variable::from($fn),
            vec![$(expression!($($x)+)),*],
        )
    };
    (@($($x:tt)+), ($($y:tt)+)) => {
        $crate::expressions::Expression::Point(
            Box::new((expression!($($x)+), expression!($($y)+))),
        )
    };
    (@$x:expr, ($($y:tt)+)) => {
        expression!(@($x), ($($y)+))
    };
    (@($($x:tt)+), $y:expr) => {
        expression!(@($($x)+), ($y))
    };
    (@$x:expr, $y:expr) => {
        expression!(@($x), ($y))
    };
    (#$var:expr) => {
        $crate::expressions::Expression::Variable($crate::expressions::Variable::from($var))
    };
    ($x:expr) => {
        $crate::expressions::Expression::Value($crate::values::Value::Number($x as f64))
    };
}

#[cfg(test)]
macro_rules! value {
    ($x:expr) => {
        $crate::values::Value::Number($x as f64)
    };
    ($x:expr, $y:expr) => {
        $crate::values::Value::Point($x as f64, $y as f64)
    };
}

#[cfg(test)]
macro_rules! segment {
    ($start:expr, $end:expr, $($expr:tt)*) => {
        $crate::statement::Segment {
            start: $crate::expressions::Variable::from($start),
            end: $crate::expressions::Variable::from($end),
            offset: expression!($($expr)*),
        }
    }
}
