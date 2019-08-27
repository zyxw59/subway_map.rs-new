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
/// Macro for building Expressions
macro_rules! expression {
    ($op:tt, ($($x:tt)+), ($($y:tt)+)) => {
        {
            use $crate::tables::Table;
            $crate::expressions::Expression::BinaryOperator(
                $crate::operators::BinaryBuiltins.get($op).unwrap(),
                Box::new((
                        expression!($($x)+),
                        expression!($($y)+),
                        ))
            )
        }
    };
    ($op:tt, $x:expr, $y:expr) => {
        expression!($op, ($x), ($y))
    };
    ($op:tt, $($x:tt)+) => {
        {
            use $crate::tables::Table;
            $crate::expressions::Expression::UnaryOperator(
                $crate::operators::UnaryBuiltins.get($op).unwrap(),
                Box::new(expression!($($x)+)),
                )
        }
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
    (#$var:expr) => {
        $crate::expressions::Expression::Variable($crate::expressions::Variable::from($var))
    };
    ($x:expr) => {
        $crate::expressions::Expression::Value($crate::values::Value::Number($x as f64))
    };
    ($x:expr, $y:expr) => {
        $crate::expressions::Expression::Value($crate::values::Value::Point($x as f64, $y as f64))
    };
}
