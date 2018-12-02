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
