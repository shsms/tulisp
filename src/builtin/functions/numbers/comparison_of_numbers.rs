use crate::{
    Error, TulispContext, TulispObject, TulispValue, builtin::functions::functions::reduce_with,
};
use std::rc::Rc;

macro_rules! compare_ops {
    ($oper:expr) => {{
        |selfobj: &TulispObject, other: &TulispObject| -> Result<bool, Error> {
            if selfobj.floatp() {
                let s: f64 = selfobj.as_float().unwrap();
                let o: f64 = other.try_into()?;
                Ok($oper(&s, &o))
            } else if other.floatp() {
                let o: f64 = other.as_float().unwrap();
                let s: f64 = selfobj.try_into()?;
                Ok($oper(&s, &o))
            } else {
                let s: i64 = selfobj.try_into()?;
                let o: i64 = other.try_into()?;
                Ok($oper(&s, &o))
            }
        }
    }};
}

macro_rules! compare_impl {
    ($name:ident, $symbol:literal) => {
        fn $name(ctx: &mut TulispContext, rest: &TulispObject) -> Result<TulispObject, Error> {
            let items: Vec<TulispObject> = rest.base_iter().collect();
            if items.len() < 2 {
                return Err(Error::new(
                    crate::ErrorKind::OutOfRange,
                    format!("{} requires at least 2 arguments", $symbol),
                ));
            }
            for items in items.windows(2) {
                let a = ctx.eval(&items[0])?;
                let b = ctx.eval(&items[1])?;
                if !a.numberp() {
                    return Err(Error::new(
                        crate::ErrorKind::TypeMismatch,
                        format!("Expected number, found: {a}"),
                    )
                    .with_trace(items[0].clone()));
                }
                if !b.numberp() {
                    return Err(Error::new(
                        crate::ErrorKind::TypeMismatch,
                        format!("Expected number, found: {b}"),
                    )
                    .with_trace(items[1].clone()));
                }
                if !compare_ops!(std::cmp::PartialOrd::$name)(&a, &b)? {
                    return Ok(TulispObject::nil());
                }
            }
            Ok(TulispObject::t())
        }
    };
}

pub(crate) fn add(ctx: &mut TulispContext) {
    compare_impl!(gt, ">");
    intern_set_func!(ctx, gt, ">");

    compare_impl!(ge, ">=");
    intern_set_func!(ctx, ge, ">=");

    compare_impl!(lt, "<");
    intern_set_func!(ctx, lt, "<");

    compare_impl!(le, "<=");
    intern_set_func!(ctx, le, "<=");

    fn max(ctx: &mut TulispContext, rest: &TulispObject) -> Result<TulispObject, Error> {
        reduce_with(ctx, rest, max_min_ops!(max))
    }
    intern_set_func!(ctx, max, "max");

    fn min(ctx: &mut TulispContext, rest: &TulispObject) -> Result<TulispObject, Error> {
        reduce_with(ctx, rest, max_min_ops!(min))
    }
    intern_set_func!(ctx, min, "min");
}
