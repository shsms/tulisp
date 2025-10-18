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
        fn $name(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
            if args.null() || args.cdr_and_then(|x| Ok(x.null()))? {
                return Err(Error::new(
                    crate::ErrorKind::OutOfRange,
                    format!("{} requires at least 2 arguments", $symbol),
                ));
            }
            args.car_and_then(|x| {
                ctx.eval_and_then(x, |ctx, first| {
                    if !first.numberp() {
                        return Err(Error::new(
                            crate::ErrorKind::TypeMismatch,
                            format!("Expected number, found: {first}"),
                        )
                        .with_trace(args.car()?));
                    }
                    args.cadr_and_then(|x| {
                        ctx.eval_and_then(x, |ctx, second| {
                            if !second.numberp() {
                                return Err(Error::new(
                                    crate::ErrorKind::TypeMismatch,
                                    format!("Expected number, found: {second}"),
                                )
                                .with_trace(args.cadr()?));
                            }
                            if compare_ops!(std::cmp::PartialOrd::$name)(first, second)? {
                                args.cdr_and_then(|cdr| {
                                    if cdr.cdr_and_then(|x| Ok(x.null()))? {
                                        Ok(TulispObject::t())
                                    } else {
                                        $name(ctx, &cdr)
                                    }
                                })
                            } else {
                                Ok(TulispObject::nil())
                            }
                        })
                    })
                })
            })
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
