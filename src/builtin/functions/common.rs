use crate::{Error, TulispContext, TulispObject, TulispValue};

pub(crate) fn eval_1_arg_special_form(
    ctx: &mut TulispContext,
    name: &str,
    args: &TulispObject,
    has_rest: bool,
    lambda: fn(&mut TulispContext, &TulispObject, &TulispObject) -> Result<TulispObject, Error>,
) -> Result<TulispObject, Error> {
    if args.null() {
        return Err(Error::missing_argument(if has_rest {
            format!("{}: expected at least 1 argument.", name)
        } else {
            format!("{}: expected 1 argument.", name)
        }));
    }
    args.car_and_then(|arg1| {
        args.cdr_and_then(|rest| {
            if !has_rest && !rest.null() {
                return Err(Error::missing_argument(format!(
                    "{}: expected only 1 argument.",
                    name
                )));
            }
            lambda(ctx, arg1, rest)
        })
    })
}

pub(crate) fn eval_2_arg_special_form(
    ctx: &mut TulispContext,
    name: &str,
    args: &TulispObject,
    has_rest: bool,
    lambda: fn(
        &mut TulispContext,
        &TulispObject,
        &TulispObject,
        &TulispObject,
    ) -> Result<TulispObject, Error>,
) -> Result<TulispObject, Error> {
    let TulispValue::List { cons: args, .. } = &*args.inner_ref() else {
        return Err(Error::missing_argument(if has_rest {
            format!("{}: expected at least 2 arguments.", name)
        } else {
            format!("{}: expected 2 arguments.", name)
        }));
    };
    if args.cdr().null() {
        return Err(Error::missing_argument(if has_rest {
            format!("{}: expected at least 2 arguments.", name)
        } else {
            format!("{}: expected 2 arguments.", name)
        }));
    }
    let arg1 = args.car();
    args.cdr().car_and_then(|arg2| {
        args.cdr().cdr_and_then(|rest| {
            if !has_rest && !rest.null() {
                return Err(Error::missing_argument(format!(
                    "{}: expected only 2 arguments.",
                    name
                )));
            }
            lambda(ctx, arg1, arg2, rest)
        })
    })
}
