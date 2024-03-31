use std::rc::Rc;

use crate::{
    builtin::functions::common::eval_1_arg_special_form, eval::eval_cow, Error, ErrorKind,
    TulispContext, TulispObject, TulispValue,
};

pub(crate) fn add(ctx: &mut TulispContext) {
    fn fround(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
        eval_1_arg_special_form(ctx, "fround", args, false, |ctx, arg1, _| {
            let val = eval_cow(ctx, arg1)?;
            if val.floatp() {
                Ok(f64::round(val.as_float().unwrap()).into())
            } else {
                Err(Error::new(
                    ErrorKind::TypeMismatch,
                    format!("Expected float for fround. Got: {}", val),
                ))
            }
        })
    }
    intern_set_func!(ctx, fround);

    fn ftruncate(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
        eval_1_arg_special_form(ctx, "ftruncate", args, false, |ctx, arg1, _| {
            let val = eval_cow(ctx, arg1)?;
            if val.floatp() {
                Ok(f64::trunc(val.as_float().unwrap()).into())
            } else {
                Err(Error::new(
                    ErrorKind::TypeMismatch,
                    format!("Expected float for ftruncate. Got: {}", val),
                ))
            }
        })
    }
    intern_set_func!(ctx, ftruncate);
}
