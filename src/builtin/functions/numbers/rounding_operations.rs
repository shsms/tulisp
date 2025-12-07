use std::rc::Rc;

use crate::{
    Error, TulispContext, TulispObject, TulispValue,
    builtin::functions::common::eval_1_arg_special_form, eval::eval_and_then,
};

pub(crate) fn add(ctx: &mut TulispContext) {
    fn fround(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
        eval_1_arg_special_form(ctx, "fround", args, false, |ctx, arg1, _| {
            eval_and_then(ctx, arg1, |_, x| {
                if x.floatp() {
                    Ok(f64::round(x.as_float().unwrap()).into())
                } else {
                    Err(Error::type_mismatch(format!(
                        "Expected float for fround. Got: {}",
                        x
                    )))
                }
            })
        })
    }
    intern_set_func!(ctx, fround);

    fn ftruncate(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
        eval_1_arg_special_form(ctx, "ftruncate", args, false, |ctx, arg1, _| {
            eval_and_then(ctx, arg1, |_, x| {
                if x.floatp() {
                    Ok(f64::trunc(x.as_float().unwrap()).into())
                } else {
                    Err(Error::type_mismatch(format!(
                        "Expected float for ftruncate. Got: {}",
                        x
                    )))
                }
            })
        })
    }
    intern_set_func!(ctx, ftruncate);
}
