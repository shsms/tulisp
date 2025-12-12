use crate::{Error, TulispContext, destruct_eval_bind};

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.add_special_form("fround", |ctx, args| {
        destruct_eval_bind!(ctx, (x) = args);

        if x.floatp() {
            Ok(f64::round(x.as_float().unwrap()).into())
        } else {
            Err(Error::type_mismatch(format!(
                "Expected float for fround. Got: {}",
                x,
            )))
        }
    });

    ctx.add_special_form("ftruncate", |ctx, args| {
        destruct_eval_bind!(ctx, (x) = args);

        if x.floatp() {
            Ok(f64::trunc(x.as_float().unwrap()).into())
        } else {
            Err(Error::type_mismatch(format!(
                "Expected float for ftruncate. Got: {}",
                x,
            )))
        }
    });
}
