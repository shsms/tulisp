use crate::{TulispContext, destruct_eval_bind};

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.add_special_form("fround", |ctx, args| {
        destruct_eval_bind!(ctx, (x) = args);

        Ok(f64::round(x.as_float()?).into())
    });

    ctx.add_special_form("ftruncate", |ctx, args| {
        destruct_eval_bind!(ctx, (x) = args);

        Ok(f64::trunc(x.as_float()?).into())
    });
}
