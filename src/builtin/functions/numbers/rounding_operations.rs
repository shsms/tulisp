use crate::{TulispContext, destruct_eval_bind};

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.defspecial("fround", |ctx, args| {
        destruct_eval_bind!(ctx, (x) = args);

        Ok(f64::round(x.as_float()?).into())
    });

    ctx.defspecial("ftruncate", |ctx, args| {
        destruct_eval_bind!(ctx, (x) = args);

        Ok(f64::trunc(x.as_float()?).into())
    });
}
