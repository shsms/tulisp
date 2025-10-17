use crate::{Error, ErrorKind, TulispContext, destruct_bind};

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.add_special_form("error", |ctx, args| {
        destruct_bind!((msg) = args);
        Err(Error::new(
            ErrorKind::LispError,
            ctx.eval(&msg)?.as_string()?,
        ))
    });
}
