use crate::{Error, ErrorKind, TulispContext, TulispObject, destruct_bind};

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.add_special_form("error", |ctx, args| {
        destruct_bind!((msg) = args);
        Err(Error::new(
            ErrorKind::LispError,
            ctx.eval(&msg)?.as_string()?,
        ))
    });

    ctx.add_special_form("catch", |ctx, args| {
        destruct_bind!((tag &rest body) = args);
        let res = ctx.eval_progn(&body);
        if let Err(ref e) = res {
            let tag = ctx.eval(&tag)?;
            if let ErrorKind::Throw(obj) = e.kind_ref() {
                if let Ok(true) = obj.car_and_then(|e_tag| Ok(e_tag.eq(&tag))) {
                    return Ok(obj.cdr()?);
                }
            }
        }
        res
    });

    ctx.add_special_form("throw", |ctx, args| {
        destruct_bind!((tag value) = args);
        Err(Error::new(
            ErrorKind::Throw(TulispObject::cons(ctx.eval(&tag)?, ctx.eval(&value)?)),
            String::new(),
        ))
    });
}
