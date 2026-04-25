use crate::{TulispContext, TulispObject};

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.defun("equal", |a: TulispObject, b: TulispObject| -> bool {
        a.equal(&b)
    });
    ctx.defun("eq", |a: TulispObject, b: TulispObject| -> bool {
        a.eq(&b)
    });
}
