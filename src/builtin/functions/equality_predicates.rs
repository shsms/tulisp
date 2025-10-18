use crate::{Error, ErrorKind, TulispContext, TulispObject, destruct_eval_bind};

pub(crate) fn add(ctx: &mut TulispContext) {
    fn equal(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
        destruct_eval_bind!(ctx, (object1 object2) = args);
        Ok(object1.equal(&object2).into())
    }
    ctx.add_special_form("equal", equal);

    fn eq(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
        destruct_eval_bind!(ctx, (object1 object2) = args);
        Ok(object1.eq(&object2).into())
    }
    ctx.add_special_form("eq", eq);
}
