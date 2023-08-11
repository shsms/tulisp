use crate::{
    destruct_bind,
    eval::{eval_basic, eval_check_null},
    list, Error, TulispContext, TulispObject, TulispValue,
};
use std::rc::Rc;
use tulisp_proc_macros::crate_fn_no_eval;

macro_rules! intern_set_func {
    ($ctx:ident, $func: ident, $name: literal) => {
        $ctx.intern($name)
            .set_scope(TulispValue::Func(Rc::new($func)).into())
            .unwrap();
    };
}

pub(crate) fn add(ctx: &mut TulispContext) {
    fn impl_if(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
        destruct_bind!((condition then_body &rest body) = args);
        if eval_check_null(ctx, &condition)? {
            ctx.eval_progn(&body)
        } else {
            let mut result = None;
            eval_basic(ctx, &then_body, &mut result)?;
            if let Some(result) = result {
                Ok(result)
            } else {
                Ok(then_body)
            }
        }
    }
    intern_set_func!(ctx, impl_if, "if");

    #[crate_fn_no_eval(add_macro = "ctx")]
    fn when(
        ctx: &mut TulispContext,
        cond: TulispObject,
        rest: TulispObject,
    ) -> Result<TulispObject, Error> {
        list!(,ctx.intern("if") ,cond ,TulispObject::cons(ctx.intern("progn"), rest))
    }

    #[crate_fn_no_eval(add_macro = "ctx")]
    fn unless(ctx: &mut TulispContext, cond: TulispObject, rest: TulispObject) -> TulispObject {
        TulispObject::cons(
            ctx.intern("if"),
            TulispObject::cons(cond, TulispObject::cons(TulispObject::nil(), rest)),
        )
    }

    #[crate_fn_no_eval(add_func = "ctx")]
    fn cond(ctx: &mut TulispContext, rest: TulispObject) -> Result<TulispObject, Error> {
        for item in rest.base_iter() {
            destruct_bind!((condition &rest body) = item);
            if !eval_check_null(ctx, &condition)? {
                return ctx.eval_progn(&body);
            }
        }
        Ok(TulispObject::nil())
    }
}
