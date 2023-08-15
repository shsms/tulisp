use crate::{
    destruct_bind,
    eval::{eval_basic, eval_check_null, eval_is_truthy},
    list, Error, TulispContext, TulispObject, TulispValue,
};
use std::rc::Rc;
use tulisp_proc_macros::{crate_fn, crate_fn_no_eval};

pub(crate) fn add(ctx: &mut TulispContext) {
    fn impl_if(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
        if args.car_with(|x| eval_is_truthy(ctx, x))? {
            args.cadr_with(|x| ctx.eval(x))
        } else {
            args.cddr_with(|x| ctx.eval_progn(x))
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

    // Constructs for combining conditions
    #[crate_fn(add_func = "ctx")]
    fn not(condition: TulispObject) -> bool {
        condition.null()
    }

    #[crate_fn_no_eval(add_func = "ctx")]
    fn and(ctx: &mut TulispContext, rest: TulispObject) -> Result<TulispObject, Error> {
        let mut ret = TulispObject::nil();
        for item in rest.base_iter() {
            let mut result = None;
            eval_basic(ctx, &item, &mut result)?;
            if let Some(result) = result {
                if result.null() {
                    return Ok(result);
                }
                ret = result;
            } else {
                if item.null() {
                    return Ok(item);
                }
                ret = item;
            }
        }
        Ok(ret)
    }

    #[crate_fn_no_eval(add_func = "ctx")]
    fn or(ctx: &mut TulispContext, rest: TulispObject) -> Result<TulispObject, Error> {
        for item in rest.base_iter() {
            let mut result = None;
            eval_basic(ctx, &item, &mut result)?;
            if let Some(result) = result {
                if !result.null() {
                    return Ok(result);
                }
            } else {
                if !item.null() {
                    return Ok(item);
                }
            }
        }
        Ok(TulispObject::nil())
    }

    #[crate_fn(add_func = "ctx")]
    fn xor(cond1: TulispObject, cond2: TulispObject) -> TulispObject {
        if cond1.null() {
            cond2
        } else if cond2.null() {
            cond1
        } else {
            TulispObject::nil()
        }
    }
}
