use std::rc::Rc;

use tulisp_proc_macros::{crate_add_macro, crate_fn_no_eval};

use crate::cons::Cons;
use crate::context::TulispContext;
use crate::error::Error;
use crate::TulispObject;
use crate::TulispValue;
use crate::{destruct_bind, list};

fn thread_first(_ctx: &mut TulispContext, vv: &TulispObject) -> Result<TulispObject, Error> {
    destruct_bind!((x &optional form &rest more) = vv);
    if form.null() {
        Ok(x)
    } else if more.null() {
        if form.consp() {
            Ok(list!(,form.car()? ,x.clone() ,@form.cdr()?)?)
        } else {
            Ok(list!(,form ,x.clone())?)
        }
    } else {
        let inner = thread_first(_ctx, &list!(,x.clone() ,form.clone())?)?;
        thread_first(_ctx, &list!(,inner ,@more.clone())?)
    }
}

fn thread_last(_ctx: &mut TulispContext, vv: &TulispObject) -> Result<TulispObject, Error> {
    destruct_bind!((x &optional form &rest more) = vv);
    if form.null() {
        Ok(x)
    } else if more.null() {
        if form.consp() {
            Ok(list!(,@form ,x.clone())?)
        } else {
            Ok(list!(,form ,x.clone())?)
        }
    } else {
        let inner = thread_last(_ctx, &list!(,x.clone() ,form.clone())?)?;
        thread_last(_ctx, &list!(,inner ,@more.clone())?)
    }
}

#[crate_fn_no_eval]
fn let_star(
    ctx: &mut TulispContext,
    varlist: TulispObject,
    rest: TulispObject,
) -> Result<TulispObject, Error> {
    fn unwrap_varlist(
        ctx: &mut TulispContext,
        varlist: TulispObject,
        body: TulispObject,
    ) -> Result<TulispObject, Error> {
        destruct_bind!((nextvar &rest rest) = varlist);

        let mut ret = Cons::new(ctx.intern("let"), TulispObject::nil());
        ret.push(list!(,nextvar)?)?;
        if !rest.null() {
            ret.push(unwrap_varlist(ctx, rest, body)?)?;
        } else {
            for ele in body.base_iter() {
                ret.push(ele.clone())?;
            }
        }
        Ok(TulispValue::List {
            cons: ret,
            ctxobj: Some(ctx.intern("let").get()?),
        }
        .into_ref())
    }
    unwrap_varlist(ctx, varlist, rest)
}

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.intern("->")
        .set_scope(TulispValue::Macro(Rc::new(thread_first)).into_ref())
        .unwrap();
    ctx.intern("thread-first")
        .set_scope(TulispValue::Macro(Rc::new(thread_first)).into_ref())
        .unwrap();
    ctx.intern("->>")
        .set_scope(TulispValue::Macro(Rc::new(thread_last)).into_ref())
        .unwrap();
    ctx.intern("thread-last")
        .set_scope(TulispValue::Macro(Rc::new(thread_last)).into_ref())
        .unwrap();

    crate_add_macro!(ctx, let_star, "let*");

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
}
