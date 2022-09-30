use std::rc::Rc;

use proc_macros::{crate_add_macro, crate_fn_no_eval};

use crate::cons::Cons;
use crate::context::TulispContext;
use crate::error::Error;
use crate::value::TulispValue;
use crate::value_enum::TulispValueEnum;
use crate::{destruct_bind, list};

fn thread_first(_ctx: &mut TulispContext, vv: &TulispValue) -> Result<TulispValue, Error> {
    destruct_bind!((_name x &optional form &rest more) = vv);
    if form.null() {
        Ok(x)
    } else if more.null() {
        if form.consp() {
            Ok(list!(,form.car()? ,x.clone() ,@form.cdr()?)?)
        } else {
            Ok(list!(,form ,x.clone())?)
        }
    } else {
        let inner = thread_first(_ctx, &list!(,TulispValue::nil() ,x.clone() ,form.clone())?)?;
        thread_first(_ctx, &list!(,TulispValue::nil() ,inner ,@more.clone())?)
    }
}

fn thread_last(_ctx: &mut TulispContext, vv: &TulispValue) -> Result<TulispValue, Error> {
    destruct_bind!((_name x &optional form &rest more) = vv);
    if form.null() {
        Ok(x)
    } else if more.null() {
        if form.consp() {
            Ok(list!(,@form ,x.clone())?)
        } else {
            Ok(list!(,form ,x.clone())?)
        }
    } else {
        let inner = thread_last(_ctx, &list!(,TulispValue::nil() ,x.clone() ,form.clone())?)?;
        thread_last(_ctx, &list!(,TulispValue::nil() ,inner ,@more.clone())?)
    }
}

#[crate_fn_no_eval]
fn let_star(
    ctx: &mut TulispContext,
    varlist: TulispValue,
    rest: TulispValue,
) -> Result<TulispValue, Error> {
    fn unwrap_varlist(
        ctx: &mut TulispContext,
        varlist: TulispValue,
        body: TulispValue,
    ) -> Result<TulispValue, Error> {
        destruct_bind!((nextvar &rest rest) = varlist);

        let mut ret = Cons::new(ctx.intern("let"), TulispValue::nil());
        ret.push(list!(,nextvar)?)?;
        if !rest.null() {
            ret.push(unwrap_varlist(ctx, rest, body)?)?;
        } else {
            for ele in body.base_iter() {
                ret.push(ele.clone())?;
            }
        }
        Ok(TulispValueEnum::List {
            cons: ret,
            ctxobj: Some(ctx.intern("let").get()?),
        }
        .into_ref())
    }
    unwrap_varlist(ctx, varlist, rest)
}

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.intern("->")
        .set_scope(TulispValueEnum::Macro(Rc::new(thread_first)).into_ref())
        .unwrap();
    ctx.intern("thread-first")
        .set_scope(TulispValueEnum::Macro(Rc::new(thread_first)).into_ref())
        .unwrap();
    ctx.intern("->>")
        .set_scope(TulispValueEnum::Macro(Rc::new(thread_last)).into_ref())
        .unwrap();
    ctx.intern("thread-last")
        .set_scope(TulispValueEnum::Macro(Rc::new(thread_last)).into_ref())
        .unwrap();

    crate_add_macro!(ctx, let_star, "let*");
}
