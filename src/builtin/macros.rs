use proc_macros::{crate_add_macro, crate_fn_no_eval};

use crate::cons::Cons;
use crate::context::{ContextObject, TulispContext};
use crate::error::Error;
use crate::value::TulispValue;
use crate::value_enum::TulispValueEnum;
use crate::{destruct_bind, list};

fn thread_first(ctx: &mut TulispContext, vv: &TulispValue) -> Result<TulispValue, Error> {
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
        let inner = thread_first(ctx, &list!(,TulispValue::nil() ,x.clone() ,form.clone())?)?;
        thread_first(ctx, &list!(,TulispValue::nil() ,inner ,@more.clone())?)
    }
}

fn thread_last(ctx: &mut TulispContext, vv: &TulispValue) -> Result<TulispValue, Error> {
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
        let inner = thread_last(ctx, &list!(,TulispValue::nil() ,x.clone() ,form.clone())?)?;
        thread_last(ctx, &list!(,TulispValue::nil() ,inner ,@more.clone())?)
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

        let mut ret = Cons::new(TulispValue::symbol("let".to_string()), TulispValue::nil());
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
            ctxobj: ctx.get_str("let"),
        }
        .into_ref())
    }
    unwrap_varlist(ctx, varlist, rest)
}

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.set_str(
        "->".to_string(),
        ContextObject::Macro(Box::new(thread_first)),
    )
    .unwrap();
    ctx.set_str(
        "thread-first".to_string(),
        ContextObject::Macro(Box::new(thread_first)),
    )
    .unwrap();

    ctx.set_str(
        "->>".to_string(),
        ContextObject::Macro(Box::new(thread_last)),
    )
    .unwrap();
    ctx.set_str(
        "thread-last".to_string(),
        ContextObject::Macro(Box::new(thread_last)),
    )
    .unwrap();

    crate_add_macro!(ctx, let_star, "let*");
}
