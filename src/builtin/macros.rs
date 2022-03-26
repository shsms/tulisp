use proc_macros::crate_fn_no_eval;

use crate::cons::{car, cdr, Cons};
use crate::context::{ContextObject, TulispContext};
use crate::error::Error;
use crate::value::TulispValue::{self, Nil};
use crate::value_ref::TulispValueRef;
use crate::{destruct_bind, list};

fn thread_first(ctx: &mut TulispContext, vv: TulispValueRef) -> Result<TulispValueRef, Error> {
    destruct_bind!((_name x &optional form &rest more) = vv);
    if form.is_null() {
        Ok(x.clone())
    } else if more.is_null() {
        if form.is_cons() {
            Ok(list!(,car(form.clone())? ,x.clone() ,@cdr(form.clone())?)?)
        } else {
            Ok(list!(,form.clone() ,x.clone())?)
        }
    } else {
        let inner = thread_first(ctx, list!(,Nil.into_ref() ,x.clone() ,form.clone())?)?;
        thread_first(ctx, list!(,Nil.into_ref() ,inner ,@more.clone())?)
    }
}

fn thread_last(ctx: &mut TulispContext, vv: TulispValueRef) -> Result<TulispValueRef, Error> {
    destruct_bind!((_name x &optional form &rest more) = vv);
    if form.is_null() {
        Ok(x.clone())
    } else if more.is_null() {
        if form.is_cons() {
            Ok(list!(,@form.clone() ,x.clone())?)
        } else {
            Ok(list!(,form.clone() ,x.clone())?)
        }
    } else {
        let inner = thread_last(ctx, list!(,Nil.into_ref() ,x.clone() ,form.clone())?)?;
        thread_last(ctx, list!(,Nil.into_ref() ,inner ,@more.clone())?)
    }
}

#[crate_fn_no_eval]
fn let_star(
    ctx: &mut TulispContext,
    varlist: TulispValueRef,
    rest: TulispValueRef,
) -> Result<TulispValueRef, Error> {
    fn unwrap_varlist(
        ctx: &mut TulispContext,
        varlist: TulispValueRef,
        body: TulispValueRef,
    ) -> Result<TulispValueRef, Error> {
        destruct_bind!((nextvar &rest rest) = varlist);

        let mut ret = Cons::new();
        ret.push(TulispValue::ident_from("let".to_string(), None).into_ref())?
            .push(list!(,nextvar)?)?;
        if rest != TulispValue::Nil {
            ret.push(unwrap_varlist(ctx, rest, body)?)?;
        } else {
            for ele in body.iter() {
                ret.push(ele.clone())?;
            }
        }
        Ok(TulispValue::List {
            cons: ret,
            ctxobj: ctx.get_str(&"let".to_string()),
            span: None,
        }
        .into_ref())
    }
    unwrap_varlist(ctx, varlist, rest)
}

pub fn add(ctx: &mut TulispContext) {
    // TODO: crate_fn needs to implement Optional, before threading macros can be transitioned.
    ctx.set_str("->".to_string(), ContextObject::Macro(thread_first))
        .unwrap();
    ctx.set_str(
        "thread-first".to_string(),
        ContextObject::Macro(thread_first),
    )
    .unwrap();

    ctx.set_str("->>".to_string(), ContextObject::Macro(thread_last))
        .unwrap();
    ctx.set_str("thread-last".to_string(), ContextObject::Macro(thread_last))
        .unwrap();

    ctx.set_str("let*".to_string(), ContextObject::Macro(let_star))
        .unwrap();
}
