use std::rc::Rc;

use crate::ErrorKind;
use crate::TulispObject;
use crate::TulispValue;
use crate::context::TulispContext;
use crate::error::Error;
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

fn quote(_ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
    if !args.consp() {
        return Err(Error::new(
            ErrorKind::TypeMismatch,
            "quote: expected one argument".to_string(),
        ));
    }
    args.cdr_and_then(|cdr| {
        if !cdr.null() {
            return Err(Error::new(
                ErrorKind::TypeMismatch,
                "quote: expected one argument".to_string(),
            ));
        }
        Ok(())
    })?;
    let arg = args.car()?;
    Ok(TulispValue::Quote { value: arg }.into_ref(None))
}

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.intern("->")
        .set(TulispValue::Macro(Rc::new(thread_first)).into_ref(None))
        .unwrap();
    ctx.intern("thread-first")
        .set(TulispValue::Macro(Rc::new(thread_first)).into_ref(None))
        .unwrap();
    ctx.intern("->>")
        .set(TulispValue::Macro(Rc::new(thread_last)).into_ref(None))
        .unwrap();
    ctx.intern("thread-last")
        .set(TulispValue::Macro(Rc::new(thread_last)).into_ref(None))
        .unwrap();
    ctx.intern("quote")
        .set(TulispValue::Macro(Rc::new(quote)).into_ref(None))
        .unwrap();
}
