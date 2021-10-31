use std::cell::RefCell;
use std::rc::Rc;

use crate::builtin::functions::defun_args;
use crate::builtin::functions::list;
use crate::cons::{car, cdr, Cons};
use crate::context::{ContextObject, Scope, TulispContext};
use crate::value::TulispValue::{self, Nil};
use crate::Error;

fn thread_first(ctx: &mut TulispContext, vv: &TulispValue) -> Result<TulispValue, Error> {
    defun_args!(_name (x &optional form &rest more) = vv);
    if form.is_null() {
        Ok(x.clone())
    } else if more.is_null() {
        if form.is_list() {
            Ok(list!(,car(form)?.clone() ,x.clone() ,@cdr(form)?.clone()))
        } else {
            Ok(list!(,form.clone() ,x.clone()))
        }
    } else {
        let inner = thread_first(ctx, &list!(Nil ,x.clone() ,form.clone()))?;
        thread_first(ctx, &list!(Nil ,inner ,@more.clone()))
    }
}

fn thread_last(ctx: &mut TulispContext, vv: &TulispValue) -> Result<TulispValue, Error> {
    defun_args!(_name (x &optional form &rest more) = vv);
    if form.is_null() {
        Ok(x.clone())
    } else if more.is_null() {
        if form.is_list() {
            Ok(list!(,@form.clone() ,x.clone()))
        } else {
            Ok(list!(,form.clone() ,x.clone()))
        }
    } else {
        let inner = thread_last(ctx, &list!(Nil ,x.clone() ,form.clone()))?;
        thread_last(ctx, &list!(Nil ,inner ,@more.clone()))
    }
}

pub fn add(ctx: &mut Scope) {
    ctx.insert(
        "->".to_string(),
        Rc::new(RefCell::new(ContextObject::Macro(thread_first))),
    );
    ctx.insert(
        "thread-first".to_string(),
        Rc::new(RefCell::new(ContextObject::Macro(thread_first))),
    );

    ctx.insert(
        "->>".to_string(),
        Rc::new(RefCell::new(ContextObject::Macro(thread_last))),
    );
    ctx.insert(
        "thread-last".to_string(),
        Rc::new(RefCell::new(ContextObject::Macro(thread_last))),
    );

    ctx.insert(
        "let*".to_string(),
        Rc::new(RefCell::new(ContextObject::Macro(|ctx, vv| {
            defun_args!(_name (varlist &rest body) = vv);
            fn unwrap_varlist(
                ctx: &mut TulispContext,
                varlist: &TulispValue,
                body: &TulispValue,
            ) -> Result<TulispValue, Error> {
                defun_args!((nextvar &rest rest) = varlist);

                let mut ret = Cons::new();
                ret.push(TulispValue::Ident("let".to_string()))?;
                ret.push(nextvar.clone().into_list())?;
                if *rest != TulispValue::Nil {
                    ret.push(unwrap_varlist(ctx, rest, body)?)?;
                } else {
                    for ele in body.iter() {
                        ret.push(ele.clone())?;
                    }
                }
                Ok(TulispValue::SExp {
                    cons: Box::new(ret),
                    ctxobj: ctx.get_str(&"let".to_string()),
                    span: None,
                })
            }
            unwrap_varlist(ctx, varlist, body)
        }))),
    );
}
