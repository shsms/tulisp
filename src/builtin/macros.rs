use std::cell::RefCell;
use std::rc::Rc;

use crate::builtin::functions::defun_args;
use crate::builtin::functions::list;
use crate::cons::{car, cdr, Cons};
use crate::context::{ContextObject, Scope, TulispContext};
use crate::value::TulispValue;
use crate::Error;

fn thread_last(ctx: &mut TulispContext, vv: &TulispValue) -> Result<TulispValue, Error> {
    defun_args!(let (x &optional form &rest more) = vv);
    if !form.as_bool() {
        Ok(x.clone())
    } else if !more.as_bool() {
        if form.is_list() {
            Ok(form.clone().into_push(x.clone())?)
        } else {
            Ok(list!(form.clone(), x.clone()))
        }
    } else {
        let inner = thread_last(ctx, &list!(x.clone(), form.clone()))?
            .into_list()
            .into_append(more.clone())?;
        thread_last(ctx, &inner)
    }
}

pub fn add(ctx: &mut Scope) {
    ctx.insert(
        "defmacro".to_string(),
        Rc::new(RefCell::new(ContextObject::Macro(|ctx, vv| {
            defun_args!(let (name args &rest body) = vv);
            ctx.set_str(
                name.as_ident()?,
                ContextObject::Defmacro {
                    args: args.clone(),
                    body: body.clone(),
                },
            )?;
            Ok(TulispValue::Nil)
        }))),
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
            defun_args!(let (varlist &rest body) = vv);
            fn unwrap_varlist(
                ctx: &mut TulispContext,
                varlist: &TulispValue,
                body: &TulispValue,
            ) -> Result<TulispValue, Error> {
                defun_args!(let (nextvar &rest rest) = varlist);

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
                Ok(TulispValue::SExp(
                    Box::new(ret),
                    ctx.get_str(&"let".to_string()),
                ))
            }
            unwrap_varlist(ctx, varlist, body)
        }))),
    );
}
