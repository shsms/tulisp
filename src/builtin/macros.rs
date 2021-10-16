use std::cell::RefCell;
use std::rc::Rc;

use crate::builtin::functions::defun_args;
use crate::cons::{car, cdr, Cons};
use crate::context::{ContextObject, Scope, TulispContext};
use crate::value::TulispValue;
use crate::Error;

pub fn add(ctx: &mut Scope) {
    ctx.insert(
        "defmacro".to_string(),
        Rc::new(RefCell::new(ContextObject::Macro(|ctx, vv| {
            defun_args!(let (name args &rest body) = vv);
            ctx.set_str(
                &name.as_ident()?,
                ContextObject::Defmacro {
                    args: args.clone(),
                    body: body.clone(),
                },
            )?;
            Ok(TulispValue::Nil)
        }))),
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
                if *rest != TulispValue::Uninitialized {
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
