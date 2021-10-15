use std::cell::RefCell;
use std::rc::Rc;

use crate::cons::{car, cdr, Cons};
use crate::context::{ContextObject, Scope, TulispContext};
use crate::value::TulispValue;
use crate::Error;

pub fn add(ctx: &mut Scope) {
    ctx.insert(
        "defmacro".to_string(),
        Rc::new(RefCell::new(ContextObject::Macro(|ctx, vv| {
            let name = match car(&vv) {
                Ok(nn) => nn.clone().as_ident()?,
                Err(_) => return Err(Error::Undefined("defmacro with no name".to_string())),
            };
            let vv = cdr(&vv)?;
            let args = match car(&vv) {
                Ok(aa @ TulispValue::SExp(_, _)) => aa.clone(),
                _ => TulispValue::Nil,
            };
            let body = cdr(vv)?.clone();
            ctx.set_str(&name, ContextObject::Defmacro { args, body })?;
            Ok(TulispValue::Nil)
        }))),
    );

    ctx.insert(
        "let*".to_string(),
        Rc::new(RefCell::new(ContextObject::Macro(|ctx, vv| {
            let varlist = car(&vv)?;
            let body = cdr(&vv)?;
            fn unwrap_varlist(
                ctx: &mut TulispContext,
                varlist: &TulispValue,
                body: &TulispValue,
            ) -> Result<TulispValue, Error> {
                let nextvar = car(&varlist)?;
                let rest = cdr(&varlist)?;

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
                Ok(TulispValue::SExp(Box::new(ret),  ctx.get_str(&"let".to_string())))
            }
            unwrap_varlist(ctx, varlist, body)
        }))),
    );
}
