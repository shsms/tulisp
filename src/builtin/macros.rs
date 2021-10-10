use std::collections::HashMap;

use crate::cons::{car, cdr, Cons};
use crate::context::ContextObject;
use crate::value::TulispValue;
use crate::Error;

pub fn add(ctx: &mut HashMap<String, ContextObject>) {
    ctx.insert(
        "defmacro".to_string(),
        ContextObject::Macro(|ctx, vv| {
            let name = match car(&vv) {
                Ok(nn) => nn.clone().as_ident()?,
                Err(_) => return Err(Error::Undefined("defmacro with no name".to_string())),
            };
            let vv = cdr(&vv)?;
            let args = match car(&vv) {
                Ok(aa @ TulispValue::SExp(_)) => aa.clone(),
                _ => TulispValue::Nil,
            };
            let body = cdr(vv)?.clone();
            ctx.set_str(&name, ContextObject::Defmacro { args, body })?;
            Ok(TulispValue::Nil)
        }),
    );

    ctx.insert(
        "let*".to_string(),
        ContextObject::Macro(|_, vv| {
            let varlist = car(&vv)?;
            let body = cdr(&vv)?;
            fn unwrap_varlist(
                varlist: &TulispValue,
                body: &TulispValue,
            ) -> Result<TulispValue, Error> {
                let nextvar = car(&varlist)?;
                let rest = cdr(&varlist)?;

                let mut ret = Cons::new();
                ret.append(TulispValue::Ident("let".to_string()));
                ret.append(nextvar.clone().into_list());
                if *rest != TulispValue::Uninitialized {
                    ret.append(unwrap_varlist(rest, body)?);
                } else {
                    for ele in body.iter() {
                        ret.append(ele.clone());
                    }
                }
                Ok(TulispValue::SExp(Box::new(ret)))
            }
            unwrap_varlist(varlist, body)
        }),
    );
}
