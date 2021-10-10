use std::collections::HashMap;

use crate::cons::{car, cdr, Cons};
use crate::context::{ContextObject, TulispContext};
use crate::value::TulispValue;
use crate::Error;

pub fn macro_context() -> TulispContext {
    let mut ctx = HashMap::new();

    ctx.insert(
        "let*".to_string(),
        ContextObject::Func(|_, vv| {
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

    TulispContext::new(ctx)
}
