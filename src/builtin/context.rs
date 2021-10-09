use crate::Error;
use std::collections::HashMap;
use crate::eval::eval_each;
use crate::context::ContextObject;
use crate::value::TulispValue;
use crate::eval::eval;
use crate::cons::cdr;
use crate::context::TulispContext;
use crate::cons::car;
use crate::cons::Cons;
use std::convert::TryInto;


macro_rules! max_min_ops {
    ($oper:tt) => {{
        |selfobj: TulispValue, other: TulispValue| -> Result<TulispValue, Error> {
            if let TulispValue::Float(s) = selfobj {
                let o: f64 = other.try_into()?;
                Ok(f64::$oper(s, o).into())
            } else if let TulispValue::Float(o) = other {
                let s: f64 = selfobj.try_into()?;
                Ok(f64::$oper(s, o).into())
            } else {
                let s: i64 = selfobj.try_into()?;
                let o: i64 = other.try_into()?;
                Ok(std::cmp::$oper(s, o).into())
            }
        }
    }};
}

macro_rules! binary_ops {
    ($oper:expr) => {{
        |selfobj: TulispValue, other: TulispValue| -> Result<TulispValue, Error> {
            if let TulispValue::Float(s) = selfobj {
                let o: f64 = other.try_into()?;
                Ok($oper(&s, &o).into())
            } else if let TulispValue::Float(o) = other {
                let s: f64 = selfobj.try_into()?;
                Ok($oper(&s, &o).into())
            } else {
                let s: i64 = selfobj.try_into()?;
                let o: i64 = other.try_into()?;
                Ok($oper(&s, &o).into())
            }
        }
    }};
}

fn reduce_with(
    ctx: &mut TulispContext,
    list: TulispValue,
    method: impl Fn(TulispValue, TulispValue) -> Result<TulispValue, Error>,
) -> Result<TulispValue, Error> {
    list.into_iter()
        .map(|x| eval(ctx, &x))
        .reduce(|v1, v2| method(v1?, v2?))
        .unwrap_or(Err(Error::TypeMismatch(
            "Incorrect number of arguments: 0".to_string(),
        )))
}

pub fn make_context() -> TulispContext {
    let mut ctx = HashMap::new();
    ctx.insert(
        "+".to_string(),
        ContextObject::Func(|ctx, vv| reduce_with(ctx, vv, binary_ops!(std::ops::Add::add))),
    );
    ctx.insert(
        "-".to_string(),
        ContextObject::Func(|ctx, vv| reduce_with(ctx, vv, binary_ops!(std::ops::Sub::sub))),
    );
    ctx.insert(
        "*".to_string(),
        ContextObject::Func(|ctx, vv| reduce_with(ctx, vv, binary_ops!(std::ops::Mul::mul))),
    );
    ctx.insert(
        "/".to_string(),
        ContextObject::Func(|ctx, vv| reduce_with(ctx, vv, binary_ops!(std::ops::Div::div))),
    );
    ctx.insert(
        ">".to_string(),
        ContextObject::Func(|ctx, vv| reduce_with(ctx, vv, binary_ops!(std::cmp::PartialOrd::gt))),
    );
    ctx.insert(
        ">=".to_string(),
        ContextObject::Func(|ctx, vv| reduce_with(ctx, vv, binary_ops!(std::cmp::PartialOrd::ge))),
    );
    ctx.insert(
        "<".to_string(),
        ContextObject::Func(|ctx, vv| reduce_with(ctx, vv, binary_ops!(std::cmp::PartialOrd::lt))),
    );
    ctx.insert(
        "<=".to_string(),
        ContextObject::Func(|ctx, vv| reduce_with(ctx, vv, binary_ops!(std::cmp::PartialOrd::le))),
    );
    ctx.insert(
        "equal".to_string(),
        ContextObject::Func(|ctx, vv| reduce_with(ctx, vv, binary_ops!(std::cmp::PartialEq::eq))),
    );
    ctx.insert(
        "max".to_string(),
        ContextObject::Func(|ctx, vv| reduce_with(ctx, vv, max_min_ops!(max))),
    );
    ctx.insert(
        "min".to_string(),
        ContextObject::Func(|ctx, vv| reduce_with(ctx, vv, max_min_ops!(min))),
    );
    ctx.insert(
        "concat".to_string(),
        ContextObject::Func(|ctx, vv| {
            let mut ret = String::new();
            for ele in vv.into_iter() {
                match eval(ctx, &ele)? {
                    TulispValue::String(s) => ret.push_str(&s),
                    _ => return Err(Error::TypeMismatch(format!("Not a string: {:?}", ele))),
                }
            }
            Ok(TulispValue::String(ret))
        }),
    );
    ctx.insert(
        "expt".to_string(),
        ContextObject::Func(|ctx, vv| {
            let base = eval(ctx, car(&vv)?)?;
            let rest = cdr(&vv)?;
            let pow = eval(ctx, car(&rest)?)?;
            Ok(f64::powf(base.try_into()?, pow.clone().try_into()?).into())
        }),
    );
    ctx.insert(
        // TODO: make more elisp compatible.
        "print".to_string(),
        ContextObject::Func(|ctx, vv| {
            let mut iter = vv.into_iter();
            let object = iter.next();
            if iter.next().is_some() {
                Err(Error::NotImplemented(
                    "output stream currently not supported".to_string(),
                ))
            } else if let Some(v) = object {
                println!("{}", eval(ctx, &v)?);
                Ok(v)
            } else {
                Err(Error::TypeMismatch(
                    "Incorrect number of arguments: print, 0".to_string(),
                ))
            }
        }),
    );
    ctx.insert(
        "prin1-to-string".to_string(),
        ContextObject::Func(|ctx, vv| {
            Ok(TulispValue::String(
                eval(ctx, car(&vv)?)?.to_string(),
            ))
        }),
    );
    ctx.insert(
        "if".to_string(),
        ContextObject::Func(|ctx, vv| {
            let condition = car(&vv)?;
            let body = cdr(&vv)?;
            let then_body = car(&body)?;
            let else_body = cdr(&body)?;
            if eval(ctx, condition)?.into() {
                eval(ctx, then_body)
            } else {
                eval_each(ctx, else_body)
            }
        }),
    );
    ctx.insert(
        "cond".to_string(),
        ContextObject::Func(|ctx, vv| {
            for item in vv.into_iter() {
                let condition = car(&item)?;
                let value = cdr(&item)?;
                if eval(ctx, condition)?.into() {
                    return eval_each(ctx, value);
                }
            }
            Ok(TulispValue::Nil)
        }),
    );
    ctx.insert(
        "while".to_string(),
        ContextObject::Func(|ctx, vv| {
            let condition = car(&vv)?;
            let body = cdr(&vv)?;
            let mut result = TulispValue::Nil;
            while eval(ctx, condition)?.into() {
                result = eval_each(ctx, body)?;
            }
            Ok(result)
        }),
    );
    ctx.insert(
        "setq".to_string(),
        ContextObject::Func(|ctx, vv| {
            let mut iter = vv.into_iter();
            let name = match iter.next() {
                Some(vv) => vv,
                None => {
                    return Err(Error::TypeMismatch(
                        "Incorrect number of arguments: setq, 0".to_string(),
                    ))
                }
            };
            let value = match iter.next() {
                Some(vv) => eval(ctx, &vv)?,
                None => {
                    return Err(Error::TypeMismatch(
                        "Incorrect number of arguments: setq, 1".to_string(),
                    ))
                }
            };
            ctx.set(&name, value)?;
            // TODO: result from set
            Ok(TulispValue::Nil)
        }),
    );
    ctx.insert(
        "let".to_string(),
        ContextObject::Func(|ctx, vv| {
            // TODO: add cons_destruct method that returns car and cdr
            let car = car(&vv)?.clone();
            let cdr = cdr(&vv)?;
            ctx.r#let(car)?;
            let ret = match cdr {
                vv @ TulispValue::SExp(_) => eval_each(ctx, vv),
                _ => Err(Error::TypeMismatch(
                    "let: expected varlist and body".to_string(),
                )),
            };
            ctx.pop();
            ret
        }),
    );
    ctx.insert(
        "let*".to_string(),
        ContextObject::Func(|ctx, vv| {
            let varlist = car(&vv)?;
            let body = cdr(&vv)?;
            fn unwrap_varlist(
                varlist: TulispValue,
                body: TulispValue,
            ) -> Result<TulispValue, Error> {
                let nextvar = car(&varlist)?;
                let rest = cdr(&varlist)?;

                let mut ret = Cons::new();
                ret.append(TulispValue::Ident("let".to_string()));
                ret.append(nextvar.clone().into_list());
                if *rest != TulispValue::Nil {
                    // TODO: avoid clone
                    ret.append(unwrap_varlist(rest.clone(), body)?);
                } else {
                    for ele in body.into_iter() {
                        ret.append(ele);
                    }
                }
                Ok(TulispValue::SExp(Box::new(ret)))
            }
            eval(ctx, &unwrap_varlist(varlist.clone(), body.clone())?)
        }),
    );
    ctx.insert(
        "defun".to_string(),
        ContextObject::Func(|ctx, vv| {
            let name = match car(&vv) {
                Ok(nn) => nn.clone().into_ident()?,
                Err(_) => return Err(Error::Undefined("Defun with no name".to_string())),
            };
            let vv = cdr(&vv)?;
            let args = match car(&vv) {
                Ok(aa @ TulispValue::SExp(_)) => aa.clone(),
                _ => TulispValue::Nil, // TODO: test defun with no args
            };
            let body = cdr(vv)?;
            ctx.set_str(&name, ContextObject::Defun { args, body: body.clone() })?;
            Ok(TulispValue::Nil)
        }),
    );
    TulispContext::new(ctx)
}
