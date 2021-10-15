pub(crate) use crate::cons::car;
use crate::cons::cdr;
use crate::cons::Cons;
use crate::context::ContextObject;
use crate::context::Scope;
use crate::context::TulispContext;
use crate::eval::eval;
use crate::eval::eval_each;
use crate::parser::macroexpand;
use crate::value::TulispValue;
use crate::Error;
use std::cell::RefCell;
use std::convert::TryInto;
use std::rc::Rc;

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
    list: &TulispValue,
    method: impl Fn(TulispValue, TulispValue) -> Result<TulispValue, Error>,
) -> Result<TulispValue, Error> {
    list.iter()
        .map(|x| eval(ctx, x))
        .reduce(|v1, v2| method(v1?, v2?))
        .unwrap_or(Err(Error::TypeMismatch(
            "Incorrect number of arguments: 0".to_string(),
        )))
}

pub fn add(ctx: &mut Scope) {
    ctx.insert(
        "+".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            reduce_with(ctx, vv, binary_ops!(std::ops::Add::add))
        }))),
    );
    ctx.insert(
        "-".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            reduce_with(ctx, vv, binary_ops!(std::ops::Sub::sub))
        }))),
    );
    ctx.insert(
        "*".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            reduce_with(ctx, vv, binary_ops!(std::ops::Mul::mul))
        }))),
    );
    ctx.insert(
        "/".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            reduce_with(ctx, vv, binary_ops!(std::ops::Div::div))
        }))),
    );
    ctx.insert(
        ">".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            reduce_with(ctx, vv, binary_ops!(std::cmp::PartialOrd::gt))
        }))),
    );
    ctx.insert(
        ">=".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            reduce_with(ctx, vv, binary_ops!(std::cmp::PartialOrd::ge))
        }))),
    );
    ctx.insert(
        "<".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            reduce_with(ctx, vv, binary_ops!(std::cmp::PartialOrd::lt))
        }))),
    );
    ctx.insert(
        "<=".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            reduce_with(ctx, vv, binary_ops!(std::cmp::PartialOrd::le))
        }))),
    );
    ctx.insert(
        "equal".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            reduce_with(ctx, vv, binary_ops!(std::cmp::PartialEq::eq))
        }))),
    );
    ctx.insert(
        "max".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            reduce_with(ctx, vv, max_min_ops!(max))
        }))),
    );
    ctx.insert(
        "min".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            reduce_with(ctx, vv, max_min_ops!(min))
        }))),
    );
    ctx.insert(
        "mod".to_string(), // TODO: ensure only 2 args
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            reduce_with(ctx, vv, binary_ops!(std::ops::Rem::rem))
        }))),
    );
    ctx.insert(
        "concat".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            let mut ret = String::new();
            for ele in vv.iter() {
                match eval(ctx, ele)? {
                    TulispValue::String(s) => ret.push_str(&s),
                    _ => return Err(Error::TypeMismatch(format!("Not a string: {:?}", ele))),
                }
            }
            Ok(TulispValue::String(ret))
        }))),
    );
    ctx.insert(
        "expt".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            let base = eval(ctx, car(&vv)?)?;
            let rest = cdr(&vv)?;
            let pow = eval(ctx, car(&rest)?)?;
            Ok(f64::powf(base.try_into()?, pow.try_into()?).into())
        }))),
    );
    ctx.insert(
        // TODO: make more elisp compatible.
        "print".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            let mut iter = vv.iter();
            let object = iter.next();
            if iter.next().is_some() {
                Err(Error::NotImplemented(
                    "output stream currently not supported".to_string(),
                ))
            } else if let Some(v) = object {
                let ret = eval(ctx, &v)?;
                println!("{}", ret);
                Ok(ret)
            } else {
                Err(Error::TypeMismatch(
                    "Incorrect number of arguments: print, 0".to_string(),
                ))
            }
        }))),
    );
    ctx.insert(
        "prin1-to-string".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            Ok(TulispValue::String(eval(ctx, car(&vv)?)?.to_string()))
        }))),
    );
    ctx.insert(
        "if".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            let condition = car(&vv)?;
            let body = cdr(&vv)?;
            let then_body = car(&body)?;
            let else_body = cdr(&body)?;
            if eval(ctx, condition)?.into() {
                eval(ctx, then_body)
            } else {
                eval_each(ctx, else_body)
            }
        }))),
    );
    ctx.insert(
        "cond".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            for item in vv.iter() {
                let condition = car(item)?;
                let value = cdr(item)?;
                if eval(ctx, condition)?.into() {
                    return eval_each(ctx, value);
                }
            }
            Ok(TulispValue::Nil)
        }))),
    );
    ctx.insert(
        "while".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            let condition = car(&vv)?;
            let body = cdr(&vv)?;
            let mut result = TulispValue::Nil;
            while eval(ctx, condition)?.into() {
                result = eval_each(ctx, body)?;
            }
            Ok(result)
        }))),
    );
    ctx.insert(
        "setq".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            let mut iter = vv.iter();
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
            ctx.set(name, value.clone())?;
            Ok(value)
        }))),
    );
    ctx.insert(
        "let".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            // TODO: add cons_destruct method that returns car and cdr
            let car = car(&vv)?;
            let cdr = cdr(&vv)?;
            ctx.r#let(car)?;
            let ret = match cdr {
                vv @ TulispValue::SExp(_, _) => eval_each(ctx, vv),
                _ => Err(Error::TypeMismatch(
                    "let: expected varlist and body".to_string(),
                )),
            };
            ctx.pop();
            ret
        }))),
    );

    ctx.insert(
        "defun".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            let name = match car(&vv) {
                Ok(nn) => nn.clone().as_ident()?,
                Err(_) => return Err(Error::Undefined("defun with no name".to_string())),
            };
            let vv = cdr(&vv)?;
            let args = match car(&vv) {
                Ok(aa @ TulispValue::SExp(_, _)) => aa.clone(),
                _ => TulispValue::Nil,
            };
            let body = cdr(vv)?.clone();
            ctx.set_str(&name, ContextObject::Defun { args, body })?;
            Ok(TulispValue::Nil)
        }))),
    );

    ctx.insert(
        "macroexpand".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            let mut iter = vv.iter();
            let vv = match iter.next() {
                Some(vv) => eval(ctx, vv)?,
                None => {
                    return Err(Error::TypeMismatch(
                        "macroexpand: too few arguments".to_string(),
                    ))
                }
            };
            if iter.next().is_some() {
                return Err(Error::Unimplemented(
                    "macroexpand: environment param is unimplemented".to_string(),
                ));
            }
            macroexpand(ctx, vv)
        }))),
    );

    // List functions
    ctx.insert(
        "car".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            Ok(car(&eval(ctx, car(vv)?)?)?.to_owned())
        }))),
    );

    ctx.insert(
        "cdr".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            Ok(cdr(&eval(ctx, car(vv)?)?)?.to_owned())
        }))),
    );

    ctx.insert(
        "append".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            let mut first = eval(ctx, car(vv)?)?;
            for ele in cdr(vv)?.iter() {
                first.append(eval(ctx, ele)?)?
            }
            Ok(first)
        }))),
    );

    ctx.insert(
        "list".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            let mut cons = Cons::new();
            for ele in vv.iter() {
                cons.push(eval(ctx, ele)?)?
            }
            Ok(TulispValue::SExp(Box::new(cons), None))
        }))),
    );

    /*
    ctx.insert(
        "".to_string(),
        ContextObject::Func(|ctx, vv| {

        })
    );
    */
}
