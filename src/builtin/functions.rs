use crate::cons;
pub(crate) use crate::cons::car;
use crate::cons::cdr;
use crate::cons::Cons;
use crate::context::ContextObject;
use crate::context::Scope;
use crate::context::TulispContext;
use crate::eval::eval;
use crate::eval::eval_progn;
use crate::parser::macroexpand;
use crate::value::TulispValue;
use crate::Error;
use crate::ErrorKind;
use std::cell::RefCell;
use std::cmp::Ordering;
use std::convert::TryInto;
use std::rc::Rc;

macro_rules! list {
    (@push $ret:ident, $item:expr) => {
        $ret.push($item)?;
    };
    (@push $ret:ident, @ $item:expr) => {
        $ret.append($item)?;
    };
    (@push $ret:ident, $item:expr, $($items:tt)+) => {
        list!(@push $ret, $item);
        list!(@push $ret, $($items)+);
    };
    (@push $ret:ident, @ $item:expr, $($items:tt)+) => {
        list!(@push $ret, @ $item);
        list!(@push $ret, $($items)+);
    };
    (, $($items:tt)+) => { list!($($items)+) };
    ($($items:tt)+) => {{
	let mut ret = TulispValue::Nil;
        list!(@push ret, $($items)+);
        ret
    }};
}

macro_rules! defun_args {
    (@reqr $vv:ident, $var:ident) => {
        let $var = car($vv)?;
        let $vv = cdr($vv)?;
    };
    (@reqr $vv:ident, $var:ident $($vars:tt)+) => {
        defun_args!(@reqr $vv, $var);
        defun_args!(@reqr $vv, $($vars)+);
    };
    (@reqr $vv:ident,) => {};
    (@no-rest $vv:ident) => {
        if *$vv != TulispValue::Uninitialized {
            return Err(Error::new(ErrorKind::TypeMismatch,"Too many arguments".to_string(), ));
        }
    };
    (@rest $rest:ident $vv:ident) => {
        let $rest = if *$vv == TulispValue::Uninitialized {
            &TulispValue::NIL
        } else {
            $vv
        };
    };
    (@optvar $vv:ident, $var:ident) => {
        let ($var, $vv) = if *$vv != TulispValue::Uninitialized {
            (car($vv)?, cdr($vv)?)
        } else {
            (&TulispValue::NIL, &TulispValue::UNINITIALIZED)
        };
    };
    (@optvar $vv:ident, $var:ident $($vars:ident)+) => {
        defun_args!(@optvar $vv, $var);
        defun_args!(@optvar $vv, $($vars)+)
    };
    (($($vars:ident)+) = $vv:ident) => {
        defun_args!(@reqr $vv, $($vars)+);
        defun_args!(@no-rest $vv);
    };
    (($($vars:ident)* &optional $($optvars:ident)+) = $vv:ident) => {
	defun_args!(@reqr $vv, $($vars)*);
        defun_args!(@optvar $vv, $($optvars)+);
        defun_args!(@no-rest $vv);
    };
    (($($vars:ident)* &rest $rest:ident) = $vv:ident) => {
	defun_args!(@reqr $vv, $($vars)*);
        defun_args!(@rest $rest $vv);
    };
    (($($vars:ident)* &optional $($optvars:ident)+ &rest $rest:ident) = $vv:ident) => {
	defun_args!(@reqr $vv, $($vars)*);
        defun_args!(@optvar $vv, $($optvars)+);
        defun_args!(@rest $rest $vv);
    };
    (_ ($($rest:tt)*) = $vv:ident) => {
        let $vv = cdr($vv)?;
        defun_args!(($($rest)*) = $vv);
    };
    ($defun_name:ident ($($rest:tt)*) = $vv:ident) => {
        let $defun_name = car($vv)?;
        defun_args!(_ ($($rest)*) = $vv);
    }
}

pub(crate) use defun_args;
pub(crate) use list;

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
    method: fn(TulispValue, TulispValue) -> Result<TulispValue, Error>,
) -> Result<TulispValue, Error> {
    list.iter()
        .map(|x| eval(ctx, x))
        .reduce(|v1, v2| method(v1?, v2?))
        .unwrap_or(Err(Error::new(
            ErrorKind::TypeMismatch,
            "Incorrect number of arguments: 0".to_string(),
        )))
}

fn mark_tail_calls(name: &TulispValue, body: &TulispValue) -> Result<TulispValue, Error> {
    let mut ret = TulispValue::Nil;
    let mut body_iter = body.iter();
    let mut tail = body_iter.next().unwrap(); // TODO: make safe
    while let Some(next) = body_iter.next() {
        ret = ret.into_push(tail.clone())?;
        tail = next;
    }
    if !tail.is_list() {
        return Ok(body.clone());
    }
    let span = if let TulispValue::SExp { span, .. } = tail {
        span.clone()
    } else {
        return Ok(tail.clone());
    };
    let tail_ident = car(tail)?;
    let tail_name_str = tail_ident.as_ident()?;
    let new_tail = if *tail_ident == *name {
        let ret_tail = TulispValue::SExp {
            cons: Box::new(Cons::EMPTY),
            ctxobj: None,
            span,
        }
        .into_append(cdr(tail)?.clone())?;
        TulispValue::Nil
            .into_push(TulispValue::Ident("list".to_string()))?
            .into_push(TulispValue::Bounce)?
            .into_append(ret_tail)?
    } else if tail_name_str == "progn" {
        mark_tail_calls(name, cdr(tail)?)?
    } else if tail_name_str == "if" {
        defun_args!(_ (condition then_body &rest else_body) = tail);
        let ret = TulispValue::Nil;
        ret.into_push(tail_ident.clone())?
            .into_push(condition.clone())?
            .into_push(car(&mark_tail_calls(name, &then_body.clone().into_list())?)?.clone())?
            .into_append(mark_tail_calls(name, else_body)?)?
    } else if tail_name_str == "cond" {
        defun_args!(_ (&rest conds) = tail);
        let mut ret = TulispValue::Nil.into_push(tail_ident.clone())?;
        for cond in conds.iter() {
            defun_args!((condition &rest body) = cond);
            ret.push(
                TulispValue::Nil
                    .into_push(condition.clone())?
                    .into_append(mark_tail_calls(name, body)?)?,
            )?;
        }
        ret
    } else {
        tail.clone()
    };
    let ret = ret.into_push(new_tail)?;
    Ok(ret)
}

pub fn add(ctx: &mut Scope) {
    ctx.insert(
        "+".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(_ (&rest vv) = vv);
            reduce_with(ctx, vv, binary_ops!(std::ops::Add::add))
        }))),
    );
    ctx.insert(
        "-".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(_ (&rest vv) = vv);
            let args = vv;
            defun_args!((first &rest rest) = args);
            if !rest.as_bool() {
                let vv = binary_ops!(std::ops::Sub::sub)(TulispValue::Int(0), eval(ctx, first)?)?;
                Ok(vv)
            } else {
                reduce_with(ctx, vv, binary_ops!(std::ops::Sub::sub))
            }
        }))),
    );
    ctx.insert(
        "*".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(_ (&rest vv) = vv);
            reduce_with(ctx, vv, binary_ops!(std::ops::Mul::mul))
        }))),
    );
    ctx.insert(
        "/".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(_ (&rest vv) = vv);
            let args = vv;
            defun_args!((_first &rest rest) = args);
            for ele in rest.iter() {
                if ele == &TulispValue::Int(0) || ele == &TulispValue::Float(0.0) {
                    return Err(Error::new(
                        ErrorKind::Undefined,
                        "Division by zero".to_string(),
                    ));
                }
            }
            reduce_with(ctx, vv, binary_ops!(std::ops::Div::div))
        }))),
    );
    // TODO: >, >=, <, <=, equal - need to be able to support more than 2 args
    ctx.insert(
        ">".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(_ (&rest vv) = vv);
            reduce_with(ctx, vv, binary_ops!(std::cmp::PartialOrd::gt))
        }))),
    );
    ctx.insert(
        ">=".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(_ (&rest vv) = vv);
            reduce_with(ctx, vv, binary_ops!(std::cmp::PartialOrd::ge))
        }))),
    );
    ctx.insert(
        "<".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(_ (&rest vv) = vv);
            reduce_with(ctx, vv, binary_ops!(std::cmp::PartialOrd::lt))
        }))),
    );
    ctx.insert(
        "<=".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(_ (&rest vv) = vv);
            reduce_with(ctx, vv, binary_ops!(std::cmp::PartialOrd::le))
        }))),
    );
    ctx.insert(
        "equal".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(_ (&rest vv) = vv);
            reduce_with(ctx, vv, binary_ops!(std::cmp::PartialEq::eq))
        }))),
    );
    ctx.insert(
        "max".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(_ (&rest vv) = vv);
            reduce_with(ctx, vv, max_min_ops!(max))
        }))),
    );
    ctx.insert(
        "min".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(_ (&rest vv) = vv);
            reduce_with(ctx, vv, max_min_ops!(min))
        }))),
    );
    ctx.insert(
        "mod".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(_ (dividend divisor) = vv);
            binary_ops!(std::ops::Rem::rem)(eval(ctx, dividend)?, eval(ctx, divisor)?)
        }))),
    );
    ctx.insert(
        "concat".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(_ (&rest vv) = vv);
            let mut ret = String::new();
            for ele in vv.iter() {
                match eval(ctx, ele)? {
                    TulispValue::String(s) => ret.push_str(&s),
                    _ => {
                        return Err(Error::new(
                            ErrorKind::TypeMismatch,
                            format!("Not a string: {}", ele),
                        ))
                    }
                }
            }
            Ok(TulispValue::String(ret))
        }))),
    );
    ctx.insert(
        "expt".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(_ (base pow) = vv);
            Ok(f64::powf(eval(ctx, base)?.try_into()?, eval(ctx, pow)?.try_into()?).into())
        }))),
    );
    ctx.insert(
        // TODO: make more elisp compatible.
        "print".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(_ (&rest vv) = vv);
            let mut iter = vv.iter();
            let object = iter.next();
            if iter.next().is_some() {
                Err(Error::new(
                    ErrorKind::NotImplemented,
                    "output stream currently not supported".to_string(),
                ))
            } else if let Some(v) = object {
                let ret = eval(ctx, &v)?;
                println!("{}", ret);
                Ok(ret)
            } else {
                Err(Error::new(
                    ErrorKind::TypeMismatch,
                    "Incorrect number of arguments: print, 0".to_string(),
                ))
            }
        }))),
    );
    ctx.insert(
        "prin1-to-string".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(_(arg) = vv);
            Ok(TulispValue::String(eval(ctx, arg)?.fmt_string()))
        }))),
    );
    ctx.insert(
        "princ".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(_ (&rest vv) = vv);
            let mut iter = vv.iter();
            let object = iter.next();
            if iter.next().is_some() {
                Err(Error::new(
                    ErrorKind::NotImplemented,
                    "output stream currently not supported".to_string(),
                ))
            } else if let Some(v) = object {
                let ret = eval(ctx, &v)?;
                println!("{}", ret.fmt_string());
                Ok(ret)
            } else {
                Err(Error::new(
                    ErrorKind::TypeMismatch,
                    "Incorrect number of arguments: print, 0".to_string(),
                ))
            }
        }))),
    );
    ctx.insert(
        "if".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(_ (condition then_body &rest else_body) = vv);
            if eval(ctx, condition)?.into() {
                eval(ctx, then_body)
            } else {
                eval_progn(ctx, else_body)
            }
        }))),
    );
    ctx.insert(
        "cond".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(_ (&rest vv) = vv);
            for item in vv.iter() {
                defun_args!((condition &rest body) = item);
                if eval(ctx, condition)?.into() {
                    return eval_progn(ctx, body);
                }
            }
            Ok(TulispValue::Nil)
        }))),
    );
    ctx.insert(
        "while".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(_ (condition &rest body) = vv);
            let mut result = TulispValue::Nil;
            while eval(ctx, condition)?.into() {
                result = eval_progn(ctx, body)?;
            }
            Ok(result)
        }))),
    );
    ctx.insert(
        "setq".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(_ (name value) = vv);
            let value = eval(ctx, value)?;
            ctx.set(name, value.clone())?;
            Ok(value)
        }))),
    );
    ctx.insert(
        "let".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(_ (varlist &rest body) = vv);
            ctx.r#let(varlist)?;
            let ret = match body {
                vv @ TulispValue::SExp { .. } => eval_progn(ctx, vv),
                _ => Err(Error::new(
                    ErrorKind::TypeMismatch,
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
            defun_args!(_ (name args &rest body) = vv);
            // TODO: don't discard docstring
            let body = if let TulispValue::String(_) = car(body)? {
                cdr(body)?
            } else {
                body
            };
            let body = mark_tail_calls(name, body).map_err(|e| {
                println!("mark_tail_calls error: {:?}", e);
                e
            })?;
            ctx.set_str(
                name.as_ident()?,
                ContextObject::Defun {
                    args: args.clone(),
                    body,
                },
            )?;
            Ok(TulispValue::Nil)
        }))),
    );

    ctx.insert(
        "defmacro".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(_ (name args &rest body) = vv);
            // TODO: don't discard docstring
            let body = if let TulispValue::String(_) = car(body)? {
                cdr(body)?
            } else {
                body
            };
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
        "eval".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(_(arg) = vv);
            let arg = eval(ctx, arg)?;
            eval(ctx, &arg)
        }))),
    );

    ctx.insert(
        "macroexpand".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(_(name) = vv);
            let name = eval(ctx, name)?;
            macroexpand(ctx, name)
        }))),
    );

    // List functions
    ctx.insert(
        "car".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(_(name) = vv);
            Ok(car(&eval(ctx, name)?)?.to_owned())
        }))),
    );

    ctx.insert(
        "cdr".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(_(name) = vv);
            Ok(cdr(&eval(ctx, name)?)?.to_owned())
        }))),
    );

    ctx.insert(
        "cons".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(_ (car cdr) = vv);
            let car = eval(ctx, car)?;
            let cdr = eval(ctx, cdr)?;
            Ok(cons::cons(car, cdr))
        }))),
    );

    ctx.insert(
        "append".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(_ (first &rest rest) = vv);
            let mut first = eval(ctx, first)?;
            for ele in rest.iter() {
                first.append(eval(ctx, ele)?)?;
            }
            Ok(first)
        }))),
    );

    ctx.insert(
        "list".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(_ (&rest vv) = vv);
            let (ctxobj, span) = if let TulispValue::SExp { ctxobj, span, .. } = vv {
                (ctxobj.clone(), span.clone())
            } else {
                (None, None)
            };
            let mut cons = Cons::new();
            for ele in vv.iter() {
                cons.push(eval(ctx, ele)?)?
            }
            Ok(TulispValue::SExp {
                cons: Box::new(cons),
                ctxobj,
                span,
            })
        }))),
    );

    ctx.insert(
        "sort".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(_ (seq pred) = vv);
            let pred = eval(ctx, pred)?;
            let pred = ctx.get(&pred).ok_or_else(|| {
                Error::new(ErrorKind::Undefined, format!("Unknown predicate: {}", pred))
            })?;
            let seq = eval(ctx, seq)?;
            let mut vec: Vec<_> = seq.iter().map(|v| v.clone()).collect();
            vec.sort_by(|v1, v2| {
                let vv = TulispValue::Nil
                    .into_push(TulispValue::Nil)
                    .unwrap()
                    .into_push(v1.clone())
                    .unwrap()
                    .into_push(v2.clone())
                    .unwrap()
                    .with_ctxobj(Some(pred.clone()));

                if eval(ctx, &vv).unwrap_or(TulispValue::Nil).as_bool() {
                    Ordering::Less
                } else {
                    Ordering::Equal
                }
            });
            let ret = vec.iter().fold(TulispValue::Nil, |v1, v2| {
                v1.into_push((*v2).clone()).unwrap()
            });
            Ok(ret)
        }))),
    );
    /*
    ctx.insert(
        "".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {

        })))
    );
    */
}
