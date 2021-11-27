use crate::cons;
pub(crate) use crate::cons::car;
use crate::cons::cdr;
use crate::cons::Cons;
use crate::context::ContextObject;
use crate::context::Scope;
use crate::context::TulispContext;
use crate::error::Error;
use crate::error::ErrorKind;
use crate::eval::eval;
use crate::eval::eval_progn;
use crate::macros::{defun_args, list};
use crate::parser::macroexpand;
use crate::value::TulispValue;
use crate::value::TulispValueRef;
use std::cell::RefCell;
use std::cmp::Ordering;
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
    list: TulispValueRef,
    method: fn(TulispValue, TulispValue) -> Result<TulispValue, Error>,
) -> Result<TulispValueRef, Error> {
    list.as_ref()
        .borrow()
        .iter()
        .map(|x| eval(ctx, x))
        .reduce(|v1, v2| {
            method(v1?.as_ref().borrow().clone(), v2?.as_ref().borrow().clone())
                .map(|x| x.into_rc_refcell())
        })
        .unwrap_or(Err(Error::new(
            ErrorKind::TypeMismatch,
            "Incorrect number of arguments: 0".to_string(),
        )))
}

fn mark_tail_calls(name: TulispValueRef, body: TulispValueRef) -> Result<TulispValueRef, Error> {
    let mut ret = TulispValue::Nil;
    let mut body_iter = body.as_ref().borrow().iter();
    let mut tail = body_iter.next().unwrap(); // TODO: make safe
    while let Some(next) = body_iter.next() {
        ret.push(tail)?;
        tail = next;
    }
    if !tail.as_ref().borrow().is_list() {
        return Ok(body.clone());
    }
    let span = if let TulispValue::List { span, .. } = tail.as_ref().borrow().clone() {
        span.clone()
    } else {
        return Ok(tail.clone());
    };
    let tail_ident = car(tail.clone())?;
    let tail_name_str = tail_ident.as_ref().borrow().as_ident()?;
    let new_tail = if *tail_ident.as_ref().borrow() == *name.as_ref().borrow() {
        let ret_tail = TulispValue::List {
            cons: Cons::new(),
            ctxobj: None,
            span,
        }
        .append(cdr(tail)?)?
        .to_owned()
        .into_rc_refcell();
        list!(,TulispValue::Ident("list".to_string()).into_rc_refcell()
              ,TulispValue::Bounce.into_rc_refcell()
              ,@ret_tail)?
    } else if tail_name_str == "progn" {
        mark_tail_calls(name, cdr(tail)?)?
    } else if tail_name_str == "if" {
        defun_args!(_ (condition then_body &rest else_body) = tail);
        list!(,tail_ident.clone()
              ,condition.clone()
              ,car(mark_tail_calls(
                  name.clone(),
                  then_body
                      .as_ref()
                      .borrow()
                      .clone()
                      .into_list()
                      .into_rc_refcell()
              )?)?.to_owned()
              ,@mark_tail_calls(name, else_body)?
        )?
    } else if tail_name_str == "cond" {
        defun_args!(_ (&rest conds) = tail);
        let mut ret = list!(,tail_ident.clone())?;
        for cond in conds.as_ref().borrow().iter() {
            defun_args!((condition &rest body) = cond);
            ret = list!(,@ret
                        ,list!(,condition.clone()
                               ,@mark_tail_calls(name.clone(), body)?)?)?;
        }
        ret
    } else {
        tail.clone()
    };
    ret.push(new_tail)?;
    Ok(ret.into_rc_refcell())
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
            let args = vv.clone();
            defun_args!((first &rest rest) = args);
            if rest.as_ref().borrow().is_null() {
                let vv = binary_ops!(std::ops::Sub::sub)(
                    TulispValue::Int(0),
                    eval(ctx, first)?.as_ref().borrow().clone(),
                )?;
                Ok(vv.into_rc_refcell())
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
            let args = vv.clone();
            defun_args!((_first &rest rest) = args);
            for ele in rest.as_ref().borrow().iter() {
                if *ele.as_ref().borrow() == TulispValue::Int(0)
                    || *ele.as_ref().borrow() == TulispValue::Float(0.0)
                {
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
            binary_ops!(std::ops::Rem::rem)(
                eval(ctx, dividend).map(|x| x.as_ref().borrow().clone())?,
                eval(ctx, divisor).map(|x| x.as_ref().borrow().clone())?,
            )
            .map(|x| x.into_rc_refcell())
        }))),
    );
    ctx.insert(
        "concat".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(_ (&rest vv) = vv);
            let mut ret = String::new();
            for ele in vv.as_ref().borrow().iter() {
                match *eval(ctx, ele.clone())?.as_ref().borrow() {
                    TulispValue::String(ref s) => ret.push_str(s),
                    _ => {
                        return Err(Error::new(
                            ErrorKind::TypeMismatch,
                            format!("Not a string: {}", ele.as_ref().borrow()),
                        ))
                    }
                }
            }
            Ok(TulispValue::String(ret).into_rc_refcell())
        }))),
    );
    ctx.insert(
        "expt".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(_ (base pow) = vv);
            Ok(f64::powf(
                eval(ctx, base)
                    .map(|x| x.as_ref().borrow().clone())?
                    .try_into()?,
                eval(ctx, pow)
                    .map(|x| x.as_ref().borrow().clone())?
                    .try_into()?,
            )
            .into())
            .map(|x: TulispValue| x.into_rc_refcell())
        }))),
    );
    ctx.insert(
        // TODO: make more elisp compatible.
        "print".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(_ (&rest vv) = vv);
            let mut iter = vv.as_ref().borrow().iter();
            let object = iter.next();
            if iter.next().is_some() {
                Err(Error::new(
                    ErrorKind::NotImplemented,
                    "output stream currently not supported".to_string(),
                ))
            } else if let Some(v) = object {
                let ret = eval(ctx, v)?;
                println!("{}", ret.as_ref().borrow());
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
            Ok(
                TulispValue::String(eval(ctx, arg)?.as_ref().borrow().fmt_string())
                    .into_rc_refcell(),
            )
        }))),
    );
    ctx.insert(
        "princ".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(_ (&rest vv) = vv);
            let mut iter = vv.as_ref().borrow().iter();
            let object = iter.next();
            if iter.next().is_some() {
                Err(Error::new(
                    ErrorKind::NotImplemented,
                    "output stream currently not supported".to_string(),
                ))
            } else if let Some(v) = object {
                let ret = eval(ctx, v)?;
                println!("{}", ret.as_ref().borrow().fmt_string());
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
            if eval(ctx, condition)?.as_ref().borrow().as_bool() {
                eval(ctx, then_body)
            } else {
                eval_progn(ctx, else_body)
            }
            .map(|x| x)
        }))),
    );
    ctx.insert(
        "cond".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(_ (&rest vv) = vv);
            for item in vv.as_ref().borrow().iter() {
                defun_args!((condition &rest body) = item);
                if eval(ctx, condition)?.as_ref().borrow().as_bool() {
                    return eval_progn(ctx, body);
                }
            }
            Ok(TulispValue::Nil.into_rc_refcell())
        }))),
    );
    ctx.insert(
        "while".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(_ (condition &rest body) = vv);
            let mut result = TulispValue::Nil.into_rc_refcell();
            while eval(ctx, condition.clone())?.as_ref().borrow().as_bool() {
                result = eval_progn(ctx, body.clone())?;
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
            ctx.r#let(&varlist.as_ref().borrow())?;
            let ret = match *body.as_ref().borrow() {
                TulispValue::List { .. } => eval_progn(ctx, body.clone()),
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
            let body = if let TulispValue::String(_) = *car(body.clone())?.as_ref().borrow() {
                cdr(body)?
            } else {
                body
            };
            let body = mark_tail_calls(name.clone(), body).map_err(|e| {
                println!("mark_tail_calls error: {:?}", e);
                e
            })?;
            ctx.set_str(
                name.as_ref().borrow().as_ident()?,
                ContextObject::Defun { args, body },
            )?;
            Ok(TulispValue::Nil.into_rc_refcell())
        }))),
    );

    ctx.insert(
        "defmacro".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(_ (name args &rest body) = vv);
            // TODO: don't discard docstring
            let body = if let TulispValue::String(_) = *car(body.clone())?.as_ref().borrow() {
                cdr(body)?
            } else {
                body
            };
            ctx.set_str(
                name.as_ref().borrow().as_ident()?,
                ContextObject::Defmacro { args, body },
            )?;
            Ok(TulispValue::Nil.into_rc_refcell())
        }))),
    );

    ctx.insert(
        "eval".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(_(arg) = vv);
            let arg = eval(ctx, arg)?;
            eval(ctx, arg)
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
            Ok(car(eval(ctx, name)?)?.to_owned())
        }))),
    );

    ctx.insert(
        "cdr".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(_(name) = vv);
            Ok(cdr(eval(ctx, name)?)?.to_owned())
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
            let first = eval(ctx, first)?;
            for ele in rest.as_ref().borrow().iter() {
                first.as_ref().borrow_mut().append(eval(ctx, ele)?)?;
            }
            Ok(first)
        }))),
    );

    ctx.insert(
        "list".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(_ (&rest vv) = vv);
            let vv = vv.as_ref().borrow();
            let (ctxobj, span) = (vv.ctxobj(), vv.span());
            let mut cons = Cons::new();
            for ele in vv.iter() {
                cons.push(eval(ctx, ele)?)?
            }
            Ok(TulispValue::List { cons, ctxobj, span }.into_rc_refcell())
        }))),
    );

    ctx.insert(
        "sort".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(_ (seq pred) = vv);
            let pred = eval(ctx, pred)?;
            let pred = ctx.get(pred.clone()).ok_or_else(|| {
                Error::new(
                    ErrorKind::Undefined,
                    format!("Unknown predicate: {}", pred.as_ref().borrow()),
                )
            })?;
            let seq = eval(ctx, seq)?;
            let mut vec: Vec<_> = seq.as_ref().borrow().iter().map(|v| v.clone()).collect();
            vec.sort_by(|v1, v2| {
                let vv = list!(,TulispValue::Nil.into_rc_refcell() ,v1.clone() ,v2.clone())
                    .unwrap()
                    .as_ref()
                    .borrow()
                    .to_owned()
                    .with_ctxobj(Some(pred.clone()))
                    .into_rc_refcell();

                if eval(ctx, vv)
                    .unwrap_or(TulispValue::Nil.into_rc_refcell())
                    .as_ref()
                    .borrow()
                    .as_bool()
                {
                    Ordering::Less
                } else {
                    Ordering::Equal
                }
            });
            let ret = vec
                .iter()
                .fold(list!(), |v1, v2| list!(,@v1 ,(*v2).clone()).unwrap());
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
