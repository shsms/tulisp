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

macro_rules! list {
    (@push $ret:ident, $item:expr) => {
        $ret.push($item)?;
    };
    (@push $ret:ident, $item:expr, $($items:expr),+) => {
        $ret.push($item)?;
        list!(@push $ret, $($items),+);
    };
    ($($items:expr),+) => {{
	let mut ret = TulispValue::Nil;
        list!(@push ret, $($items),+);
        ret
    }};
}

macro_rules! defun_args {
    (@impl $vv:ident, $var:ident) => {
        let $var = car($vv)?;
        let $vv = cdr($vv)?;
    };
    (@impl $vv:ident, $var:ident $($vars:tt)+) => {
        defun_args!(@impl $vv, $var);
        defun_args!(@impl $vv, $($vars)+);
    };
    (@impl $vv:ident,) => {};
    (@no-rest $vv:ident) => {
        if *$vv != TulispValue::Uninitialized {
            return Err(Error::TypeMismatch("Too many arguments".to_string()));
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
    (let ($($vars:ident)+) = $vv:ident) => {
	defun_args!(@impl $vv, $($vars)+);
        defun_args!(@no-rest $vv);
    };
    (let ($($vars:ident)* &optional $($optvars:ident)+) = $vv:ident) => {
	defun_args!(@impl $vv, $($vars)*);
        defun_args!(@optvar $vv, $($optvars)+);
        defun_args!(@no-rest $vv);
    };
    (let ($($vars:ident)* &rest $rest:ident) = $vv:ident) => {
	defun_args!(@impl $vv, $($vars)*);
        defun_args!(@rest $rest $vv);
    };
    (let ($($vars:ident)* &optional $($optvars:ident)+ &rest $rest:ident) = $vv:ident) => {
	defun_args!(@impl $vv, $($vars)*);
        defun_args!(@optvar $vv, $($optvars)+);
        defun_args!(@rest $rest $vv);
    };
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
            let args = vv;
            defun_args!(let (first &rest rest) = args);
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
        "mod".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(let (dividend divisor) = vv);
            binary_ops!(std::ops::Rem::rem)(eval(ctx, dividend)?, eval(ctx, divisor)?)
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
            defun_args!(let (base pow) = vv);
            Ok(f64::powf(eval(ctx, base)?.try_into()?, eval(ctx, pow)?.try_into()?).into())
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
            defun_args!(let (arg) = vv);
            Ok(TulispValue::String(eval(ctx, arg)?.fmt_string()))
        }))),
    );
    ctx.insert(
        "princ".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            let mut iter = vv.iter();
            let object = iter.next();
            if iter.next().is_some() {
                Err(Error::NotImplemented(
                    "output stream currently not supported".to_string(),
                ))
            } else if let Some(v) = object {
                let ret = eval(ctx, &v)?;
                println!("{}", ret.fmt_string());
                Ok(ret)
            } else {
                Err(Error::TypeMismatch(
                    "Incorrect number of arguments: print, 0".to_string(),
                ))
            }
        }))),
    );
    ctx.insert(
        "if".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(let (condition then_body &rest else_body) = vv);
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
                defun_args!(let (condition &rest body) = item);
                if eval(ctx, condition)?.into() {
                    return eval_each(ctx, body);
                }
            }
            Ok(TulispValue::Nil)
        }))),
    );
    ctx.insert(
        "while".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(let (condition &rest body) = vv);
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
            defun_args!(let (name value) = vv);
            let value = eval(ctx, value)?;
            ctx.set(name, value.clone())?;
            Ok(value)
        }))),
    );
    ctx.insert(
        "let".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(let (varlist &rest body) = vv);
            ctx.r#let(varlist)?;
            let ret = match body {
                vv @ TulispValue::SExp { .. } => eval_each(ctx, vv),
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
            defun_args!(let (name args &rest body) = vv);
            ctx.set_str(
                name.as_ident()?,
                ContextObject::Defun {
                    args: args.clone(),
                    body: body.clone(),
                },
            )?;
            Ok(TulispValue::Nil)
        }))),
    );

    ctx.insert(
        "defmacro".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
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
        "eval".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(let (arg) = vv);
            let arg = eval(ctx, arg)?;
            eval(ctx, &arg)
        }))),
    );

    ctx.insert(
        "macroexpand".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(let (name) = vv);
            let name = eval(ctx, name)?;
            macroexpand(ctx, name)
        }))),
    );

    // List functions
    ctx.insert(
        "car".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(let (name) = vv);
            Ok(car(&eval(ctx, name)?)?.to_owned())
        }))),
    );

    ctx.insert(
        "cdr".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(let (name) = vv);
            Ok(cdr(&eval(ctx, name)?)?.to_owned())
        }))),
    );

    ctx.insert(
        "append".to_string(),
        Rc::new(RefCell::new(ContextObject::Func(|ctx, vv| {
            defun_args!(let (first &rest rest) = vv);
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
            let mut cons = Cons::new();
            for ele in vv.iter() {
                cons.push(eval(ctx, ele)?)?
            }
            Ok(TulispValue::SExp {
                cons: Box::new(cons),
                ctxobj: None,
                span: None,
            })
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
