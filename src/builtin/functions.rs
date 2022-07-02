use crate::cons::Cons;
use crate::context::ContextObject;
use crate::context::TulispContext;
use crate::error::Error;
use crate::error::ErrorKind;
use crate::eval::eval;
use crate::value::TulispValue;
use crate::value_ref::TulispValueRef;
use crate::{destruct_bind, list};
use proc_macros::crate_fn;
use proc_macros::crate_fn_no_eval;
use std::cmp::Ordering;
use std::convert::TryInto;

macro_rules! max_min_ops {
    ($oper:tt) => {{
        |selfobj: TulispValueRef, other: TulispValueRef| -> Result<TulispValueRef, Error> {
            if let Ok(s) = selfobj.as_float() {
                let o: f64 = other.try_into()?;
                Ok(f64::$oper(s, o).into())
            } else if let Ok(o) = other.as_float() {
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
        |selfobj: TulispValueRef, other: TulispValueRef| -> Result<TulispValueRef, Error> {
            if let Ok(s) = selfobj.as_float() {
                let o: f64 = other.try_into()?;
                Ok($oper(&s, &o).into())
            } else if let Ok(o) = other.as_float() {
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
    method: fn(TulispValueRef, TulispValueRef) -> Result<TulispValueRef, Error>,
) -> Result<TulispValueRef, Error> {
    list.base_iter()
        .map(|x| eval(ctx, &x))
        .reduce(|v1, v2| method(v1?, v2?))
        .unwrap_or_else(|| {
            Err(Error::new(
                ErrorKind::TypeMismatch,
                "Incorrect number of arguments: 0".to_string(),
            ))
        })
}

fn mark_tail_calls(name: TulispValueRef, body: TulispValueRef) -> Result<TulispValueRef, Error> {
    let mut ret = TulispValue::Nil;
    let mut body_iter = body.base_iter();
    let mut tail = body_iter.next().unwrap(); // TODO: make safe
    for next in body_iter {
        ret.push(tail)?;
        tail = next;
    }
    if !tail.is_cons() {
        return Ok(body);
    }
    let span = tail.span();
    let tail_ident = tail.car()?;
    let tail_name_str = tail_ident.as_symbol()?;
    let new_tail = if tail_ident == name {
        let ret_tail = TulispValue::Nil
            .into_ref()
            .append(tail.cdr()?)?
            .to_owned()
            .with_span(span);
        list!(,TulispValue::symbol_from("list".to_string(),  None).into_ref()
              ,TulispValue::Bounce.into_ref() ,@ret_tail)?
    } else if tail_name_str == "progn" || tail_name_str == "let" || tail_name_str == "let*" {
        list!(,tail_ident ,@mark_tail_calls(name, tail.cdr()?)?)?
    } else if tail_name_str == "if" {
        destruct_bind!((_if condition then_body &rest else_body) = tail);
        list!(,tail_ident
              ,condition.clone()
              ,mark_tail_calls(
                  name.clone(),
                  list!(,then_body)?
              )?.car()?
              ,@mark_tail_calls(name, else_body)?
        )?
    } else if tail_name_str == "cond" {
        destruct_bind!((_cond &rest conds) = tail);
        let mut ret = list!(,tail_ident)?;
        for cond in conds.base_iter() {
            destruct_bind!((condition &rest body) = cond);
            ret = list!(,@ret
                        ,list!(,condition.clone()
                               ,@mark_tail_calls(name.clone(), body)?)?)?;
        }
        ret
    } else {
        tail
    };
    ret.push(new_tail)?;
    Ok(ret.into_ref())
}

pub(crate) fn add(ctx: &mut TulispContext) {
    #[crate_fn_no_eval(add_func = "ctx", name = "+")]
    fn add(ctx: &mut TulispContext, rest: TulispValueRef) -> Result<TulispValueRef, Error> {
        reduce_with(ctx, rest, binary_ops!(std::ops::Add::add))
    }

    #[crate_fn_no_eval(add_func = "ctx", name = "-")]
    fn sub(ctx: &mut TulispContext, rest: TulispValueRef) -> Result<TulispValueRef, Error> {
        let args = rest.clone();
        destruct_bind!((first &rest ohne_first) = args);
        if ohne_first.is_null() {
            let vv = binary_ops!(std::ops::Sub::sub)(0.into(), eval(ctx, &first)?)?;
            Ok(vv)
        } else {
            reduce_with(ctx, rest, binary_ops!(std::ops::Sub::sub))
        }
    }

    #[crate_fn_no_eval(add_func = "ctx", name = "*")]
    fn mul(ctx: &mut TulispContext, rest: TulispValueRef) -> Result<TulispValueRef, Error> {
        reduce_with(ctx, rest, binary_ops!(std::ops::Mul::mul))
    }

    #[crate_fn_no_eval(add_func = "ctx", name = "/")]
    fn div(ctx: &mut TulispContext, rest: TulispValueRef) -> Result<TulispValueRef, Error> {
        let args = rest.clone();
        destruct_bind!((_first &rest ohne_first) = args);
        for ele in ohne_first.base_iter() {
            if ele == TulispValue::from(0) || ele == TulispValue::from(0.0) {
                return Err(Error::new(
                    ErrorKind::Undefined,
                    "Division by zero".to_string(),
                ));
            }
        }
        reduce_with(ctx, rest, binary_ops!(std::ops::Div::div))
    }

    // // TODO: >, >=, <, <=, equal - need to be able to support more than 2 args
    #[crate_fn_no_eval(add_func = "ctx", name = ">")]
    fn gt(ctx: &mut TulispContext, rest: TulispValueRef) -> Result<TulispValueRef, Error> {
        reduce_with(ctx, rest, binary_ops!(std::cmp::PartialOrd::gt))
    }

    #[crate_fn_no_eval(add_func = "ctx", name = ">=")]
    fn ge(ctx: &mut TulispContext, rest: TulispValueRef) -> Result<TulispValueRef, Error> {
        reduce_with(ctx, rest, binary_ops!(std::cmp::PartialOrd::ge))
    }

    #[crate_fn_no_eval(add_func = "ctx", name = "<")]
    fn lt(ctx: &mut TulispContext, rest: TulispValueRef) -> Result<TulispValueRef, Error> {
        reduce_with(ctx, rest, binary_ops!(std::cmp::PartialOrd::lt))
    }

    #[crate_fn_no_eval(add_func = "ctx", name = "<=")]
    fn le(ctx: &mut TulispContext, rest: TulispValueRef) -> Result<TulispValueRef, Error> {
        reduce_with(ctx, rest, binary_ops!(std::cmp::PartialOrd::le))
    }

    #[crate_fn_no_eval(add_func = "ctx")]
    fn equal(ctx: &mut TulispContext, rest: TulispValueRef) -> Result<TulispValueRef, Error> {
        reduce_with(ctx, rest, binary_ops!(std::cmp::PartialEq::eq))
    }
    #[crate_fn_no_eval(add_func = "ctx")]
    fn max(ctx: &mut TulispContext, rest: TulispValueRef) -> Result<TulispValueRef, Error> {
        reduce_with(ctx, rest, max_min_ops!(max))
    }
    #[crate_fn_no_eval(add_func = "ctx")]
    fn min(ctx: &mut TulispContext, rest: TulispValueRef) -> Result<TulispValueRef, Error> {
        reduce_with(ctx, rest, max_min_ops!(min))
    }

    #[crate_fn(add_func = "ctx", name = "mod")]
    fn impl_mod(
        dividend: TulispValueRef,
        divisor: TulispValueRef,
    ) -> Result<TulispValueRef, Error> {
        binary_ops!(std::ops::Rem::rem)(dividend, divisor)
    }

    #[crate_fn(add_func = "ctx")]
    fn expt(base: TulispValueRef, pow: TulispValueRef) -> Result<TulispValueRef, Error> {
        Ok(f64::powf(base.try_into()?, pow.try_into()?).into())
    }

    #[crate_fn_no_eval(add_func = "ctx")]
    fn concat(ctx: &mut TulispContext, rest: TulispValueRef) -> Result<TulispValueRef, Error> {
        let mut ret = String::new();
        for ele in rest.base_iter() {
            match eval(ctx, &ele)?.as_string() {
                Ok(ref s) => ret.push_str(s),
                _ => {
                    return Err(Error::new(
                        ErrorKind::TypeMismatch,
                        format!("Not a string: {}", ele),
                    ))
                }
            }
        }
        Ok(TulispValue::from(ret).into_ref())
    }

    #[crate_fn(add_func = "ctx")]
    fn print(val: TulispValueRef) -> Result<TulispValueRef, Error> {
        println!("{}", val);
        Ok(val)
    }

    #[crate_fn(add_func = "ctx", name = "prin1-to-string")]
    fn prin1_to_string(arg: TulispValueRef) -> Result<TulispValueRef, Error> {
        Ok(TulispValue::from(arg.fmt_string()).into_ref())
    }

    #[crate_fn(add_func = "ctx")]
    fn princ(val: TulispValueRef) -> Result<TulispValueRef, Error> {
        println!("{}", val.fmt_string());
        Ok(val)
    }

    #[crate_fn_no_eval(add_func = "ctx", name = "if")]
    fn impl_if(
        ctx: &mut TulispContext,
        condition: TulispValueRef,
        then_body: TulispValueRef,
        rest: TulispValueRef, // else_body
    ) -> Result<TulispValueRef, Error> {
        if eval(ctx, &condition)?.as_bool() {
            eval(ctx, &then_body)
        } else {
            ctx.eval_progn(&rest)
        }
    }

    #[crate_fn_no_eval(add_func = "ctx")]
    fn cond(ctx: &mut TulispContext, rest: TulispValueRef) -> Result<TulispValueRef, Error> {
        for item in rest.base_iter() {
            destruct_bind!((condition &rest body) = item);
            if eval(ctx, &condition)?.as_bool() {
                return ctx.eval_progn(&body);
            }
        }
        Ok(TulispValue::Nil.into_ref())
    }

    #[crate_fn_no_eval(add_func = "ctx", name = "while")]
    fn impl_while(
        ctx: &mut TulispContext,
        condition: TulispValueRef,
        rest: TulispValueRef,
    ) -> Result<TulispValueRef, Error> {
        let mut result = TulispValue::Nil.into_ref();
        while eval(ctx, &condition)?.as_bool() {
            result = ctx.eval_progn(&rest)?;
        }
        Ok(result)
    }

    #[crate_fn_no_eval(add_func = "ctx")]
    fn setq(
        ctx: &mut TulispContext,
        name: TulispValueRef,
        value: TulispValueRef,
    ) -> Result<TulispValueRef, Error> {
        let value = eval(ctx, &value)?;
        ctx.set(name, value.clone())?;
        Ok(value)
    }

    #[crate_fn_no_eval(add_func = "ctx", name = "let")]
    fn impl_let(
        ctx: &mut TulispContext,
        varlist: TulispValueRef,
        rest: TulispValueRef,
    ) -> Result<TulispValueRef, Error> {
        ctx.r#let(varlist)?;
        let ret = if rest.is_cons() {
            ctx.eval_progn(&rest)
        } else {
            Err(Error::new(
                ErrorKind::TypeMismatch,
                "let: expected varlist and body".to_string(),
            ))
        };
        ctx.pop();
        ret
    }

    #[crate_fn_no_eval(add_func = "ctx")]
    fn progn(ctx: &mut TulispContext, rest: TulispValueRef) -> Result<TulispValueRef, Error> {
        ctx.eval_progn(&rest)
    }

    #[crate_fn_no_eval(add_func = "ctx")]
    fn defun(
        ctx: &mut TulispContext,
        name: TulispValueRef,
        args: TulispValueRef,
        rest: TulispValueRef,
    ) -> Result<TulispValueRef, Error> {
        // TODO: don't discard docstring
        let body = if rest.car()?.as_string().is_ok() {
            rest.cdr()?
        } else {
            rest
        };
        let body = mark_tail_calls(name.clone(), body).map_err(|e| {
            println!("mark_tail_calls error: {:?}", e);
            e
        })?;
        ctx.set_str(name.as_symbol()?, ContextObject::Defun { args, body })?;
        Ok(TulispValue::Nil.into_ref())
    }

    #[crate_fn_no_eval(add_func = "ctx")]
    fn defmacro(
        ctx: &mut TulispContext,
        name: TulispValueRef,
        args: TulispValueRef,
        rest: TulispValueRef,
    ) -> Result<TulispValueRef, Error> {
        // TODO: don't discard docstring
        let body = if rest.car()?.as_string().is_ok() {
            rest.cdr()?
        } else {
            rest
        };
        ctx.set_str(name.as_symbol()?, ContextObject::Defmacro { args, body })?;
        Ok(TulispValue::Nil.into_ref())
    }

    #[crate_fn(add_func = "ctx")]
    fn null(arg: TulispValueRef) -> bool {
        arg.is_null()
    }

    #[crate_fn(add_func = "ctx", name = "eval")]
    fn impl_eval(ctx: &mut TulispContext, arg: TulispValueRef) -> Result<TulispValueRef, Error> {
        crate::eval::eval(ctx, &arg)
    }

    #[crate_fn(add_func = "ctx", name = "macroexpand")]
    fn impl_macroexpand(
        ctx: &mut TulispContext,
        name: TulispValueRef,
    ) -> Result<TulispValueRef, Error> {
        crate::parser::macroexpand(ctx, name)
    }

    // List functions

    #[crate_fn(add_func = "ctx", name = "car")]
    fn impl_car(name: TulispValueRef) -> Result<TulispValueRef, Error> {
        name.car()
    }

    #[crate_fn(add_func = "ctx", name = "cdr")]
    fn impl_cdr(name: TulispValueRef) -> Result<TulispValueRef, Error> {
        name.cdr()
    }

    #[crate_fn(add_func = "ctx", name = "cons")]
    fn impl_cons(car: TulispValueRef, cdr: TulispValueRef) -> TulispValueRef {
        TulispValueRef::from_cons(car, cdr)
    }

    #[crate_fn_no_eval(add_func = "ctx")]
    fn append(
        ctx: &mut TulispContext,
        first: TulispValueRef,
        rest: TulispValueRef,
    ) -> Result<TulispValueRef, Error> {
        let first = eval(ctx, &first)?;
        for ele in rest.base_iter() {
            first.append(eval(ctx, &ele)?.deep_copy()?)?;
        }
        Ok(first)
    }

    #[crate_fn_no_eval(add_func = "ctx")]
    fn dolist(
        ctx: &mut TulispContext,
        spec: TulispValueRef,
        rest: TulispValueRef,
    ) -> Result<TulispValueRef, Error> {
        destruct_bind!((var list &optional result) = spec);
        let body = rest;
        let mut list = ctx.eval(&list)?;
        while list.as_bool() {
            let varlist = TulispValue::Nil
                .into_ref()
                .push(
                    TulispValue::Nil
                        .into_ref()
                        .push(var.clone())?
                        .push(list.car()?)?
                        .clone(),
                )?
                .clone();
            ctx.r#let(varlist)?;
            let eval_res = ctx.eval_progn(&body);
            ctx.pop();
            eval_res?;
            list = list.cdr()?;
        }
        ctx.eval(&result)
    }

    #[crate_fn_no_eval(add_func = "ctx")]
    fn list(ctx: &mut TulispContext, rest: TulispValueRef) -> Result<TulispValueRef, Error> {
        let (ctxobj, span) = (rest.ctxobj(), rest.span());
        let mut cons: Option<Cons> = None;
        for ele in rest.base_iter() {
            match cons {
                Some(ref mut cons) => {
                    cons.push(eval(ctx, &ele)?)?;
                }
                None => cons = Some(Cons::new(eval(ctx, &ele)?, TulispValue::Nil.into_ref())),
            }
        }
        match cons {
            Some(cons) => Ok(TulispValue::List { cons, ctxobj, span }.into_ref()),
            None => Ok(TulispValue::Nil.into_ref()),
        }
    }

    #[crate_fn(add_func = "ctx")]
    fn consp(arg: TulispValueRef) -> bool {
        arg.is_cons()
    }

    #[crate_fn_no_eval(add_func = "ctx")]
    fn sort(
        ctx: &mut TulispContext,
        seq: TulispValueRef,
        pred: TulispValueRef,
    ) -> Result<TulispValueRef, Error> {
        let pred = eval(ctx, &pred)?;
        let pred = ctx.get(pred.clone()).ok_or_else(|| {
            Error::new(ErrorKind::Undefined, format!("Unknown predicate: {}", pred))
        })?;
        let seq = eval(ctx, &seq)?;
        let mut vec: Vec<_> = seq.base_iter().collect();
        vec.sort_by(|v1, v2| {
            let vv = list!(,TulispValue::Nil.into_ref() ,v1.clone() ,v2.clone()).unwrap();
            vv.with_ctxobj(Some(pred.clone()));

            if eval(ctx, &vv)
                .unwrap_or_else(|_| TulispValue::Nil.into_ref())
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
    }

    // alist functions
    fn assoc_impl(
        key: TulispValueRef,
        alist: TulispValueRef,
        _testfn: Option<TulispValueRef>, // TODO: implement testfn support
    ) -> Result<TulispValueRef, Error> {
        if !alist.is_cons() {
            return Err(
                Error::new(ErrorKind::TypeMismatch, "expected alist".to_owned())
                    .with_span(alist.span()),
            );
        }
        for kvpair in alist.base_iter() {
            if !kvpair.is_cons() {
                return Err(Error::new(
                    ErrorKind::TypeMismatch,
                    "expected cons inside alist".to_owned(),
                )
                .with_span(kvpair.span()));
            }
            if kvpair.car()? == key {
                return Ok(kvpair);
            }
        }
        Ok(TulispValue::Nil.into())
    }

    #[crate_fn(add_func = "ctx")]
    fn assoc(
        key: TulispValueRef,
        alist: TulispValueRef,
        testfn: Option<TulispValueRef>,
    ) -> Result<TulispValueRef, Error> {
        assoc_impl(key, alist, testfn)
    }

    #[crate_fn(add_func = "ctx", name = "alist-get")]
    fn alist_get(
        key: TulispValueRef,
        alist: TulispValueRef,
        default_value: Option<TulispValueRef>,
        _remove: Option<TulispValueRef>, // TODO: implement remove, testfn support
        testfn: Option<TulispValueRef>,
    ) -> Result<TulispValueRef, Error> {
        let x = assoc_impl(key, alist, testfn)?;
        if x.as_bool() {
            x.cdr()
        } else {
            Ok(default_value.unwrap_or_else(|| TulispValue::Nil.into_ref()))
        }
    }
}
