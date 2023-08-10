use crate::cons::Cons;
use crate::context::Scope;
use crate::context::TulispContext;
use crate::error::Error;
use crate::error::ErrorKind;
use crate::eval::eval;
use crate::eval::eval_basic;
use crate::eval::eval_check_null;
use crate::eval::DummyEval;
use crate::eval::Eval;
use crate::lists;
use crate::TulispObject;
use crate::TulispValue;
use crate::{destruct_bind, list};
use std::convert::TryInto;
use std::rc::Rc;
use tulisp_proc_macros::{crate_fn, crate_fn_no_eval};

macro_rules! max_min_ops {
    ($oper:tt) => {{
        |selfobj: &TulispObject, other: &TulispObject| -> Result<TulispObject, Error> {
            if selfobj.floatp() {
                let s: f64 = selfobj.as_float().unwrap();
                let o: f64 = other.try_into()?;
                Ok(f64::$oper(s, o).into())
            } else if other.floatp() {
                let o: f64 = other.as_float().unwrap();
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
        |selfobj: &TulispObject, other: &TulispObject| -> Result<TulispObject, Error> {
            if selfobj.floatp() {
                let s: f64 = selfobj.as_float().unwrap();
                let o: f64 = other.try_into()?;
                Ok($oper(&s, &o).into())
            } else if other.floatp() {
                let o: f64 = other.as_float().unwrap();
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
    list: &TulispObject,
    method: fn(&TulispObject, &TulispObject) -> Result<TulispObject, Error>,
) -> Result<TulispObject, Error> {
    let mut eval_result = None;
    let mut iter = list.base_iter();

    let Some(mut first) = iter.next() else {
        return Err(Error::new(
            ErrorKind::TypeMismatch,
            "Incorrect number of arguments: 0".to_string(),
        ));
    };

    eval_basic(ctx, &first, &mut eval_result)?;
    if eval_result.is_some() {
        first = eval_result.unwrap();
        eval_result = None;
    }

    for next in iter {
        eval_basic(ctx, &next, &mut eval_result)?;
        if eval_result.is_some() {
            first = method(&first, &eval_result.unwrap())?;
            eval_result = None;
        } else {
            first = method(&first, &next)?;
        }
    }

    Ok(first)
}

fn mark_tail_calls(
    ctx: &mut TulispContext,
    name: TulispObject,
    body: TulispObject,
) -> Result<TulispObject, Error> {
    let ret = TulispObject::nil();
    let mut body_iter = body.base_iter();
    let mut tail = body_iter.next().unwrap(); // TODO: make safe
    for next in body_iter {
        ret.push(tail)?;
        tail = next;
    }
    if !tail.consp() {
        return Ok(body);
    }
    let span = tail.span();
    let ctxobj = tail.ctxobj();
    let tail_ident = tail.car()?;
    let tail_name_str = tail_ident.as_symbol()?;
    let new_tail = if tail_ident.eq(&name) {
        let ret_tail = TulispObject::nil().append(tail.cdr()?)?.to_owned();
        list!(,ctx.intern("list")
              ,TulispValue::Bounce.into_ref()
              ,@ret_tail)?
    } else if tail_name_str == "progn" || tail_name_str == "let" || tail_name_str == "let*" {
        list!(,tail_ident ,@mark_tail_calls(ctx, name, tail.cdr()?)?)?
    } else if tail_name_str == "if" {
        destruct_bind!((_if condition then_body &rest else_body) = tail);
        list!(,tail_ident
              ,condition.clone()
              ,mark_tail_calls(
                  ctx,
                  name.clone(),
                  list!(,then_body)?
              )?.car()?
              ,@mark_tail_calls(ctx, name, else_body)?
        )?
    } else if tail_name_str == "cond" {
        destruct_bind!((_cond &rest conds) = tail);
        let mut ret = list!(,tail_ident)?;
        for cond in conds.base_iter() {
            destruct_bind!((condition &rest body) = cond);
            ret = list!(,@ret
                        ,list!(,condition.clone()
                               ,@mark_tail_calls(ctx, name.clone(), body)?)?)?;
        }
        ret
    } else {
        tail
    };
    ret.push(new_tail.with_ctxobj(ctxobj).with_span(span))?;
    Ok(ret)
}

macro_rules! intern_set_func {
    ($ctx:ident, $func: ident, $name: literal) => {
        $ctx.intern($name)
            .set_scope(TulispValue::Func(Rc::new($func)).into())
            .unwrap();
    };
}

pub(crate) fn add(ctx: &mut TulispContext) {
    fn add(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
        reduce_with(ctx, args, binary_ops!(std::ops::Add::add))
    }
    intern_set_func!(ctx, add, "+");

    fn sub(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
        if let Some(cons) = args.as_list_cons() {
            if cons.cdr().null() {
                let vv = binary_ops!(std::ops::Sub::sub)(&0.into(), &eval(ctx, cons.car())?)?;
                Ok(vv)
            } else {
                reduce_with(ctx, args, binary_ops!(std::ops::Sub::sub))
            }
        } else {
            Err(Error::new(
                ErrorKind::MissingArgument,
                "Call to `sub` without any arguments".to_string(),
            )
            .with_span(args.span()))
        }
    }
    intern_set_func!(ctx, sub, "-");

    fn mul(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
        reduce_with(ctx, args, binary_ops!(std::ops::Mul::mul))
    }
    intern_set_func!(ctx, mul, "*");

    fn div(ctx: &mut TulispContext, rest: &TulispObject) -> Result<TulispObject, Error> {
        for ele in rest.base_iter() {
            if ele == TulispValue::from(0) || ele == TulispValue::from(0.0) {
                return Err(Error::new(
                    ErrorKind::Undefined,
                    "Division by zero".to_string(),
                ));
            }
        }
        reduce_with(ctx, &rest, binary_ops!(std::ops::Div::div))
    }
    intern_set_func!(ctx, div, "/");

    // // TODO: >, >=, <, <= - need to be able to support more than 2 args
    fn gt(ctx: &mut TulispContext, rest: &TulispObject) -> Result<TulispObject, Error> {
        reduce_with(ctx, rest, binary_ops!(std::cmp::PartialOrd::gt))
    }
    intern_set_func!(ctx, gt, ">");

    fn ge(ctx: &mut TulispContext, rest: &TulispObject) -> Result<TulispObject, Error> {
        reduce_with(ctx, rest, binary_ops!(std::cmp::PartialOrd::ge))
    }
    intern_set_func!(ctx, ge, ">=");

    fn lt(ctx: &mut TulispContext, rest: &TulispObject) -> Result<TulispObject, Error> {
        reduce_with(ctx, rest, binary_ops!(std::cmp::PartialOrd::lt))
    }
    intern_set_func!(ctx, lt, "<");

    fn le(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
        reduce_with(ctx, args, binary_ops!(std::cmp::PartialOrd::le))
    }
    intern_set_func!(ctx, le, "<=");

    fn max(ctx: &mut TulispContext, rest: &TulispObject) -> Result<TulispObject, Error> {
        reduce_with(ctx, rest, max_min_ops!(max))
    }
    intern_set_func!(ctx, max, "max");

    fn min(ctx: &mut TulispContext, rest: &TulispObject) -> Result<TulispObject, Error> {
        reduce_with(ctx, rest, max_min_ops!(min))
    }
    intern_set_func!(ctx, min, "min");

    #[crate_fn(add_func = "ctx")]
    fn equal(object1: TulispObject, object2: TulispObject) -> bool {
        object1.equal(&object2)
    }

    #[crate_fn(add_func = "ctx")]
    fn eq(object1: TulispObject, object2: TulispObject) -> bool {
        object1.eq(&object2)
    }

    #[crate_fn(add_func = "ctx", name = "1+")]
    fn impl_1_plus(number: TulispObject) -> Result<TulispObject, Error> {
        match &*number.inner_ref() {
            TulispValue::Int { value } => Ok((*value + 1).into()),
            TulispValue::Float { value } => Ok((*value + 1.0).into()),
            _ => Err(Error::new(
                ErrorKind::TypeMismatch,
                "expected a number as argument.".to_string(),
            )
            .with_span(number.span())),
        }
    }

    #[crate_fn(add_func = "ctx", name = "1-")]
    fn impl_1_minus(number: TulispObject) -> Result<TulispObject, Error> {
        match &*number.inner_ref() {
            TulispValue::Int { value } => Ok((*value - 1).into()),
            TulispValue::Float { value } => Ok((*value - 1.0).into()),
            _ => Err(Error::new(
                ErrorKind::TypeMismatch,
                "expected a number as argument.".to_string(),
            )
            .with_span(number.span())),
        }
    }

    #[crate_fn(add_func = "ctx", name = "mod")]
    fn impl_mod(dividend: TulispObject, divisor: TulispObject) -> Result<TulispObject, Error> {
        binary_ops!(std::ops::Rem::rem)(&dividend, &divisor)
    }

    #[crate_fn(add_func = "ctx")]
    fn expt(base: TulispObject, pow: TulispObject) -> Result<TulispObject, Error> {
        Ok(f64::powf(base.try_into()?, pow.try_into()?).into())
    }

    #[crate_fn_no_eval(add_func = "ctx")]
    fn concat(ctx: &mut TulispContext, rest: TulispObject) -> Result<TulispObject, Error> {
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
    fn format(input: TulispObject, rest: TulispObject) -> Result<TulispObject, Error> {
        let mut args = rest.base_iter();
        let mut output = String::new();
        let in_string = input.as_string().map_err(|e| e.with_span(input.span()))?;
        let mut in_chars = in_string.chars();
        while let Some(ch) = in_chars.next() {
            if ch != '%' {
                output.push(ch);
                continue;
            }
            let ch = match in_chars.next() {
                Some(vv) => vv,
                None => break,
            };
            if ch == '%' {
                output.push(ch);
                continue;
            }
            let Some(next_arg) = args.next() else {
                return Err(Error::new(
                    ErrorKind::MissingArgument,
                    "format has missing args".to_string(),
                ));
            };
            // TODO: improve format-spec coverage:
            // https://www.gnu.org/software/emacs/manual/html_node/elisp/Formatting-Strings.html
            match ch {
                's' => output.push_str(&next_arg.fmt_string()),
                'S' => output.push_str(&next_arg.to_string()),
                'd' => output.push_str(&next_arg.try_int()?.to_string()),
                'f' => output.push_str(&next_arg.try_float()?.to_string()),
                _ => {
                    return Err(Error::new(
                        ErrorKind::SyntaxError,
                        format!("Invalid format operation: %{}", ch),
                    )
                    .with_span(input.span()))
                }
            }
        }
        Ok(TulispObject::from(output).with_span(input.span()))
    }

    #[crate_fn(add_func = "ctx")]
    fn print(val: TulispObject) -> Result<TulispObject, Error> {
        println!("{}", val.fmt_string());
        Ok(val)
    }

    #[crate_fn(add_func = "ctx", name = "prin1-to-string")]
    fn prin1_to_string(arg: TulispObject) -> Result<TulispObject, Error> {
        Ok(TulispValue::from(arg.fmt_string()).into_ref())
    }

    #[crate_fn(add_func = "ctx")]
    fn princ(val: TulispObject) -> Result<TulispObject, Error> {
        println!("{}", val.fmt_string());
        Ok(val)
    }

    fn impl_if(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
        destruct_bind!((condition then_body &rest body) = args);
        if eval_check_null(ctx, &condition)? {
            ctx.eval_progn(&body)
        } else {
            let mut result = None;
            eval_basic(ctx, &then_body, &mut result)?;
            if let Some(result) = result {
                Ok(result)
            } else {
                Ok(then_body)
            }
        }
    }
    intern_set_func!(ctx, impl_if, "if");

    #[crate_fn_no_eval(add_func = "ctx")]
    fn cond(ctx: &mut TulispContext, rest: TulispObject) -> Result<TulispObject, Error> {
        for item in rest.base_iter() {
            destruct_bind!((condition &rest body) = item);
            if !eval_check_null(ctx, &condition)? {
                return ctx.eval_progn(&body);
            }
        }
        Ok(TulispObject::nil())
    }

    #[crate_fn_no_eval(add_func = "ctx", name = "while")]
    fn impl_while(
        ctx: &mut TulispContext,
        condition: TulispObject,
        rest: TulispObject,
    ) -> Result<TulispObject, Error> {
        let mut result = TulispObject::nil();
        while !eval_check_null(ctx, &condition)? {
            result = ctx.eval_progn(&rest)?;
        }
        Ok(result)
    }

    #[crate_fn_no_eval(add_func = "ctx")]
    fn setq(
        ctx: &mut TulispContext,
        name: TulispObject,
        value: TulispObject,
    ) -> Result<TulispObject, Error> {
        let value = eval(ctx, &value)?;
        name.set(value.clone())?;
        Ok(value)
    }

    #[crate_fn_no_eval(add_func = "ctx", name = "let")]
    fn impl_let(
        ctx: &mut TulispContext,
        varlist: TulispObject,
        rest: TulispObject,
    ) -> Result<TulispObject, Error> {
        if !rest.consp() {
            return Err(Error::new(
                ErrorKind::TypeMismatch,
                "let: expected varlist and body".to_string(),
            ));
        }
        let mut local = Scope::default();
        for varitem in varlist.base_iter() {
            if varitem.symbolp() {
                local.set(varitem, TulispObject::nil())?;
            } else if varitem.consp() {
                let span = varitem.span();
                destruct_bind!((&optional name value &rest rest) = varitem);
                if name.null() {
                    return Err(Error::new(
                        ErrorKind::Undefined,
                        "let varitem requires name".to_string(),
                    )
                    .with_span(span));
                }
                if !rest.null() {
                    return Err(Error::new(
                        ErrorKind::Undefined,
                        "let varitem has too many values".to_string(),
                    )
                    .with_span(span));
                }
                local.set(name, eval(ctx, &value)?)?;
            } else {
                return Err(Error::new(
                    ErrorKind::SyntaxError,
                    format!(
                        "varitems inside a let-varlist should be a var or a binding: {}",
                        varitem
                    ),
                )
                .with_span(varlist.span()));
            };
        }

        let ret = ctx.eval_progn(&rest);
        local.remove_all()?;

        ret
    }

    #[crate_fn_no_eval(add_func = "ctx")]
    fn progn(ctx: &mut TulispContext, rest: TulispObject) -> Result<TulispObject, Error> {
        ctx.eval_progn(&rest)
    }

    #[crate_fn_no_eval(add_func = "ctx")]
    fn defun(
        ctx: &mut TulispContext,
        name: TulispObject,
        params: TulispObject,
        rest: TulispObject,
    ) -> Result<TulispObject, Error> {
        let body = if rest.car()?.as_string().is_ok() {
            rest.cdr()?
        } else {
            rest
        };
        let body = mark_tail_calls(ctx, name.clone(), body).map_err(|e| {
            println!("mark_tail_calls error: {:?}", e);
            e
        })?;
        name.set_scope(
            TulispValue::Lambda {
                params: params.try_into()?,
                body,
            }
            .into_ref(),
        )?;
        Ok(TulispObject::nil())
    }

    #[crate_fn_no_eval(add_func = "ctx")]
    fn lambda(params: TulispObject, rest: TulispObject) -> Result<TulispObject, Error> {
        let body = if rest.car()?.as_string().is_ok() {
            rest.cdr()?
        } else {
            rest
        };
        Ok(TulispValue::Lambda {
            params: params.try_into()?,
            body,
        }
        .into_ref())
    }

    #[crate_fn_no_eval(add_func = "ctx")]
    fn defmacro(
        name: TulispObject,
        params: TulispObject,
        rest: TulispObject,
    ) -> Result<TulispObject, Error> {
        let body = if rest.car()?.as_string().is_ok() {
            rest.cdr()?
        } else {
            rest
        };
        name.set_scope(
            TulispValue::Defmacro {
                params: params.try_into()?,
                body,
            }
            .into_ref(),
        )?;
        Ok(TulispObject::nil())
    }

    #[crate_fn_no_eval(add_func = "ctx")]
    fn quote(arg: TulispObject) -> TulispObject {
        TulispValue::Quote { value: arg }.into()
    }

    #[crate_fn(add_func = "ctx")]
    fn null(arg: TulispObject) -> bool {
        arg.null()
    }

    #[crate_fn(add_func = "ctx", name = "eval")]
    fn impl_eval(ctx: &mut TulispContext, arg: TulispObject) -> Result<TulispObject, Error> {
        crate::eval::eval(ctx, &arg)
    }

    #[crate_fn_no_eval(add_func = "ctx", name = "funcall")]
    fn funcall(
        ctx: &mut TulispContext,
        name: TulispObject,
        rest: TulispObject,
    ) -> Result<TulispObject, Error> {
        let name = eval(ctx, &name)?;
        let name = eval(ctx, &name)?;
        if matches!(&*name.inner_ref(), TulispValue::Lambda { .. }) {
            crate::eval::funcall::<Eval>(ctx, &name, &rest)
        } else {
            crate::eval::funcall::<DummyEval>(ctx, &name, &rest)
        }
    }

    #[crate_fn(add_func = "ctx", name = "macroexpand")]
    fn impl_macroexpand(
        ctx: &mut TulispContext,
        name: TulispObject,
    ) -> Result<TulispObject, Error> {
        crate::eval::macroexpand(ctx, name)
    }

    // List functions

    #[crate_fn(add_func = "ctx", name = "car")]
    fn impl_car(name: TulispObject) -> Result<TulispObject, Error> {
        name.car()
    }

    #[crate_fn(add_func = "ctx", name = "cdr")]
    fn impl_cdr(name: TulispObject) -> Result<TulispObject, Error> {
        name.cdr()
    }

    #[crate_fn(add_func = "ctx", name = "cons")]
    fn impl_cons(car: TulispObject, cdr: TulispObject) -> TulispObject {
        TulispObject::cons(car, cdr)
    }

    #[crate_fn_no_eval(add_func = "ctx")]
    fn append(
        ctx: &mut TulispContext,
        first: TulispObject,
        rest: TulispObject,
    ) -> Result<TulispObject, Error> {
        let first = eval(ctx, &first)?;
        for ele in rest.base_iter() {
            first.append(eval(ctx, &ele)?.deep_copy()?)?;
        }
        Ok(first)
    }

    #[crate_fn_no_eval(add_func = "ctx")]
    fn dolist(
        ctx: &mut TulispContext,
        spec: TulispObject,
        rest: TulispObject,
    ) -> Result<TulispObject, Error> {
        destruct_bind!((var list &optional result) = spec);
        let body = rest;
        let mut list = ctx.eval(&list)?;
        while list.as_bool() {
            let mut scope = Scope::default();
            scope.set(var.clone(), list.car()?)?;
            let eval_res = ctx.eval_progn(&body);
            scope.remove_all()?;
            eval_res?;
            list = list.cdr()?;
        }
        ctx.eval(&result)
    }

    #[crate_fn_no_eval(add_func = "ctx")]
    fn dotimes(
        ctx: &mut TulispContext,
        spec: TulispObject,
        rest: TulispObject,
    ) -> Result<TulispObject, Error> {
        destruct_bind!((var count &optional result) = spec);
        for counter in 0..count.as_int()? {
            let mut scope = Scope::default();
            scope.set(var.clone(), TulispObject::from(counter))?;
            let eval_res = ctx.eval_progn(&rest);
            scope.remove_all()?;
            eval_res?;
        }
        ctx.eval(&result)
    }

    #[crate_fn_no_eval(add_func = "ctx")]
    fn list(ctx: &mut TulispContext, rest: TulispObject) -> Result<TulispObject, Error> {
        let (ctxobj, span) = (rest.ctxobj(), rest.span());
        let mut cons: Option<Cons> = None;
        for ele in rest.base_iter() {
            match cons {
                Some(ref mut cons) => {
                    cons.push(eval(ctx, &ele)?)?;
                }
                None => cons = Some(Cons::new(eval(ctx, &ele)?, TulispObject::nil())),
            }
        }
        match cons {
            Some(cons) => Ok(TulispValue::List { cons, ctxobj }
                .into_ref()
                .with_span(span)),
            None => Ok(TulispObject::nil()),
        }
    }

    #[crate_fn(add_func = "ctx")]
    fn nthcdr(n: i64, list: TulispObject) -> Result<TulispObject, Error> {
        lists::nthcdr(n, list)
    }

    #[crate_fn(add_func = "ctx")]
    fn nth(n: i64, list: TulispObject) -> Result<TulispObject, Error> {
        lists::nth(n, list)
    }

    #[crate_fn(add_func = "ctx")]
    fn mapcar(
        ctx: &mut TulispContext,
        func: TulispObject,
        seq: TulispObject,
    ) -> Result<TulispObject, Error> {
        ctx.map(&func, &seq)
    }

    #[crate_fn(add_func = "ctx")]
    fn assoc(
        ctx: &mut TulispContext,
        key: TulispObject,
        alist: TulispObject,
        testfn: Option<TulispObject>,
    ) -> Result<TulispObject, Error> {
        lists::assoc(ctx, &key, &alist, testfn)
    }

    #[crate_fn(add_func = "ctx", name = "alist-get")]
    fn alist_get(
        ctx: &mut TulispContext,
        key: TulispObject,
        alist: TulispObject,
        default_value: Option<TulispObject>,
        remove: Option<TulispObject>, // TODO: implement after `setf`
        testfn: Option<TulispObject>,
    ) -> Result<TulispObject, Error> {
        lists::alist_get(ctx, &key, &alist, default_value, remove, testfn)
    }

    #[crate_fn(add_func = "ctx", name = "plist-get")]
    fn plist_get(plist: TulispObject, property: TulispObject) -> Result<TulispObject, Error> {
        lists::plist_get(plist, &property)
    }

    // predicates begin
    macro_rules! predicate_function {
        ($name: ident) => {
            #[crate_fn(add_func = "ctx")]
            fn $name(arg: TulispObject) -> bool {
                arg.$name()
            }
        };
    }
    predicate_function!(consp);
    predicate_function!(listp);
    predicate_function!(floatp);
    predicate_function!(integerp);
    predicate_function!(numberp);
    predicate_function!(stringp);
    predicate_function!(symbolp);

    // predicates end
}
