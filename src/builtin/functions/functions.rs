use crate::TulispObject;
use crate::TulispValue;
use crate::cons::Cons;
use crate::context::Scope;
use crate::context::TulispContext;
use crate::destruct_eval_bind;
use crate::error::Error;
use crate::eval::DummyEval;
use crate::eval::Eval;
use crate::eval::eval;
use crate::eval::eval_and_then;
use crate::eval::eval_check_null;
use crate::lists;
use crate::value::DefunParams;
use crate::{destruct_bind, list};
use std::convert::TryInto;

pub(super) fn reduce_with(
    ctx: &mut TulispContext,
    list: &TulispObject,
    method: fn(&TulispObject, &TulispObject) -> Result<TulispObject, Error>,
) -> Result<TulispObject, Error> {
    let mut first = list.car_and_then(|x| eval(ctx, x))?;
    let mut rest = list.cdr()?;
    while rest.is_truthy() {
        let next = rest.car_and_then(|x| eval(ctx, x))?;
        first = method(&first, &next)?;
        rest = rest.cdr()?;
    }

    Ok(first)
}

fn mark_tail_calls(
    ctx: &mut TulispContext,
    name: TulispObject,
    body: TulispObject,
) -> Result<TulispObject, Error> {
    if !body.consp() {
        return Ok(body);
    }
    let ret = TulispObject::nil();
    let mut body_iter = body.base_iter();
    let mut tail = body_iter.next().unwrap();
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
            ,TulispValue::Bounce.into_ref(None)
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

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.add_special_form("load", |ctx, args| {
        destruct_eval_bind!(ctx, (filename) = args);
        ctx.eval_file(&filename.as_string()?)
    });

    ctx.add_special_form("intern", |ctx, args| {
        destruct_eval_bind!(ctx, (name) = args);
        Ok(ctx.intern(&name.as_string()?))
    });

    fn make_symbol(name: String) -> Result<TulispObject, Error> {
        let constant = name.starts_with(":");
        Ok(TulispObject::symbol(name, constant))
    }

    ctx.add_special_form("make-symbol", |ctx, args| {
        destruct_eval_bind!(ctx, (name) = args);
        make_symbol(name.as_string()?)
    });

    ctx.add_special_form("gensym", |ctx, args| {
        destruct_eval_bind!(ctx, (&optional prefix) = args);
        let prefix = if !prefix.null() {
            if prefix.stringp() {
                prefix.as_string()?
            } else {
                return Err(Error::type_mismatch(
                    "gensym: prefix must be a string".to_string(),
                ));
            }
        } else {
            "g".to_string()
        };
        let counter = ctx.intern("gensym-counter");
        let count = if counter.boundp() {
            let value = counter.get()?;
            if value.integerp() {
                value.as_int().unwrap()
            } else {
                0
            }
        } else {
            0
        };
        counter.set(TulispObject::from(count + 1))?;

        make_symbol(format!("{prefix}{count}"))
    });

    ctx.add_special_form("concat", |ctx, args| {
        destruct_eval_bind!(ctx, (&rest rest) = args);
        let mut ret = String::new();
        for ele in rest.base_iter() {
            match ele.as_string() {
                Ok(ref s) => ret.push_str(s),
                _ => {
                    return Err(Error::type_mismatch(format!("Not a string: {}", ele)));
                }
            }
        }
        Ok(TulispValue::from(ret).into_ref(rest.span()))
    });

    ctx.add_special_form("format", |ctx, args| {
        destruct_eval_bind!(ctx, (input &rest rest) = args);
        let mut args = rest.base_iter();
        let mut output = String::new();
        let in_string = input.as_string().map_err(|e| e.with_trace(input.clone()))?;
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
                return Err(Error::missing_argument(
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
                    return Err(Error::syntax_error(format!(
                        "Invalid format operation: %{}",
                        ch
                    )));
                }
            }
        }
        Ok(TulispObject::from(output).with_span(input.span()))
    });

    ctx.add_special_form("print", |ctx, args| {
        destruct_eval_bind!(ctx, (val) = args);
        println!("{}", val.fmt_string());
        Ok(val)
    });

    ctx.add_special_form("prin1-to-string", |ctx, args| {
        destruct_eval_bind!(ctx, (arg) = args);
        Ok(TulispValue::from(arg.fmt_string()).into_ref(arg.span()))
    });

    ctx.add_special_form("princ", |ctx, args| {
        destruct_eval_bind!(ctx, (val) = args);
        println!("{}", val.fmt_string());
        Ok(val)
    });

    ctx.add_special_form("while", |ctx, args| {
        destruct_bind!((condition &rest rest) = args);
        let mut result = TulispObject::nil();
        while !eval_check_null(ctx, &condition)? {
            result = ctx.eval_progn(&rest)?;
        }
        Ok(result)
    });

    ctx.add_special_form("setq", |ctx, args| {
        let value = args.cdr_and_then(|args| {
            if args.null() {
                return Err(Error::type_mismatch(
                    "setq requires exactly 2 arguments".to_string(),
                ));
            }
            args.cdr_and_then(|x| {
                if !x.null() {
                    return Err(Error::type_mismatch(
                        "setq requires exactly 2 arguments".to_string(),
                    ));
                }
                args.car_and_then(|arg| ctx.eval(arg))
            })
        })?;
        args.car_and_then(|name| name.set(value.clone()))?;
        Ok(value)
    });

    ctx.add_special_form("set", |ctx, args| {
        let value = args.cdr_and_then(|args| {
            if args.null() {
                return Err(Error::type_mismatch(
                    "set requires exactly 2 arguments".to_string(),
                ));
            }
            args.cdr_and_then(|x| {
                if !x.null() {
                    return Err(Error::type_mismatch(
                        "set requires exactly 2 arguments".to_string(),
                    ));
                }
                args.car_and_then(|arg| ctx.eval(arg))
            })
        })?;
        args.car_and_then(|name_sym| {
            ctx.eval_and_then(name_sym, |_, name| name.set(value.clone()))
        })?;
        Ok(value)
    });

    fn impl_let(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
        destruct_bind!((varlist &rest rest) = args);
        if !rest.consp() {
            return Err(Error::type_mismatch(
                "let: expected varlist and body".to_string(),
            ));
        }
        let mut local = Scope::default();
        for varitem in varlist.base_iter() {
            if varitem.symbolp() {
                local.set(varitem, TulispObject::nil())?;
            } else if varitem.consp() {
                destruct_bind!((&optional name value &rest rest) = varitem);
                if name.null() {
                    return Err(Error::undefined("let varitem requires name".to_string()));
                }
                if !rest.null() {
                    return Err(Error::undefined(
                        "let varitem has too many values".to_string(),
                    ));
                }
                local.set(name, eval(ctx, &value)?)?;
            } else {
                return Err(Error::syntax_error(format!(
                    "varitems inside a let-varlist should be a var or a binding: {}",
                    varitem
                )));
            };
        }

        let ret = ctx.eval_progn(&rest);
        local.remove_all()?;

        ret
    }
    ctx.add_special_form("let", impl_let);
    ctx.add_special_form("let*", impl_let);

    ctx.add_special_form("progn", |ctx, args| ctx.eval_progn(args));

    ctx.add_special_form("defun", |ctx, args| {
        destruct_bind!((name params &rest rest) = args);
        {
            let body = if rest.car()?.as_string().is_ok() {
                rest.cdr()?
            } else {
                rest
            };
            let body = mark_tail_calls(ctx, name.clone(), body).map_err(|e| {
                println!("mark_tail_calls error: {:?}", e);
                e
            })?;
            name.set_global(
                TulispValue::Lambda {
                    params: params.try_into()?,
                    body,
                }
                .into_ref(None),
            )?;
            Ok(TulispObject::nil())
        }
    });

    fn lambda(_ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
        destruct_bind!((params &rest rest) = args);
        let body = if rest.car()?.as_string().is_ok() {
            rest.cdr()?
        } else {
            rest
        };
        let params: DefunParams = params.try_into()?;
        let param_names: Vec<_> = params.iter().map(|x| x.param.clone()).collect();

        fn capture_inside_quoted(
            captured_vars: &mut Vec<(TulispObject, TulispObject)>,
            exclude: &[TulispObject],
            value: TulispObject,
        ) -> Result<TulispObject, Error> {
            let inner_ref = value.inner_ref();
            let res = match &inner_ref.0 {
                TulispValue::Backquote { value } => TulispValue::Backquote {
                    value: capture_variables(captured_vars, exclude, value.clone())?,
                }
                .into_ref(value.span()),
                TulispValue::Unquote { value } => TulispValue::Unquote {
                    value: capture_variables(captured_vars, exclude, value.clone())?,
                }
                .into_ref(value.span()),
                TulispValue::Splice { value } => TulispValue::Splice {
                    value: capture_variables(captured_vars, exclude, value.clone())?,
                }
                .into_ref(value.span()),
                TulispValue::Sharpquote { value } => TulispValue::Sharpquote {
                    value: capture_variables(captured_vars, exclude, value.clone())?,
                }
                .into_ref(value.span()),
                TulispValue::Quote { value } => TulispValue::Quote {
                    value: capture_variables(captured_vars, exclude, value.clone())?,
                }
                .into_ref(value.span()),
                _ => {
                    drop(inner_ref);
                    value
                }
            };
            Ok(res)
        }

        fn slice_contains(vec: &[TulispObject], item: &TulispObject) -> bool {
            for i in vec {
                if i.eq(item) {
                    return true;
                }
            }
            false
        }

        fn capture_symbol(
            captured_vars: &mut Vec<(TulispObject, TulispObject)>,
            exclude: &[TulispObject],
            symbol: TulispObject,
        ) -> Result<TulispObject, Error> {
            if !symbol.is_lexically_bound() {
                return Ok(symbol);
            }
            if !slice_contains(exclude, &symbol) {
                for (from, to) in captured_vars.iter() {
                    if symbol.eq(from) {
                        return Ok(to.clone().with_span(symbol.span()));
                    }
                }
                let new_var = TulispObject::lexical_binding(symbol.clone());
                new_var.set(symbol.get()?)?;
                captured_vars.push((symbol, new_var.clone()));
                return Ok(new_var);
            }
            Ok(symbol)
        }

        fn capture_variables(
            captured_vars: &mut Vec<(TulispObject, TulispObject)>,
            exclude: &[TulispObject],
            mut body: TulispObject,
        ) -> Result<TulispObject, Error> {
            let result = TulispObject::nil().with_span(body.span());
            if !body.consp() {
                if body.symbolp() {
                    return capture_symbol(captured_vars, exclude, body);
                }
                return capture_inside_quoted(captured_vars, exclude, body);
            }

            loop {
                let car = body.car()?;
                if car.consp() {
                    result.push(capture_variables(captured_vars, exclude, car)?)?;
                } else if car.symbolp() {
                    result.push(capture_symbol(captured_vars, exclude, car)?)?;
                } else {
                    result.push(capture_inside_quoted(captured_vars, exclude, car)?)?;
                }
                let cdr = body.cdr()?;
                if cdr.null() {
                    break;
                }
                if cdr.symbolp() {
                    result.append(capture_symbol(captured_vars, exclude, cdr)?)?;
                    break;
                } else if !cdr.consp() {
                    result.append(capture_inside_quoted(captured_vars, exclude, cdr)?)?;
                    break;
                }
                body = cdr;
            }
            Ok(result)
        }

        let body = capture_variables(&mut vec![], &param_names, body)?;
        Ok(TulispValue::Lambda { params, body }.into_ref(None))
    }
    ctx.add_special_form("lambda", lambda);

    ctx.add_special_form("defmacro", |_ctx, args| {
        destruct_bind!((name params &rest rest) = args);
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
            .into_ref(None),
        )?;
        Ok(TulispObject::nil())
    });

    ctx.add_special_form("null", |ctx, args| {
        destruct_eval_bind!(ctx, (arg) = args);
        Ok(arg.null().into())
    });

    ctx.add_special_form("eval", |ctx, args| {
        destruct_eval_bind!(ctx, (arg) = args);
        crate::eval::eval(ctx, &arg)
    });

    ctx.add_special_form("funcall", |ctx, args| {
        destruct_bind!((name &rest rest) = args);
        let name = eval(ctx, &name)?;
        let name = eval(ctx, &name)?;
        if matches!(&name.inner_ref().0, TulispValue::Lambda { .. }) {
            crate::eval::funcall::<Eval>(ctx, &name, &rest)
        } else {
            crate::eval::funcall::<DummyEval>(ctx, &name, &rest)
        }
    });

    ctx.add_special_form("macroexpand", |ctx, args| {
        destruct_eval_bind!(ctx, (name) = args);
        crate::eval::macroexpand(ctx, name)
    });

    // List functions

    ctx.add_special_form("cons", |ctx, args| {
        let cdr = args.cdr_and_then(|args| {
            if args.null() {
                return Err(Error::type_mismatch(
                    "cons requires exactly 2 arguments".to_string(),
                ));
            }
            args.cdr_and_then(|x| {
                if !x.null() {
                    return Err(Error::type_mismatch(
                        "cons requires exactly 2 arguments".to_string(),
                    ));
                }
                args.car_and_then(|arg| ctx.eval(arg))
            })
        })?;
        let car = args.car_and_then(|arg| ctx.eval(arg))?;
        Ok(TulispObject::cons(car, cdr))
    });

    ctx.add_special_form("append", |ctx, args| {
        destruct_eval_bind!(ctx, (first &rest rest) = args);
        for ele in rest.base_iter() {
            first.append(ele.deep_copy()?)?;
        }
        Ok(first)
    });

    ctx.add_special_form("dolist", |ctx, args| {
        destruct_bind!((spec &rest body) = args);
        destruct_bind!((var list &optional result) = spec);
        let mut list = ctx.eval(&list)?;
        var.set_scope(list.car()?)?;
        while list.is_truthy() {
            let eval_res = ctx.eval_progn(&body);
            eval_res?;
            list = list.cdr()?;
            var.set_unchecked(list.car()?);
        }
        var.unset()?;
        ctx.eval(&result)
    });

    ctx.add_special_form("dotimes", |ctx, args| {
        destruct_bind!((spec &rest body) = args);
        destruct_bind!((var count &optional result) = spec);
        var.set_scope(TulispObject::from(0))?;
        for counter in 0..count.as_int()? {
            var.set_unchecked(TulispObject::from(counter));
            let eval_res = ctx.eval_progn(&body);
            eval_res?;
        }
        var.unset()?;
        ctx.eval(&result)
    });

    ctx.add_special_form("list", |ctx, args| {
        let (ctxobj, span) = (args.ctxobj(), args.span());
        let mut cons: Option<Cons> = None;
        for ele in args.base_iter() {
            match cons {
                Some(ref mut cons) => {
                    cons.push(eval(ctx, &ele)?)?;
                }
                None => cons = Some(Cons::new(eval(ctx, &ele)?, TulispObject::nil())),
            }
        }
        match cons {
            Some(cons) => Ok(TulispValue::List { cons, ctxobj }.into_ref(span)),
            None => Ok(TulispObject::nil()),
        }
    });

    ctx.add_special_form("mapcar", |ctx, args| {
        destruct_eval_bind!(ctx, (func seq) = args);
        ctx.map(&func, &seq)
    });

    ctx.add_special_form("assoc", |ctx, args| {
        destruct_eval_bind!(ctx, (key alist &optional testfn) = args);
        lists::assoc(
            ctx,
            &key,
            &alist,
            if testfn.null() { None } else { Some(testfn) },
        )
    });

    ctx.add_special_form("alist-get", |ctx, args| {
        destruct_eval_bind!(ctx, (key alist &optional default_value remove testfn) = args);
        lists::alist_get(
            ctx,
            &key,
            &alist,
            if default_value.null() {
                None
            } else {
                Some(default_value)
            },
            if remove.null() { None } else { Some(remove) }, // TODO: implement after setf
            if testfn.null() { None } else { Some(testfn) },
        )
    });

    ctx.add_special_form("plist-get", |ctx, args| {
        destruct_eval_bind!(ctx, (plist property) = args);
        lists::plist_get(&plist, &property)
    });

    // predicates begin
    macro_rules! predicate_function {
        ($name: ident) => {
            fn $name(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
                match args.cdr_and_then(|x| Ok(x.null())) {
                    Err(err) => return Err(err),
                    Ok(false) => {
                        return Err(Error::type_mismatch(format!(
                            "Expected exatly 1 argument for {}. Got args: {}",
                            stringify!($name),
                            args
                        )))
                    }
                    Ok(true) => {}
                }
                args.car_and_then(|arg| eval_and_then(ctx, &arg, |_, x| Ok(x.$name().into())))
            }
            ctx.add_special_form(stringify!($name), $name);
        };
    }
    predicate_function!(consp);
    predicate_function!(listp);
    predicate_function!(floatp);
    predicate_function!(integerp);
    predicate_function!(numberp);
    predicate_function!(stringp);
    predicate_function!(symbolp);
    predicate_function!(boundp);
    predicate_function!(keywordp);
    // predicates end

    ctx.add_special_form("declare", |_ctx, _args| {
        // no-op
        Ok(TulispObject::nil())
    });
}
