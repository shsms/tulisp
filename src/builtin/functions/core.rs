use crate::TulispObject;
use crate::TulispValue;
use crate::context::Scope;
use crate::context::TulispContext;
use crate::destruct_eval_bind;
use crate::error::Error;
use crate::eval::DummyEval;
use crate::eval::Eval;
use crate::eval::EvalInto;
use crate::lists;
use crate::value::DefunParams;
use crate::{destruct_bind, list};
use std::convert::TryInto;

fn mark_tail_calls(
    ctx: &mut TulispContext,
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
    let new_tail = if ctx.eval(&tail_ident).is_ok_and(|f| {
        matches!(f.inner_ref().0, TulispValue::Lambda { .. })
    }) {
        let ret_tail = TulispObject::nil().append(tail.cdr()?)?.to_owned();
        list!(,ctx.intern("list")
            ,TulispValue::Bounce.into_ref(None)
            ,tail_ident
            ,@ret_tail)?
    } else if tail_name_str == "progn" || tail_name_str == "let" || tail_name_str == "let*" {
        list!(,tail_ident ,@mark_tail_calls(ctx, tail.cdr()?)?)?
    } else if tail_name_str == "if" {
        destruct_bind!((_if condition then_body &rest else_body) = tail);
        list!(,tail_ident
            ,condition.clone()
            ,mark_tail_calls(ctx, list!(,then_body)?)?.car()?
            ,@mark_tail_calls(ctx, else_body)?
        )?
    } else if tail_name_str == "cond" {
        destruct_bind!((_cond &rest conds) = tail);
        let mut ret = list!(,tail_ident)?;
        for cond in conds.base_iter() {
            destruct_bind!((condition &rest body) = cond);
            ret = list!(,@ret
                ,list!(,condition.clone()
                    ,@mark_tail_calls(ctx, body)?)?)?;
        }
        ret
    } else {
        tail
    };
    ret.push(new_tail.with_ctxobj(ctxobj).with_span(span))?;
    Ok(ret)
}

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.defun("load", |ctx: &mut TulispContext, filename: String| {
        let full_path = if let Some(ref load_path) = ctx.load_path {
            load_path.join(&filename)
        } else {
            std::path::PathBuf::from(&filename)
        };
        let Some(full_path) = full_path.to_str() else {
            return Err(Error::invalid_argument(format!(
                "load: Invalid path: {}",
                full_path.to_string_lossy()
            )));
        };
        ctx.eval_file(full_path)
    });

    ctx.defun("intern", |ctx: &mut TulispContext, name: String| -> TulispObject {
        ctx.intern(&name)
    });

    fn make_symbol(name: String) -> TulispObject {
        let constant = name.starts_with(":");
        TulispObject::symbol(name, constant)
    }

    ctx.defun("make-symbol", make_symbol);

    ctx.defun(
        "gensym",
        |ctx: &mut TulispContext, prefix: Option<String>| -> Result<TulispObject, Error> {
            let prefix = prefix.unwrap_or_else(|| "g".to_string());
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
            Ok(make_symbol(format!("{prefix}{count}")))
        },
    );

    ctx.defun(
        "concat",
        |rest: crate::Rest<TulispObject>| -> Result<String, Error> {
            let mut ret = String::new();
            for ele in rest {
                match ele.as_string() {
                    Ok(ref s) => ret.push_str(s),
                    _ => {
                        return Err(Error::type_mismatch(format!("Not a string: {}", ele)));
                    }
                }
            }
            Ok(ret)
        },
    );

    ctx.defun("format", |in_string: String, rest: crate::Rest<TulispObject>| -> Result<String, Error> {
        let rest: Vec<TulispObject> = rest.into_iter().collect();
        let mut args = rest.iter();
        let mut output = String::new();
        let mut in_chars = in_string.chars().peekable();
        // Supports `%[-][0]WIDTHTYPE` where TYPE is one of `s S d f`, plus
        // `%%` for a literal percent. The `-` flag left-aligns and the `0`
        // flag pads numerics with zeros. See the Emacs manual for the full
        // format-spec grammar:
        // https://www.gnu.org/software/emacs/manual/html_node/elisp/Formatting-Strings.html
        while let Some(ch) = in_chars.next() {
            if ch != '%' {
                output.push(ch);
                continue;
            }
            let mut left_align = false;
            let mut zero_pad = false;
            let mut width: usize = 0;
            loop {
                match in_chars.peek() {
                    Some('-') => {
                        left_align = true;
                        in_chars.next();
                    }
                    Some('0') if width == 0 => {
                        zero_pad = true;
                        in_chars.next();
                    }
                    Some(c) if c.is_ascii_digit() => {
                        width = width * 10 + (*c as usize - '0' as usize);
                        in_chars.next();
                    }
                    _ => break,
                }
            }
            let type_char = match in_chars.next() {
                Some(c) => c,
                None => {
                    return Err(Error::syntax_error(
                        "format: unterminated % spec".to_string(),
                    ));
                }
            };
            if type_char == '%' {
                output.push('%');
                continue;
            }
            let Some(next_arg) = args.next() else {
                return Err(Error::missing_argument(
                    "format has missing args".to_string(),
                ));
            };
            let formatted = match type_char {
                's' => next_arg.fmt_string(),
                'S' => next_arg.to_string(),
                'd' => next_arg.try_int()?.to_string(),
                'f' => next_arg.try_float()?.to_string(),
                _ => {
                    return Err(Error::syntax_error(format!(
                        "Invalid format operation: %{}",
                        type_char
                    )));
                }
            };
            let len = formatted.chars().count();
            if width > len {
                let pad_char = if zero_pad && !left_align && matches!(type_char, 'd' | 'f') {
                    '0'
                } else {
                    ' '
                };
                let pad = pad_char.to_string().repeat(width - len);
                if left_align {
                    output.push_str(&formatted);
                    output.push_str(&pad);
                } else {
                    output.push_str(&pad);
                    output.push_str(&formatted);
                }
            } else {
                output.push_str(&formatted);
            }
        }
        Ok(output)
    });

    ctx.defun("print", |val: TulispObject| -> TulispObject {
        println!("{}", val.fmt_string());
        val
    });

    ctx.defun("prin1-to-string", |arg: TulispObject| -> String { arg.fmt_string() });

    ctx.defun("princ", |val: TulispObject| -> TulispObject {
        println!("{}", val.fmt_string());
        val
    });

    ctx.defspecial("while", |ctx, args| {
        destruct_bind!((condition &rest rest) = args);
        let mut result = TulispObject::nil();
        while condition.eval_into(ctx)? {
            result = ctx.eval_progn(&rest)?;
        }
        Ok(result)
    });

    ctx.defspecial("setq", |ctx, args| {
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

    ctx.defun(
        "set",
        |name: TulispObject, value: TulispObject| -> Result<TulispObject, Error> {
            name.set(value.clone())?;
            Ok(value)
        },
    );

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
                    return Err(Error::syntax_error("let varitem requires name".to_string()));
                }
                if !rest.null() {
                    return Err(Error::syntax_error(
                        "let varitem has too many values".to_string(),
                    ));
                }
                local.set(name, ctx.eval(&value)?)?;
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
    ctx.defspecial("let", impl_let);
    ctx.defspecial("let*", impl_let);

    ctx.defspecial("progn", |ctx, args| ctx.eval_progn(args));

    ctx.defspecial("defun", |ctx, args| {
        destruct_bind!((name params &rest rest) = args);
        {
            let body = if rest.car()?.as_string().is_ok() {
                rest.cdr()?
            } else {
                rest
            };
            let body = mark_tail_calls(ctx, body).map_err(|e| {
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
    ctx.defspecial("lambda", lambda);

    ctx.defspecial("defmacro", |_ctx, args| {
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

    ctx.defun("null", |arg: TulispObject| -> bool { arg.null() });

    ctx.defun(
        "eval",
        |ctx: &mut TulispContext, arg: TulispObject| -> Result<TulispObject, Error> {
            ctx.eval(&arg)
        },
    );

    ctx.defspecial("funcall", |ctx, args| {
        destruct_bind!((name &rest rest) = args);
        let name = ctx.eval(&name)?;
        let name = ctx.eval(&name)?;
        if matches!(&name.inner_ref().0, TulispValue::Lambda { .. }) {
            crate::eval::funcall::<Eval>(ctx, &name, &rest)
        } else {
            crate::eval::funcall::<DummyEval>(ctx, &name, &rest)
        }
    });

    ctx.defun(
        "macroexpand",
        |ctx: &mut TulispContext, name: TulispObject| -> Result<TulispObject, Error> {
            crate::eval::macroexpand(ctx, name)
        },
    );

    // List functions

    ctx.defun(
        "cons",
        |car: TulispObject, cdr: TulispObject| -> TulispObject { TulispObject::cons(car, cdr) },
    );

    ctx.defun(
        "append",
        |first: TulispObject, rest: crate::Rest<TulispObject>| -> Result<TulispObject, Error> {
            for ele in rest {
                first.append(ele.deep_copy()?)?;
            }
            Ok(first)
        },
    );

    ctx.defspecial("dolist", |ctx, args| {
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

    ctx.defspecial("dotimes", |ctx, args| {
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

    ctx.defun(
        "list",
        |args: crate::Rest<TulispObject>| -> TulispObject { args.into_iter().collect() },
    );

    ctx.defspecial("mapcar", |ctx, args| {
        destruct_eval_bind!(ctx, (func seq) = args);
        ctx.map(&func, &seq)
    });

    ctx.defun(
        "assoc",
        |ctx: &mut TulispContext,
         key: TulispObject,
         alist: TulispObject,
         testfn: Option<TulispObject>|
         -> Result<TulispObject, Error> { lists::assoc(ctx, &key, &alist, testfn) },
    );

    ctx.defun(
        "alist-get",
        |ctx: &mut TulispContext,
         key: TulispObject,
         alist: TulispObject,
         default_value: Option<TulispObject>,
         remove: Option<TulispObject>,
         testfn: Option<TulispObject>|
         -> Result<TulispObject, Error> {
            // TODO: implement remove after `setf`.
            lists::alist_get(ctx, &key, &alist, default_value, remove, testfn)
        },
    );

    ctx.defun(
        "plist-get",
        |plist: TulispObject, property: TulispObject| -> Result<TulispObject, Error> {
            lists::plist_get(&plist, &property)
        },
    );

    // predicates begin
    macro_rules! predicate_function {
        ($name: ident) => {
            ctx.defun(stringify!($name), |arg: TulispObject| -> bool {
                arg.$name()
            });
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

    ctx.defspecial("declare", |_ctx, _args| {
        // no-op
        Ok(TulispObject::nil())
    });

    ctx.defspecial("defvar", |ctx, args| {
        destruct_bind!((name &optional initval _docstring) = args);
        if !name.symbolp() {
            return Err(Error::type_mismatch(
                "defvar: first argument must be a symbol".to_string(),
            ));
        }
        if !name.boundp() {
            let val = ctx.eval(&initval)?;
            name.set(val)?;
        }
        Ok(name)
    });
}
