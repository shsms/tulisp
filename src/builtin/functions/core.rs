use super::print_to_stdout;
use crate::Rest;
use crate::TulispObject;
use crate::TulispValue;
use crate::context::TulispContext;
use crate::error::Error;
use crate::list;
use crate::object::wrappers::generic::SharedMut;

/// Defines the macro a `(defmacro NAME PARAMS [DOC] BODY...)` form's
/// ARGS describe, and returns NAME. The parameter list is checked now;
/// the body compiles at the first expansion.
pub(crate) fn define_macro(
    ctx: &mut TulispContext,
    args: &TulispObject,
) -> Result<TulispObject, Error> {
    let (name, params, body): (TulispObject, TulispObject, Rest<TulispObject>) =
        args.destructure(ctx)?;
    crate::builtin::check_param_list(ctx, &params)?;
    let body = crate::builtin::drop_declare_after_docstring(ctx, body.into())?;
    let lambda = list!(,ctx.keywords.lambda.clone() ,params ,@body)?;
    ctx.set_function_value(
        &name,
        TulispValue::Defmacro {
            lambda,
            compiled: SharedMut::new(None),
        }
        .into_ref(None),
        None,
    )?;
    Ok(name)
}

/// Evaluates FILENAME, found under the load path when one is set, as Emacs
/// Lisp's `load` does, and returns t. With NOERROR, a file that cannot be
/// opened, or a directory, gives nil instead of an error; a file that is not
/// UTF-8 text, or an error in its code, still raises.
fn load_file(
    ctx: &mut TulispContext,
    filename: &str,
    noerror: bool,
) -> Result<TulispObject, Error> {
    let full_path = if let Some(ref load_path) = ctx.load_path {
        load_path.join(filename)
    } else {
        std::path::PathBuf::from(filename)
    };
    let Some(full_path) = full_path.to_str() else {
        return Err(Error::invalid_argument(format!(
            "load: Invalid path: {}",
            full_path.to_string_lossy()
        )));
    };
    let file = match crate::context::open_source_file(full_path) {
        Ok(file) if noerror && file.metadata().is_ok_and(|meta| meta.is_dir()) => {
            return Ok(TulispObject::nil());
        }
        Ok(file) => file,
        Err(_) if noerror => return Ok(TulispObject::nil()),
        Err(err) => return Err(err),
    };
    let contents = crate::context::read_source(full_path, file)?;
    let forms = ctx.parse_file_text(full_path, &contents)?;
    ctx.eval_progn(&forms)?;
    Ok(TulispObject::t())
}

/// Refuses a non-nil ENVIRONMENT, which Tulisp's macro expansion does not
/// support.
fn check_no_macro_environment(name: &str, environment: Option<TulispObject>) -> Result<(), Error> {
    match environment {
        Some(environment) => Err(Error::not_implemented(format!(
            "{name}: ENVIRONMENT is not supported, got: {environment}"
        ))),
        None => Ok(()),
    }
}

pub(crate) fn add(ctx: &mut TulispContext) {
    // NOMESSAGE, NOSUFFIX and MUST-SUFFIX are taken and ignored.
    ctx.defun(
        "load",
        |ctx: &mut TulispContext,
         filename: String,
         noerror: Option<TulispObject>,
         _nomessage: Option<TulispObject>,
         _nosuffix: Option<TulispObject>,
         _must_suffix: Option<TulispObject>| {
            load_file(ctx, &filename, noerror.is_some())
        },
    );

    ctx.defun(
        "intern",
        |ctx: &mut TulispContext, name: String| -> TulispObject { ctx.intern(&name) },
    );

    ctx.defun(
        "symbol-value",
        |sym: TulispObject| -> Result<TulispObject, Error> {
            if !sym.symbolp() {
                return Err(Error::wrong_type_argument(
                    "symbolp",
                    sym.clone(),
                    format!("symbol-value: expected a symbol, got {sym}"),
                ));
            }
            sym.get()
        },
    );

    ctx.defun("symbol-name", |symbol: TulispObject| symbol.symbol_name());

    // Asks the symbol's own global value, so a symbol `make-symbol` made is
    // asked about itself, not about the symbol interned under its name.
    ctx.defun("fboundp", |symbol: TulispObject| {
        if !symbol.symbolp() {
            return Err(Error::wrong_type_argument(
                "symbolp",
                symbol.clone(),
                format!("Expected symbol, got: {symbol}"),
            ));
        }
        Ok(symbol
            .global()
            .is_some_and(|value| value.inner_ref().0.is_fbound()))
    });

    ctx.defun("identity", |arg: TulispObject| arg);

    ctx.defun("ignore", |_arguments: crate::Rest<TulispObject>| ());

    ctx.defun("zerop", |number: TulispObject| {
        Ok::<_, Error>(number_of(&number)? == 0)
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
                push_text(&mut ret, &ele)?;
            }
            Ok(ret)
        },
    );

    ctx.defun(
        "format",
        |in_string: String, rest: crate::Rest<TulispObject>| -> Result<String, Error> {
            super::format::format_string(&in_string, rest)
        },
    );

    ctx.defun(
        "print",
        |val: TulispObject| -> Result<TulispObject, Error> {
            // Deliberately NOT Emacs's `print` (newline before and
            // after): a plain value-plus-newline, like print functions
            // in modern languages.
            print_to_stdout(&val.fmt_string(), true)?;
            Ok(val)
        },
    );

    ctx.defun(
        "prin1-to-string",
        |arg: TulispObject, noescape: Option<bool>| -> String {
            if noescape.unwrap_or_default() {
                arg.fmt_string()
            } else {
                arg.to_string()
            }
        },
    );

    ctx.defun(
        "princ",
        |val: TulispObject| -> Result<TulispObject, Error> {
            // Emacs `princ`: no newline; scripts emit their own via "\n".
            print_to_stdout(&val.fmt_string(), false)?;
            Ok(val)
        },
    );

    ctx.define_special_form("while");

    ctx.define_special_form("setq");

    ctx.defun(
        "set",
        |name: TulispObject, value: TulispObject| -> Result<TulispObject, Error> {
            name.set(value.clone())?;
            Ok(value)
        },
    );

    ctx.define_special_form("let");
    ctx.define_special_form("let*");

    ctx.define_special_form("progn");

    ctx.define_special_form("defun");

    ctx.define_special_form("lambda");
    ctx.define_special_form("function");

    ctx.define_special_form("defmacro");

    ctx.defun("null", |arg: TulispObject| -> bool { arg.null() });

    ctx.defun(
        "eval",
        |ctx: &mut TulispContext,
         form: TulispObject,
         _lexical: Option<TulispObject>|
         -> Result<TulispObject, Error> { ctx.eval(&form) },
    );

    // (apply FUNCTION &rest ARGUMENTS) calls FUNCTION with the
    // intermediate ARGUMENTS plus the elements of the final one, a
    // list: (apply '+ 1 2 '(3 4)) => 10.
    ctx.defun(
        "apply",
        |ctx: &mut TulispContext, args: Rest<TulispObject>| -> Result<TulispObject, Error> {
            let mut args: Vec<TulispObject> = args.into_iter().collect();
            if args.len() < 2 {
                return Err(Error::missing_argument(
                    "apply requires at least 2 arguments".to_string(),
                ));
            }
            let func = args.remove(0);
            ctx.apply(&func, crate::eval::spread_apply_args(args)?)
        },
    );

    ctx.defun(
        "funcall",
        |ctx: &mut TulispContext,
         func: TulispObject,
         args: Rest<TulispObject>|
         -> Result<TulispObject, Error> {
            ctx.apply(&func, args.into_iter().collect::<Vec<_>>())
        },
    );

    // Each takes Emacs's optional ENVIRONMENT; only nil, the usual value, is
    // supported.
    ctx.defun(
        "macroexpand",
        |ctx: &mut TulispContext, form: TulispObject, environment: Option<TulispObject>| {
            check_no_macro_environment("macroexpand", environment)?;
            crate::eval::macroexpand(ctx, form)
        },
    );
    ctx.defun(
        "macroexpand-1",
        |ctx: &mut TulispContext, form: TulispObject, environment: Option<TulispObject>| {
            check_no_macro_environment("macroexpand-1", environment)?;
            crate::eval::macroexpand_1(ctx, form)
        },
    );
    ctx.defun(
        "macroexpand-all",
        |ctx: &mut TulispContext, form: TulispObject, environment: Option<TulispObject>| {
            check_no_macro_environment("macroexpand-all", environment)?;
            crate::eval::macroexpand_all(ctx, form)
        },
    );

    // List functions

    ctx.defun(
        "cons",
        |car: TulispObject, cdr: TulispObject| -> TulispObject { TulispObject::cons(car, cdr) },
    );

    ctx.defun(
        "append",
        |rest: crate::Rest<TulispObject>| -> Result<TulispObject, Error> {
            crate::lists::append(rest.into_iter())
        },
    );

    // `dolist` and `dotimes` are macros over `let` and `while`, as in
    // Emacs. Each iteration binds `var` afresh, so closures made in
    // different iterations see different values. The loop state lives
    // in uninterned symbols, which user code cannot name.
    ctx.defmacro("dolist", |ctx, args| {
        let (spec, body): (TulispObject, Rest<TulispObject>) = args.destructure(ctx)?;
        let (var, list, result): (TulispObject, TulispObject, Option<TulispObject>) =
            spec.destructure(ctx)?;
        let result = result.unwrap_or_default();
        crate::builtin::check_not_nil_or_t(&var)?;
        let tail = TulispObject::symbol("tail".to_string(), false);
        // (let ((tail list))
        //   (while tail
        //     (let ((var (car tail))) body...)
        //     (setq tail (cdr tail)))
        //   result)
        list!(,ctx.intern("let") ,list!(,list!(,tail.clone() ,list)?)?
              ,list!(,ctx.intern("while") ,tail.clone()
                     ,list!(,ctx.intern("let")
                            ,list!(,list!(,var ,list!(,ctx.intern("car") ,tail.clone())?)?)?
                            ,@body)?
                     ,list!(,ctx.intern("setq") ,tail.clone()
                            ,list!(,ctx.intern("cdr") ,tail)?)?)?
              ,result)
    });

    ctx.defmacro("dotimes", |ctx, args| {
        let (spec, body): (TulispObject, Rest<TulispObject>) = args.destructure(ctx)?;
        let (var, count, result): (TulispObject, TulispObject, Rest<TulispObject>) =
            spec.destructure(ctx)?;
        crate::builtin::check_not_nil_or_t(&var)?;
        let limit = TulispObject::symbol("limit".to_string(), false);
        let counter = TulispObject::symbol("counter".to_string(), false);
        // (let ((limit count) (counter 0))
        //   (while (< counter limit)
        //     (let ((var counter)) body...)
        //     (setq counter (+ counter 1)))
        //   (let ((var counter)) result...))  ; only with result forms
        let result = if result.is_empty() {
            TulispObject::nil()
        } else {
            list!(,list!(,ctx.intern("let") ,list!(,list!(,var.clone() ,counter.clone())?)?
                         ,@result)?)?
        };
        list!(,ctx.intern("let")
              ,list!(,list!(,limit.clone() ,count)? ,list!(,counter.clone() ,0)?)?
              ,list!(,ctx.intern("while")
                     ,list!(,ctx.intern("<") ,counter.clone() ,limit)?
                     ,list!(,ctx.intern("let") ,list!(,list!(,var ,counter.clone())?)?
                            ,@body)?
                     ,list!(,ctx.intern("setq") ,counter.clone()
                            ,list!(,ctx.intern("+") ,counter ,1)?)?)?
              ,@result)
    });

    ctx.defun("list", |args: crate::Rest<TulispObject>| -> TulispObject {
        args.into_iter().collect()
    });

    ctx.defun(
        "assoc",
        |ctx: &mut TulispContext,
         key: TulispObject,
         alist: TulispObject,
         testfn: Option<TulispObject>|
         -> Result<TulispObject, Error> { crate::alist::assoc(ctx, &key, &alist, testfn) },
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
            // REMOVE makes `(setf (alist-get ...) nil)` delete an entry,
            // and there is no `setf`, so a non-nil one is an error.
            if remove.is_some() {
                return Err(Error::not_implemented(
                    "alist-get: REMOVE argument is not implemented (no `setf` support yet)",
                ));
            }
            crate::alist::alist_get(ctx, &key, &alist, default_value, testfn)
        },
    );

    ctx.defun(
        "plist-get",
        |ctx: &mut TulispContext,
         plist: TulispObject,
         property: TulispObject,
         predicate: Option<TulispObject>|
         -> Result<TulispObject, Error> {
            match predicate {
                Some(predicate) => crate::plist::plist_get_by(ctx, &plist, &property, &predicate),
                None => crate::plist::plist_get(&plist, &property),
            }
        },
    );

    ctx.defun(
        "plist-put",
        |ctx: &mut TulispContext,
         plist: TulispObject,
         property: TulispObject,
         value: TulispObject,
         predicate: Option<TulispObject>| {
            crate::plist::plist_put(ctx, plist, property, value, predicate.as_ref())
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
    ctx.defun("atom", |arg: TulispObject| -> bool { !arg.consp() });
    ctx.defun(
        "functionp",
        |ctx: &mut TulispContext, arg: TulispObject| -> bool { arg.functionp(ctx) },
    );
    // predicates end

    ctx.define_special_form("declare");
    ctx.define_special_form("interactive");

    ctx.define_special_form("defvar");
}

/// Adds the text of OBJ to OUT, as `concat` takes it: a string, nil, or a
/// list of characters.
pub(crate) fn push_text(out: &mut String, obj: &TulispObject) -> Result<(), Error> {
    if let TulispValue::String { value, .. } = &obj.inner_ref().0 {
        out.push_str(value);
        return Ok(());
    }
    if !obj.listp() {
        return Err(Error::type_mismatch(format!("Not a string: {obj}")));
    }
    let mut iter = obj.base_iter();
    for item in iter.by_ref() {
        out.push(to_char(&item)?);
    }
    iter.take_error()
}

/// The number OBJ holds, or the error Emacs's arithmetic gives for any other
/// value.
pub(crate) fn number_of(obj: &TulispObject) -> Result<crate::Number, Error> {
    if !obj.numberp() {
        return Err(Error::wrong_type_argument(
            "number-or-marker-p",
            obj.clone(),
            format!("Expected number, got: {obj}"),
        ));
    }
    obj.inner_ref().0.as_number()
}

/// The character whose code is OBJ.
pub(crate) fn to_char(obj: &TulispObject) -> Result<char, Error> {
    obj.as_int()
        .ok()
        .and_then(|code| u32::try_from(code).ok())
        .and_then(char::from_u32)
        .ok_or_else(|| Error::type_mismatch(format!("Not a character: {obj}")))
}

#[cfg(test)]
mod tests {
    use crate::test_utils::{
        assert_results, eval_assert, eval_assert_equal, eval_assert_equal_fresh, eval_assert_error,
        eval_assert_error_line,
    };
    use crate::{Error, TulispContext, TulispObject};

    #[test]
    fn identity_ignore_and_zerop() {
        assert_results(&[
            ("(identity 3)", "3"),
            ("(list (ignore 1 2) (ignore))", "(nil nil)"),
            (
                "(list (zerop 0) (zerop 0.0) (zerop -0.0) (zerop 1) (zerop 1.5))",
                "(t t t nil nil)",
            ),
            (
                "(zerop 'a)",
                "(ERR (wrong-type-argument number-or-marker-p a))",
            ),
            (
                r#"(zerop "0")"#,
                r#"(ERR (wrong-type-argument number-or-marker-p "0"))"#,
            ),
        ]);
    }

    #[test]
    fn symbol_name_and_fboundp() {
        assert_results(&[
            (
                "(list (symbol-name 'abc) (symbol-name nil) (symbol-name :k))",
                r#"("abc" "nil" ":k")"#,
            ),
            (
                r#"(symbol-name "a")"#,
                r#"(ERR (wrong-type-argument symbolp "a"))"#,
            ),
            (
                "(list (fboundp 'car) (fboundp 'when) (fboundp 'if) (fboundp 'no-such))",
                "(t t t nil)",
            ),
            ("(list (fboundp nil) (fboundp :k))", "(nil nil)"),
            ("(fboundp 5)", "(ERR (wrong-type-argument symbolp 5))"),
            // A `let` binding is not a function definition.
            ("(let ((f (lambda () 1))) (fboundp 'f))", "nil"),
            ("(progn (setq x5 5) (fboundp 'x5))", "nil"),
            // Unlike in Emacs, a function and a variable share one cell.
            ("(progn (setq vv (lambda () 1)) (fboundp 'vv))", "t"),
        ]);
    }

    // `fboundp` asks the symbol it is given, so a symbol `make-symbol` made is
    // not taken for the interned one of the same name.
    #[test]
    fn fboundp_asks_an_uninterned_symbol_about_itself() {
        assert_results(&[(
            r#"(let ((s (make-symbol "car"))) (list (fboundp s) (fboundp 'car)))"#,
            "(nil t)",
        )]);
    }

    // `alist-get` refuses a non-nil REMOVE, which needs `setf`.
    #[test]
    fn alist_get_refuses_remove() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(alist-get 'a '((a . 1)) nil nil)", "1");
        eval_assert_error_line(
            ctx,
            "(alist-get 'a '((a . 1)) nil t)",
            "ERR NotImplemented: alist-get: REMOVE argument is not implemented (no `setf` support yet)",
        );
    }

    // A Lisp macro that replaces a function reaches the calls compiled
    // to the function: they raise, as calling a macro does in Emacs.
    #[test]
    fn a_defmacro_over_a_function_reaches_compiled_calls() {
        let ctx = &mut TulispContext::new();
        ctx.defun("r", || 1);
        ctx.eval_string("(defun call-r () (list (r))) (call-r)")
            .unwrap();
        ctx.eval_string("(defmacro r () 5)").unwrap();
        eval_assert_error_line(ctx, "(call-r)", "ERR InvalidArgument: invalid function: r");
    }

    // Errors in the body of a dolist or dotimes keep their source
    // positions.
    #[test]
    fn errors_in_loop_bodies_keep_their_positions() {
        let ctx = &mut TulispContext::new();
        eval_assert_error(
            ctx,
            "(dolist (x (list 1)) (car 5))",
            "ERR TypeMismatch: Expected list, got: 5\n\
             <eval_string>:1.22-1.28:  at (car 5)\n\
             <eval_string>:1.1-1.29:  at (let ((tail (list 1))) (while tail (let ((x (car tail))) (car 5)) (setq tail (cd...\n",
        );
        eval_assert_error(
            ctx,
            "(dotimes (i 1) (car 5))",
            "ERR TypeMismatch: Expected list, got: 5\n\
             <eval_string>:1.16-1.22:  at (car 5)\n\
             <eval_string>:1.1-1.23:  at (let ((limit 1) (counter 0)) (while (< counter limit) (let ((i counter)) (car 5)...\n",
        );
    }

    // A loop spec that is not a list raises the list walk's error; one
    // with too many elements is a call error.
    #[test]
    fn malformed_loop_specs_are_errors() {
        let ctx = &mut TulispContext::new();
        eval_assert_error_line(
            ctx,
            "(dolist x 1)",
            "ERR TypeMismatch: Expected list, got: x",
        );
        eval_assert_error_line(
            ctx,
            "(dotimes i 1)",
            "ERR TypeMismatch: Expected list, got: i",
        );
        eval_assert_error_line(
            ctx,
            "(dolist (x '(1) nil 4) x)",
            "ERR ArityMismatch: Too many arguments",
        );
    }

    // `eval` runs a form; a `let` variable is gone after its `let`.
    #[test]
    fn eval_runs_a_form() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(eval '(mod 32 5))", "2");
        eval_assert_error(
            ctx,
            "(let ((j 10)) (+ j j))(+ j 1)",
            "ERR Uninitialized: Variable definition is void: j\n\
             <eval_string>:1.23-1.29:  at (+ j 1)\n",
        );
    }

    // A built-in special form's symbol holds a marker that is not a
    // function, and not `equal` to another special form's.
    #[test]
    fn a_special_form_holds_a_marker() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(format \"%s\" (symbol-value 'if))", "\"SpecialForm\"");
        eval_assert_equal(ctx, "(equal (symbol-value 'if) (symbol-value 'let))", "nil");
        eval_assert_error_line(
            ctx,
            "(funcall (symbol-value 'if) t 1 2)",
            "ERR InvalidArgument: invalid function: SpecialForm",
        );
        eval_assert_error_line(
            ctx,
            "(setq my-if (symbol-value 'if)) (my-if t 1 2)",
            "ERR InvalidArgument: invalid function: my-if",
        );
        // Once `my-if` holds the marker, a call to it is refused when it
        // compiles, so none of the program runs.
        eval_assert_error_line(
            ctx,
            "(setq my-if-ran t) (my-if t 1 2)",
            "ERR InvalidArgument: invalid function: my-if",
        );
        eval_assert_equal(ctx, "(condition-case nil my-if-ran (error 'void))", "'void");
        eval_assert_error_line(
            ctx,
            "(mapcar 'if '(1 2))",
            "ERR InvalidArgument: invalid function: if",
        );
        let if_ = ctx.intern("if");
        let err = ctx.funcall(&if_, (1i64, 2i64)).unwrap_err().to_string();
        assert_eq!(
            err.lines().next(),
            Some("ERR InvalidArgument: invalid function: if"),
            "{err}"
        );
    }

    // A special form is not a function, as in Emacs; `funcall` and
    // `apply` themselves stay callable.
    #[test]
    fn funcall_and_apply_refuse_special_forms() {
        let ctx = &mut TulispContext::new();
        let err = "ERR InvalidArgument: invalid function: if";
        eval_assert_error_line(ctx, "(funcall 'if t 1 2)", err);
        eval_assert_error_line(ctx, "(apply 'if '(t 1 2))", err);
        eval_assert_equal(ctx, "(funcall 'funcall '+ 1 2)", "3");
        eval_assert_equal(ctx, "(funcall 'apply '+ '(1 2))", "3");
        eval_assert_equal(ctx, "(apply 'funcall '(+ 1 2))", "3");
        eval_assert_equal(ctx, "(apply 'apply '+ '((1 2)))", "3");
        let if_sym = ctx.intern("if");
        let err = ctx.funcall(&if_sym, (true, 1, 2)).unwrap_err();
        assert!(err.to_string().contains("invalid function: if"));
        let funcall = ctx.intern("funcall");
        let plus = ctx.intern("+");
        assert_eq!(
            ctx.funcall(&funcall, (plus, 1, 2)).unwrap().to_string(),
            "3"
        );
        // They are functions, so any caller of a function can take them.
        let thunks = ctx
            .eval_string("(list (lambda () 1) (lambda () 2))")
            .unwrap();
        assert_eq!(ctx.map(&funcall, &thunks).unwrap().to_string(), "(1 2)");
        let err = ctx.funcall(&funcall, ()).unwrap_err();
        assert!(err.to_string().contains("Too few arguments"));
    }

    // `eval` takes Emacs's optional LEXICAL argument and ignores it.
    #[test]
    fn eval_takes_an_optional_lexical_argument() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(eval '(+ 1 2))", "3");
        eval_assert_equal(ctx, "(eval '(+ 1 2) t)", "3");
        eval_assert_equal(ctx, "(eval '(+ 1 2) nil)", "3");
    }

    // `eval` of a `(lambda ...)` form gives a function value, not the
    // list.
    #[test]
    fn eval_builtin_makes_a_function_of_a_lambda() {
        let ctx = &mut TulispContext::new();
        let value = ctx.eval_string("(eval '(lambda (x) x))").unwrap();
        assert!(value.inner_ref().0.is_function_value());
    }

    #[test]
    fn funcall_accepts_symbols_and_lambdas() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(setq f 'list)(funcall f 'x 'y 'z)", "'(x y z)");
        eval_assert_equal(ctx, "(funcall '1+ 4)", "5");
        eval_assert_equal(ctx, "(funcall '+ 4 5)", "9");
        eval_assert_equal(ctx, "(let ((x 10)) (funcall '+ x 2))", "12");
        eval_assert_equal(
            ctx,
            "(let ((x 10)) (funcall (lambda (x y) (+ x y)) x 2))",
            "12",
        );
        eval_assert_equal(
            ctx,
            "(let ((x 10)) (funcall '(lambda (x y) (+ x y)) x 2))",
            "12",
        );
        eval_assert_error(
            ctx,
            "(let ((y j) (j 10)) (funcall j))",
            "ERR Uninitialized: Variable definition is void: j\n\
             <eval_string>:1.1-1.32:  at (let ((y j) (j 10)) (funcall j))\n",
        );
    }

    #[test]
    fn funcall_does_not_evaluate_a_quoted_list() {
        let ctx = &mut TulispContext::new();
        // Only a `(lambda ...)` list is a function. Any other list is
        // rejected as-is, without running it.
        eval_assert_equal(
            ctx,
            "(setq zz 0)
             (condition-case nil (funcall '(progn (setq zz 1) 'car) '(1)) (error nil))
             zz",
            "0",
        );
        eval_assert_error(
            ctx,
            "(funcall '(progn 1))",
            "ERR Undefined: function is void: (progn 1)\n\
             <eval_string>:1.1-1.20:  at (funcall '(progn 1))\n",
        );
        eval_assert_error(
            ctx,
            "(apply ''car '((9)))",
            "ERR Undefined: function is void: 'car\n\
             <eval_string>:1.1-1.20:  at (apply ''car '((9)))\n",
        );
    }

    // Emacs rejects a macro as `(invalid-function m)`.
    #[test]
    fn funcall_rejects_a_macro() {
        let ctx = &mut TulispContext::new();
        eval_assert_error(
            ctx,
            "(defmacro m (x) (list '+ x 1)) (funcall 'm 2)",
            "ERR InvalidArgument: invalid function: m\n\
             <eval_string>:1.32-1.45:  at (funcall 'm 2)\n",
        );
        eval_assert_error(
            ctx,
            "(apply 'when '(t 1))",
            "ERR InvalidArgument: invalid function: when\n\
             <eval_string>:1.1-1.20:  at (apply 'when '(t 1))\n",
        );
    }

    #[test]
    fn apply_splices_the_last_argument() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(apply '+ '(1 2 3))", "6");
        // `+` needs at least one argument in tulisp; `list` does not.
        eval_assert_equal(ctx, "(apply 'list '())", "nil");
        eval_assert_equal(ctx, "(apply '+ 1 2 '(3 4))", "10");
        eval_assert_equal(ctx, "(apply 'list 1 2 '(3 4))", "'(1 2 3 4)");
        eval_assert_equal(ctx, "(apply 'concat \"a\" '(\"b\" \"c\"))", "\"abc\"");
        eval_assert_equal(
            ctx,
            "(setq f (lambda (a b c) (+ a (* b c))))(apply f 1 '(2 3))",
            "7",
        );
        eval_assert_equal(ctx, "(apply (lambda (a b) (* a b)) 3 '(4))", "12");
        eval_assert_equal(ctx, "(apply '(lambda (a b) (* a b)) '(3 4))", "12");
        // The arguments before the list are evaluated too.
        eval_assert_equal(ctx, "(let ((x 10)) (apply '+ x '(2 3)))", "15");
    }

    #[test]
    fn apply_rejects_a_circular_argument_list() {
        let ctx = &mut TulispContext::new();
        // `(apply '+ 1 2 1 2 ...)`, built at run time for `eval`.
        eval_assert_error_line(
            ctx,
            "(let ((form (list 'apply ''+ 1 2)))
               (setcdr (cdddr form) (cddr form))
               (eval form))",
            "ERR OutOfRange: Circular list",
        );
    }

    #[test]
    fn apply_rejects_a_bad_last_argument() {
        let ctx = &mut TulispContext::new();
        eval_assert_error(
            ctx,
            "(apply '+ 1 2)",
            "ERR TypeMismatch: apply: last argument must be a list, got: 2\n\
             <eval_string>:1.1-1.14:  at (apply '+ 1 2)\n",
        );
        // A dotted tail is an error, as in Emacs, not silently dropped.
        eval_assert_error(
            ctx,
            "(apply '+ 1 '(2 3 . 4))",
            "ERR TypeMismatch: apply: last argument must be a proper list, got non-nil tail: 4\n\
             <eval_string>:1.1-1.23:  at (apply '+ 1 '(2 3 . 4))\n",
        );
        // A circular list is an error instead of an endless splice.
        eval_assert_error(
            ctx,
            r#"
            (setq xs (list 1 2 3))
            (setcdr (cdr (cdr xs)) xs)
            (apply '+ xs)
        "#,
            "ERR OutOfRange: Circular list\n\
             <eval_string>:4.13-4.25:  at (apply '+ xs)\n",
        );
        eval_assert_error(
            ctx,
            "(apply)",
            "ERR MissingArgument: apply requires at least 2 arguments\n\
             <eval_string>:1.1-1.7:  at (apply)\n",
        );
        eval_assert_error(
            ctx,
            "(apply '+)",
            "ERR MissingArgument: apply requires at least 2 arguments\n\
             <eval_string>:1.1-1.10:  at (apply '+)\n",
        );
    }

    #[test]
    fn defun_and_defmacro_return_the_interned_name() {
        let ctx = &mut TulispContext::new();
        eval_assert(ctx, "(eq (defun q () 1) 'q)");
        eval_assert(ctx, "(eq (defmacro m () 1) 'm)");
    }

    #[test]
    fn lambda_rejects_a_circular_body() {
        let ctx = &mut TulispContext::new();
        // `(list x 1 x 1 ...)`, built at run time for `eval`.
        eval_assert_error_line(
            ctx,
            "(let ((form (list 'list 'x 1)))
               (setcdr (cddr form) (cdr form))
               (funcall (eval (list 'lambda '(x) form)) 1))",
            "ERR OutOfRange: Circular list",
        );
    }

    #[test]
    fn a_closure_captures_a_variable_used_only_in_a_dotted_tail() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(funcall (let ((x 1)) (lambda () `(a . ,x))))",
            "'(a . 1)",
        );
        eval_assert_equal(
            ctx,
            "(let (fns)
               (dolist (x '(1 2)) (setq fns (cons (lambda () `(a . ,x)) fns)))
               (mapcar #'funcall fns))",
            "'((a . 2) (a . 1))",
        );
    }

    #[test]
    fn let_rejects_a_circular_varlist() {
        let ctx = &mut TulispContext::new();
        // Only a macro can hand `let` a list that loops.
        eval_assert_error(
            ctx,
            "(defmacro m () (let ((vl (list '(a 1)))) (setcdr vl vl) (list 'let vl 1)))
             (m)",
            "ERR OutOfRange: Circular list\n",
        );
    }

    #[test]
    fn while_runs_until_the_condition_is_nil() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(let ((vv 0)) (while (< vv 42) (setq vv (+ 1 vv))) vv)",
            "42",
        );
    }

    #[test]
    fn consp_is_true_only_for_a_cons() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(consp '(20))", "t");
        eval_assert_equal(ctx, "(consp '20)", "nil");
    }

    #[test]
    fn keywordp_is_true_only_for_a_keyword() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(list (keywordp :a) (keywordp ':abcd) (keywordp 'abcd) (keywordp nil))",
            "'(t t nil nil)",
        );
    }

    // Expected values below were checked against GNU Emacs 30.1.

    #[test]
    fn symbol_predicates() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            r#"(list (symbolp nil) (symbolp t) (symbolp :a) (symbolp 'a)
                     (symbolp "a") (symbolp 1) (symbolp '(a)))"#,
            "'(t t t t nil nil nil)",
        );
        // A predicate looks at the value of a lexical variable.
        eval_assert_equal(
            ctx,
            "(let ((x 'y) (z 1)) (list (symbolp 'z) (symbolp x) (symbolp z)))",
            "'(t t nil)",
        );
        eval_assert_equal(
            ctx,
            r#"(list (keywordp nil) (keywordp t) (keywordp :a) (keywordp 'a)
                     (keywordp ":a") (keywordp (intern ":a")))"#,
            "'(nil nil t nil nil t)",
        );
        // Differs from Emacs, which gives nil: only interned symbols
        // are keywords there, but here any symbol whose name starts
        // with a colon is one.
        eval_assert(ctx, r#"(keywordp (make-symbol ":a"))"#);
    }

    #[test]
    fn setq_and_set_reject_a_target_that_is_not_a_variable() {
        // `setq` rejects non-symbol and constant-symbol targets at compile
        // time; `set` rejects them at runtime. Regression: the VM used to
        // `.unwrap()` the result of `obj.set(...)`, which crashed on
        // `(setq t 5)`, `(setq nil 5)`, `(setq :foo 5)`, etc.
        let ctx = &mut TulispContext::new();
        eval_assert_error(
            ctx,
            "(setq t 5)",
            r#"ERR SettingConstant: Can't set constant symbol: t
<eval_string>:1.7-1.7:  at t
<eval_string>:1.1-1.10:  at (setq t 5)
"#,
        );
        eval_assert_error(
            ctx,
            "(setq nil 5)",
            r#"ERR SettingConstant: Can't set constant symbol: nil
<eval_string>:1.7-1.9:  at nil
<eval_string>:1.1-1.12:  at (setq nil 5)
"#,
        );
        eval_assert_error(
            ctx,
            "(setq :foo 5)",
            r#"ERR SettingConstant: Can't set constant symbol: :foo
<eval_string>:1.1-1.13:  at (setq :foo 5)
"#,
        );
        eval_assert_error(
            ctx,
            r#"(setq "x" 5)"#,
            r#"ERR TypeMismatch: Expected Symbol: Can't assign to "x"
<eval_string>:1.1-1.12:  at (setq "x" 5)
"#,
        );
        eval_assert_error(
            ctx,
            "(set 't 5)",
            r#"ERR SettingConstant: Can't set constant symbol: t
<eval_string>:1.7-1.7:  at t
<eval_string>:1.1-1.10:  at (set 't 5)
"#,
        );
        eval_assert_error(
            ctx,
            "(set ':foo 5)",
            r#"ERR SettingConstant: Can't set constant symbol: :foo
<eval_string>:1.1-1.13:  at (set ':foo 5)
"#,
        );
    }

    #[test]
    fn dolist_returns_its_result_form() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(let ((res 0)) (dolist (vv '(20 30 50 33) res) (setq res (+ res vv))))",
            "133",
        );
    }

    #[test]
    fn dolist_binds_a_fresh_variable_per_iteration() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            r#"
        (setq fns nil)
        (dolist (i '(1 2 3))
          (setq fns (cons (lambda () i) fns)))
        (mapcar 'funcall fns)
        "#,
            "'(3 2 1)",
        );
    }

    #[test]
    fn dotimes_binds_a_fresh_variable_per_iteration() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            r#"
        (setq fns nil)
        (dotimes (j 3)
          (setq fns (cons (lambda () j) fns)))
        (mapcar 'funcall fns)
        "#,
            "'(2 1 0)",
        );
    }

    #[test]
    fn dotimes_evaluates_its_count() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(defun g (n) (let ((s 0)) (dotimes (i n) (setq s (+ s i))) s)) (g 4)",
            "6",
        );
        eval_assert_equal(
            ctx,
            "(let ((s 0)) (dotimes (i 2.5) (setq s (1+ s))) s)",
            "3",
        );
        eval_assert_error_line(
            ctx,
            r#"(dotimes (i "a") 1)"#,
            r#"ERR TypeMismatch: Expected number, got: "a""#,
        );
    }

    #[test]
    fn dotimes_runs_every_result_form() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(let ((x 0)) (list (dotimes (i 2 (setq x i) 7)) x))",
            "'(7 2)",
        );
    }

    #[test]
    fn dotimes_returns_nil_or_its_result_form() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(let ((res 0)) (list (dotimes (vv 4) (setq res (+ res vv))) res))",
            "'(nil 6)",
        );
        eval_assert_equal(
            ctx,
            "(let ((res 0)) (list (dotimes (vv 4 res) (setq res (+ res vv))) res))",
            "'(6 6)",
        );
    }

    #[test]
    fn dotimes_binds_its_variable_to_the_count_in_the_result_form() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(let ((i 7)) (dotimes (i 3 i)))", "3");
        eval_assert_equal(ctx, "(dotimes (i 2.5 i))", "3");
        eval_assert_equal(ctx, "(dotimes (i -2 i))", "0");
        eval_assert_equal(ctx, "(let ((i 7)) (dotimes (i 3 i)) i)", "7");
        eval_assert_equal(ctx, "(funcall (dotimes (i 2 (lambda () i))))", "2");
        eval_assert_equal(
            ctx,
            "(let ((i 5)) (funcall (lambda () (dotimes (i 2 i)))))",
            "2",
        );
        eval_assert_equal(
            ctx,
            "(let ((i 5)) (funcall (funcall (lambda () (dotimes (i 2 (lambda () i)))))))",
            "2",
        );
        eval_assert_equal(
            ctx,
            r#"(let ((i 9)) (condition-case nil (dotimes (i 2 (error "x"))) (error nil)) i)"#,
            "9",
        );
    }

    #[test]
    fn an_unused_loop_result_form_still_runs() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(setq x 0) (dolist (e '(1) (setq x 5))) x", "5");
        eval_assert_equal(
            ctx,
            "(defun h () (dolist (e '(1) (setq x 1))) (dotimes (i 2 (setq y 2))) nil)
             (setq x 0) (setq y 0) (h) (list x y)",
            "'(1 2)",
        );
    }

    #[test]
    fn a_closure_captures_a_variable_used_only_in_a_loop_result() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(let ((f (let ((x 1)) (lambda () (dolist (e '(1) x)))))) (funcall f))",
            "1",
        );
        eval_assert_equal(
            ctx,
            "(let ((f (let ((x 1)) (lambda () (dolist (x '(5) x)))))) (funcall f))",
            "1",
        );
        eval_assert_equal(
            ctx,
            "(let ((f (let ((x 2)) (lambda () (dotimes (e 1 x)))))) (funcall f))",
            "2",
        );
    }

    #[test]
    fn a_loop_does_not_see_user_variables_named_like_its_state() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(let ((tail 5)) (dolist (x '(1 2) tail) (setq tail (+ tail x))))",
            "8",
        );
        eval_assert_equal(
            ctx,
            "(let ((counter 5) (limit 9))
               (dotimes (i 2 (list counter limit)) (setq counter (1+ counter))))",
            "'(7 9)",
        );
    }

    #[test]
    fn defvar_sets_only_an_unbound_symbol() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(defvar foo-new 42) foo-new", "42");
        eval_assert_equal(
            ctx,
            "(setq foo-existing 1) (defvar foo-existing 99) foo-existing",
            "1",
        );
        eval_assert_equal(ctx, r#"(defvar foo-doc 7 "docs") foo-doc"#, "7");
        eval_assert_equal(ctx, "(defvar foo-sym 1)", "'foo-sym");
    }

    #[test]
    fn binding_nil_or_t_is_an_error() {
        let ctx = &mut TulispContext::new();
        for (form, name) in [
            ("(let ((t 1)) t)", "t"),
            ("(let ((nil 1)) 1)", "nil"),
            ("(let ((nil)) 1)", "nil"),
            ("(let (t) 1)", "t"),
            ("(let* (nil) 1)", "nil"),
            ("(let (()) 1)", "nil"),
            ("(dolist (nil '(1)) 2)", "nil"),
            ("(dotimes (t 2) 1)", "t"),
            // Differs from Emacs, which returns nil when the loop runs
            // no iterations.
            ("(dolist (nil nil) 1)", "nil"),
            ("(dotimes (t 0))", "t"),
            ("(defvar t 1)", "t"),
            ("(defun f (t) 1)", "t"),
            ("(defmacro m (a nil) a)", "nil"),
            ("(funcall (lambda (t) 1) 2)", "t"),
            ("(funcall (lambda (&optional t) 1))", "t"),
            ("(funcall (lambda (a &rest nil) a) 1)", "nil"),
            // Differs from Emacs, where a function named `t` is fine:
            // a symbol has one value slot here.
            ("(defun t () 1)", "t"),
            // A defun a macro builds is checked when the VM compiles
            // it, whatever its name.
            ("(defmacro mk () (list 'defun :k '(t) 1)) (mk)", "t"),
        ] {
            eval_assert_error_line(
                ctx,
                form,
                &format!("ERR SettingConstant: Can't set constant symbol: {name}"),
            );
        }
        // A second parameter after `&rest` is its own error, whatever
        // its name.
        eval_assert_error_line(
            ctx,
            "(funcall (lambda (&rest a t) 1))",
            "ERR TypeMismatch: Too many &rest parameters",
        );
    }

    #[test]
    fn symbol_value_of_nil_t_and_keywords_is_themselves() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(list (symbol-value nil) (symbol-value t) (symbol-value :a)
                   (let ((x 'nil)) (symbol-value x)))",
            "'(nil t :a nil)",
        );
        eval_assert_equal(ctx, "(setq v 1) (symbol-value 'v)", "1");
        eval_assert_error_line(
            ctx,
            "(symbol-value 1)",
            "ERR TypeMismatch: symbol-value: expected a symbol, got 1",
        );
    }

    // nil, t and keywords are always bound, to themselves, as in Emacs.
    #[test]
    fn nil_t_and_keywords_are_bound() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(list (boundp nil) (boundp t) (boundp :k) (boundp 'zzz) (boundp 5))",
            "'(t t t nil nil)",
        );
    }

    #[test]
    fn list_predicates() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            r#"(list (listp nil) (listp t) (listp '(1)) (listp (cons 1 2))
                     (listp 1) (listp "a") (listp 'a) (listp '(quote a)))"#,
            "'(t nil t t nil nil nil t)",
        );
        eval_assert_equal(
            ctx,
            r#"(list (atom nil) (atom t) (atom 1) (atom "a") (atom 'a)
                     (atom '(1)) (atom (cons 1 2)))"#,
            "'(t t t t t nil nil)",
        );
        // Differs from Emacs, where ''a is the list (quote a): the
        // reader here keeps a nested quote as a quote value, not a
        // cons, so it is an atom.
        eval_assert_equal(
            ctx,
            "(list (consp ''a) (listp ''a) (atom ''a))",
            "'(nil nil t)",
        );
    }

    #[test]
    fn number_and_string_predicates() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            r#"(list (numberp 1) (numberp 1.5) (numberp "1") (numberp nil)
                     (integerp 1) (integerp 1.0) (integerp 'a)
                     (floatp 1.0) (floatp 1) (floatp 1.0e+INF) (floatp 0.0e+NaN))"#,
            "'(t t nil nil t nil nil t nil t t)",
        );
        eval_assert_equal(
            ctx,
            r#"(list (stringp "") (stringp "a") (stringp 'a) (stringp nil)
                     (stringp 1))"#,
            "'(t t nil nil nil)",
        );
    }

    #[test]
    fn functionp_accepts_only_functions() {
        let ctx = &mut TulispContext::new();
        ctx.defun("rust-identity", |x: TulispObject| x);
        ctx.defspecial("rust-special", TulispObject::nil);
        eval_assert_equal(
            ctx,
            "(defun lisp-identity (x) x)
             (defmacro lisp-macro (x) x)
             (list (functionp 'lisp-identity) (functionp #'lisp-identity)
                   (functionp 'rust-identity) (functionp 'car)
                   (functionp (lambda (x) x)) (functionp '(lambda (x) x))
                   (let ((y 1)) (functionp (lambda (x) (+ x y)))))",
            "'(t t t t t t t)",
        );
        eval_assert_equal(
            ctx,
            r#"(list (functionp 'if) (functionp 'when) (functionp 'lisp-macro)
                     (functionp 'rust-special) (functionp 'no-such-function)
                     (functionp nil) (functionp t) (functionp :a)
                     (functionp 1) (functionp "car") (functionp '(1 2)))"#,
            "'(nil nil nil nil nil nil nil nil nil nil nil)",
        );
        // Binding `car` with `let` doesn't hide its function.
        eval_assert(ctx, "(let ((car 1)) (functionp 'car))");
        eval_assert(ctx, "(and (functionp 'apply) (functionp 'funcall))");
        // Differs from Emacs, which gives nil: a symbol has one value
        // slot here, shared by variables and functions, so a variable
        // holding a function names that function.
        eval_assert(ctx, "(setq fn-var (lambda (x) x)) (functionp 'fn-var)");
    }

    #[test]
    fn append_copies_every_list_but_the_last() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            r##"
        (let ((items (list 10 20)))
          (setq items
                (append items
                        '(30 40)
                        (list (+ 8 42) 60)))
          items)
        "##,
            "'(10 20 30 40 50 60)",
        );

        eval_assert_error(
            ctx,
            r#"
        (setq items
              (append items '(10)))
        "#,
            r#"ERR Uninitialized: Variable definition is void: items
<eval_string>:3.15-3.34:  at (append items '(10))
<eval_string>:2.9-3.35:  at (setq items (append items '(10)))
"#,
        );

        // Emacs `append` semantics: empty / single-arg / shared last arg /
        // dotted tail / no input mutation.
        eval_assert_equal(ctx, "(append)", "nil");
        eval_assert_equal(ctx, "(append '(1 2 3))", "'(1 2 3)");
        eval_assert_equal(ctx, "(append nil 77)", "77");
        eval_assert_equal(ctx, "(append '(1 2) 3)", "'(1 2 . 3)");
        eval_assert_equal(
            ctx,
            r##"
            (let ((xs '(1 2)))
              (append xs '(3 4))
              xs)
        "##,
            "'(1 2)",
        );
    }

    #[test]
    fn append_rejects_a_dotted_or_circular_list() {
        let ctx = &mut TulispContext::new();
        eval_assert_error(
            ctx,
            "(append '(1 . 2) nil)",
            "ERR TypeMismatch: Expected list, got: 2\n\
             <eval_string>:1.1-1.21:  at (append '(1 . 2) nil)\n",
        );
        // A value that is not a sequence names `sequencep`, as in Emacs.
        eval_assert_error(
            ctx,
            "(append 5 nil)",
            "ERR TypeMismatch: Expected sequence, got: 5\n\
             <eval_string>:1.1-1.14:  at (append 5 nil)\n",
        );
        // Only the last argument may loop: it is shared, not copied.
        eval_assert_error(
            ctx,
            "(let ((l (list 1 2 3))) (setcdr (cddr l) l) (append l nil))",
            "ERR OutOfRange: Circular list\n\
             <eval_string>:1.45-1.58:  at (append l nil)\n\
             <eval_string>:1.1-1.59:  at (let ((l (list 1 2 3))) (setcdr (cddr l) l) (append l nil))\n",
        );
        eval_assert_equal(
            ctx,
            "(let ((l (list 1 2 3))) (setcdr (cddr l) l) (nth 7 (append '(0) l)))",
            "1",
        );
    }

    #[test]
    fn test_load() -> Result<(), Error> {
        let mut ctx = TulispContext::new();

        // `load` returns t, whatever the file's last value, as in Emacs.
        eval_assert_equal(&mut ctx, r#"(load "tests/good-load.lisp")"#, "t");

        // The loaded file's definitions survive the load: a later call
        // reaches them, and so does a load inside a function body.
        eval_assert_equal(
            &mut ctx,
            "(list loaded-var (loaded-fn loaded-var))",
            "'(3 7)",
        );
        eval_assert_equal(
            &mut ctx,
            r#"(defun load-it () (load "tests/good-load.lisp"))
                    (list (load-it) (loaded-fn 1))"#,
            "'(t 5)",
        );

        // With NOERROR, a missing file gives nil; the other optional arguments
        // are taken and ignored, called directly or through `funcall`.
        eval_assert_equal(
            &mut ctx,
            r#"(list (load "tests/no-such-file.lisp" t)
                     (load "tests/good-load.lisp" nil t t t)
                     (funcall 'load "tests/no-such-file.lisp" t)
                     (funcall 'load "tests/good-load.lisp" nil t))"#,
            "'(nil t nil t)",
        );
        eval_assert_equal(&mut ctx, r#"(load "tests" t)"#, "nil");
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let path = std::env::temp_dir().join(format!(
                "tulisp_load_unreadable_{}.lisp",
                std::process::id()
            ));
            std::fs::write(&path, "(setq load-unreadable t)").unwrap();
            std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o000)).unwrap();
            // Root can still open the file, and then loads it.
            let unreadable = std::fs::File::open(&path).is_err();
            let result = ctx.eval_string(&format!("(load {:?} t)", path.to_str().unwrap()));
            std::fs::remove_file(&path).ok();
            assert_eq!(result?.null(), unreadable);
        }
        // A file that opens but cannot be read as text still raises.
        let path =
            std::env::temp_dir().join(format!("tulisp_load_not_utf8_{}.lisp", std::process::id()));
        std::fs::write(&path, b"(setq x \"\xff\")").unwrap();
        let result = ctx.eval_string(&format!("(load {:?} t)", path.to_str().unwrap()));
        std::fs::remove_file(&path).ok();
        assert!(result.is_err());

        eval_assert_error(
            &mut ctx,
            r#"(load "tests/bad-load.lisp")"#,
            r#"ERR ParsingError: Unexpected closing parenthesis
tests/bad-load.lisp:1.9-1.9:  at nil
<eval_string>:1.1-1.28:  at (load "tests/bad-load.lisp")
"#,
        );

        ctx.set_load_path(Some("tests/"))?;
        eval_assert_equal(&mut ctx, r#"(load "good-load.lisp")"#, "t");

        Ok(())
    }

    /// Emacs Lisp keeps function bindings and value bindings in separate
    /// namespaces. A `(let ((f x))` introduces a *value* binding on `f`;
    /// `(funcall 'f)` resolves the *function* binding (because the
    /// symbol arrives quoted, so funcall walks the function cell). The
    /// two should not interfere.
    ///
    /// Tulisp had a speculative todo entry (b2) flagging this as a place
    /// where lex-binding might diverge from Emacs under shadowing. The
    /// cases below verify each scenario — quoted symbol funcall, lambda-
    /// valued let binding, Rust-side ctx.defun, closure capture — and
    /// all behave as Emacs does. The entry can come out of todo.org;
    /// this test pins the behavior so a future lex-binding regression
    /// surfaces it.
    #[test]
    fn test_funcall_shadowing_keeps_namespaces_separate() -> Result<(), Error> {
        // (defun f) + (funcall 'f) under value-shadowing — function
        // binding wins.
        eval_assert_equal_fresh(
            "(defun f () 'global) (let ((f 'shadow)) (funcall 'f))",
            "'global",
        );
        // Even when the shadowing let-value is itself a callable, the
        // function binding still wins for quoted-symbol funcall.
        eval_assert_equal_fresh(
            "(defun f () 'global)
         (let ((f (lambda () 'shadow))) (funcall 'f))",
            "'global",
        );
        // A closure that references the symbol funcall'd by name resolves
        // the function cell at *call* time, not at lambda creation —
        // shadowing in the outer let doesn't reach into the closure.
        eval_assert_equal_fresh(
            "(defun f () 'global)
         (let ((g (lambda () (funcall 'f))))
           (let ((f 'shadow))
             (funcall g)))",
            "'global",
        );

        // Rust-side variant: ctx.defun-registered closure under value
        // shadowing. The Rust dispatch should still fire — function
        // cell vs value cell separation holds regardless of which side
        // registered the function.
        let mut ctx = TulispContext::new();
        ctx.defun("rust-f", || "rust-global".to_string());
        assert_eq!(
            ctx.eval_string("(let ((rust-f 'shadow)) (funcall 'rust-f))")?
                .to_string(),
            "\"rust-global\"",
        );
        Ok(())
    }

    #[test]
    fn prin1_to_string() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(prin1-to-string 'hello)", r#""hello""#);
        eval_assert_equal(ctx, "(prin1-to-string #'hello)", r#""hello""#);
        eval_assert_equal(ctx, "(prin1-to-string 25)", r#""25""#);
        eval_assert_equal(ctx, "(setq h 25)(prin1-to-string h)", r#""25""#);
        eval_assert_equal(
            ctx,
            "(setq h '(list 25 'hello))(prin1-to-string h)",
            r#""(list 25 'hello)""#,
        );
        eval_assert_equal(
            ctx,
            r#"(setq h "hello")(prin1-to-string h)"#,
            r#""\"hello\"""#,
        );
        eval_assert_equal(ctx, r#"(prin1-to-string "hello" t)"#, r#""hello""#);
        eval_assert_equal(ctx, r#"(prin1-to-string '("a") t)"#, r#""(a)""#);
        eval_assert_equal(ctx, r#"(prin1-to-string '("a") nil)"#, r#""(\"a\")""#);
    }

    #[test]
    fn concat_joins_strings() {
        let ctx = &mut TulispContext::new();
        eval_assert_error(
            ctx,
            "(concat 'hello 'world)",
            "ERR TypeMismatch: Not a string: hello
<eval_string>:1.1-1.22:  at (concat 'hello 'world)
",
        );
        eval_assert_equal(ctx, r#"(concat "hello" " world")"#, r#""hello world""#);
        eval_assert_equal(
            ctx,
            r#"(let ((hello "hello") (world "world")) (concat hello " " world))"#,
            r#""hello world""#,
        );
    }

    // `concat` takes nil and lists of characters too, as in Emacs.
    #[test]
    fn concat_takes_nil_and_character_lists() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, r#"(concat "x" nil "y")"#, r#""xy""#);
        eval_assert_equal(ctx, r#"(concat "a" '(98 99) '(233))"#, r#""abcé""#);
        eval_assert_error_line(
            ctx,
            r#"(concat "a" '(98 "c"))"#,
            r#"ERR TypeMismatch: Not a character: "c""#,
        );
        eval_assert_error_line(
            ctx,
            "(concat '(-1))",
            "ERR TypeMismatch: Not a character: -1",
        );
        eval_assert_error_line(ctx, "(concat 5)", "ERR TypeMismatch: Not a string: 5");
        eval_assert_error_line(
            ctx,
            "(concat '(4294967393))",
            "ERR TypeMismatch: Not a character: 4294967393",
        );
    }

    #[test]
    fn plist_put_sets_or_adds_a_property() {
        assert_results(&[
            ("(plist-put nil 'a 1)", "(a 1)"),
            (
                "(let ((p (list 'a 1 'b 2))) (list (plist-put p 'b 3) p))",
                "((a 1 b 3) (a 1 b 3))",
            ),
            ("(let ((p (list 'a 1))) (plist-put p 'c 3) p)", "(a 1 c 3)"),
            ("(let ((p (list 'a 1))) (eq p (plist-put p 'c 3)))", "t"),
            ("(plist-put (list 'a 1 'b 2) 'a 9)", "(a 9 b 2)"),
            (r#"(plist-put (list "a" 1) "a" 2 #'equal)"#, r#"("a" 2)"#),
            (r#"(plist-put (list "a" 1) "a" 2)"#, r#"("a" 1 "a" 2)"#),
        ]);
    }

    // A plist with a key and no value is an error that names the whole plist,
    // even when that key is the property.
    #[test]
    fn plist_put_rejects_a_plist_that_is_not_pairs() {
        assert_results(&[
            (
                "(plist-put (list 'a) 'b 1)",
                "(ERR (wrong-type-argument plistp (a)))",
            ),
            (
                "(plist-put (list 'a 1 'b) 'c 2)",
                "(ERR (wrong-type-argument plistp (a 1 b)))",
            ),
            (
                "(plist-put (list 'a 1 'b) 'b 2)",
                "(ERR (wrong-type-argument plistp (a 1 b)))",
            ),
            ("(plist-put 5 'a 1)", "(ERR (wrong-type-argument plistp 5))"),
            (
                "(plist-put (cons 'a 1) 'b 2)",
                "(ERR (wrong-type-argument plistp (a . 1)))",
            ),
        ]);
    }

    // Setting a constant is a `setting-constant` error naming the symbol, as in
    // Emacs.
    #[test]
    fn setting_a_constant_names_the_symbol() {
        assert_results(&[
            ("(set nil 1)", "(ERR (setting-constant nil))"),
            ("(set t 1)", "(ERR (setting-constant t))"),
            ("(set :k 1)", "(ERR (setting-constant :k))"),
            ("(eval '(setq nil 1) t)", "(ERR (setting-constant nil))"),
            (
                "(eval '(let ((nil 1)) 1) t)",
                "(ERR (setting-constant nil))",
            ),
            ("(add-to-list nil 1)", "(ERR (setting-constant nil))"),
            // Emacs lets `defvar` and `defun` take a keyword; Tulisp refuses it
            // as a constant.
            ("(eval '(defvar :k 1) t)", "(ERR (setting-constant :k))"),
            ("(eval '(defun :k () 1) t)", "(ERR (setting-constant :k))"),
            (
                "(condition-case e (set nil 1) (error (error-message-string e)))",
                r#""Attempt to set a constant symbol: nil""#,
            ),
        ]);
    }

    // Setting, reading or defining a value that is no symbol names `symbolp`,
    // as in Emacs.
    #[test]
    fn a_value_that_is_no_symbol_names_symbolp() {
        assert_results(&[
            ("(set 5 1)", "(ERR (wrong-type-argument symbolp 5))"),
            ("(symbol-value 5)", "(ERR (wrong-type-argument symbolp 5))"),
            (
                "(eval '(defvar 5 1) t)",
                "(ERR (wrong-type-argument symbolp 5))",
            ),
            (
                "(eval '(defconst 5 1) t)",
                "(ERR (wrong-type-argument symbolp 5))",
            ),
            (
                "(let ((l (list 1 2))) (setcar l l) (condition-case e (set l 1) (error (list (car e) (cadr e) (eq (nth 2 e) l)))))",
                "(wrong-type-argument symbolp t)",
            ),
        ]);
    }
}
