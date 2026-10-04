use std::fmt::Display;
use tulisp::{Error, TulispContext, TulispObject};

macro_rules! tulisp_assert {
    (@impl $ctx: expr, program:$input:expr, result:$result:expr $(,)?) => {
        let output = $ctx.eval_string($input).unwrap_or_else(|err| {
            panic!("{}:{}: execution failed: {}", file!(), line!(), err)
        });
        let expected = $ctx.eval_string($result)?;
        assert!(
            output.equal(&expected),
            "\n{}:{}: program: {}\n  output: {},\n  expected: {}\n",
            file!(),
            line!(),
            $input,
            output,
            expected
        );
    };

    (@impl $ctx: expr, program:$input:expr, result_str:$result:expr $(,)?) => {
        let output = $ctx.eval_string($input).map_err(|err| {
            println!("{}:{}: execution failed: {}", file!(), line!(), err);
            err
        })?;
        let expected = $ctx.eval_string($result)?;
        assert_eq!(output.to_string(), expected.to_string(),
            "\n{}:{}: program: {}\n  output: {},\n  expected: {}\n",
            file!(),
            line!(),
            $input,
            output,
            expected
        );
    };

    (@impl $ctx: expr, program:$input:expr, error:$desc:expr $(,)?) => {
        let output = $ctx.eval_string($input);
        assert!(output.is_err());
        assert_eq!(format!("{}\n", output.unwrap_err()), $desc);
    };

    (ctx: $ctx: expr, program: $($tail:tt)+) => {
        tulisp_assert!(@impl $ctx, program: $($tail)+);
    };

    (program: $($tail:tt)+) => {
        let mut ctx = TulispContext::new();
        tulisp_assert!(@impl ctx, program: $($tail)+);
    };
}

#[test]
fn test_princ_print_newlines() -> Result<(), Error> {
    // `princ` writes no newline (Emacs semantics); `print` writes the
    // value plus a trailing newline (deliberately NOT Emacs's
    // newline-before-and-after). Run the real binary so actual stdout
    // is observed — this needs an integration test because
    // CARGO_BIN_EXE is only set for integration targets.
    //
    // The pid suffix keeps concurrently running test binaries (the
    // default and `--features sync` suites) off each other's file.
    let script = std::env::temp_dir().join(format!(
        "tulisp_test_princ_print_{}.lisp",
        std::process::id()
    ));
    // Covers the defun `princ`, top-level `print` (the VM PrintPop
    // instruction), value-position `print` (the VM Print
    // instruction), and `print` reached through funcall dispatch.
    std::fs::write(
        &script,
        r#"(princ "a")(princ "b")(print 1)(print 2)(princ "end")(princ (print 5))(mapcar 'print '(9))"#,
    )
    .map_err(|e| Error::os_error(e.to_string()))?;
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_tulisp"))
        .arg(&script)
        .output()
        .map_err(|e| Error::os_error(e.to_string()))?;
    let _ = std::fs::remove_file(&script);
    assert_eq!(String::from_utf8_lossy(&out.stdout), "ab1\n2\nend5\n59\n");
    Ok(())
}

#[test]
fn test_eval_prelude_defuns_visible_to_later_evals() -> Result<(), Error> {
    // A definition evaluated through `eval_prelude` lands in the same
    // global scope as the built-in prelude, so later `eval_string`
    // calls on the same context see it like any built-in.
    let mut ctx = TulispContext::new();
    ctx.eval_prelude("user-prelude.lisp", "(defun double (x) (* 2 x))")?;
    tulisp_assert! {
        ctx: ctx,
        program: "(double 21)",
        result: "42",
    }
    Ok(())
}

#[test]
fn test_eval_prelude_error_trace_cites_given_filename() -> Result<(), Error> {
    // An error raised inside a defun that came from `eval_prelude`
    // must cite the caller-supplied filename in its trace frames, not
    // the shared `<eval_string>` bucket.
    let mut ctx = TulispContext::new();
    ctx.eval_prelude("user-prelude.lisp", "(defun add1 (x) (+ x 1))")?;
    tulisp_assert! {
        ctx: ctx,
        program: r#"(add1 "oops")"#,
        error: r#"ERR TypeMismatch: Expected number, got: "oops"
user-prelude.lisp:1.17-1.23:  at (+ x 1)
<eval_string>:1.1-1.13:  at (add1 "oops")
"#,
    }
    Ok(())
}

#[test]
fn test_eval_prelude_error_propagates_with_filename() -> Result<(), Error> {
    // A prelude program that fails must return the error to the
    // caller, with the trace citing the supplied filename.
    let mut ctx = TulispContext::new();
    let err = match ctx.eval_prelude("user-prelude.lisp", r#"(+ 1 "one")"#) {
        Err(err) => err,
        Ok(val) => panic!("expected an error, got: {val}"),
    };
    assert_eq!(
        err.to_string(),
        r#"ERR TypeMismatch: Expected number, got: "one"
user-prelude.lisp:1.1-1.11:  at (+ 1 "one")"#
    );
    Ok(())
}

#[test]
fn test_strings() -> Result<(), Error> {
    tulisp_assert! {
        program: r##"(concat 'hello 'world)"##,
        error: r#"ERR TypeMismatch: Not a string: hello
<eval_string>:1.1-1.22:  at (concat 'hello 'world)
"#
    }
    tulisp_assert! { program: r##"(concat "hello" " world")"##, result: r#""hello world""# }
    tulisp_assert! {
        program: r##"(let ((hello "hello") (world "world")) (concat hello " " world))"##,
        result: r#""hello world""#,
    }

    Ok(())
}

#[test]
fn test_cons() -> Result<(), Error> {
    tulisp_assert! {
        program: "(cons 1 2)",
        result: "'(1 . 2)",
    };
    tulisp_assert! {
        program: "(cons 1 (cons 2 (cons 3 nil)))",
        result: "'(1 2 3)",
    };
    tulisp_assert! {
        program: "(cons 1)",
        error: r#"ERR ArityMismatch: Too few arguments
<eval_string>:1.1-1.8:  at (cons 1)
"#
    };
    tulisp_assert! {
        program: "(cons 1 2 3)",
        error: r#"ERR ArityMismatch: Too many arguments
<eval_string>:1.1-1.12:  at (cons 1 2 3)
"#
    };
    Ok(())
}

#[test]
fn test_quote() -> Result<(), Error> {
    tulisp_assert! {
        program: "(quote (1 2 3))",
        result: "'(1 2 3)",
    };
    tulisp_assert! {
        program: "(quote word)",
        result: "'word",
    };
    tulisp_assert! {
        program: "(quote)",
        error: r#"ERR TypeMismatch: quote: expected one argument
<eval_string>:1.1-1.7:  at (quote)
"#
    };
    tulisp_assert! {
        program: "(quote 1 2)",
        error: r#"ERR TypeMismatch: quote: expected one argument
<eval_string>:1.1-1.11:  at (quote 1 2)
"#
    };
    Ok(())
}

#[test]
fn test_math() -> Result<(), Error> {
    // setcar / setcdr mutate cons cells in place.
    tulisp_assert! {
        program: "(let ((x (list 1 2 3))) (setcar x 99) x)",
        result: "'(99 2 3)",
    }
    tulisp_assert! {
        program: "(let ((x (list 1 2 3))) (setcdr x '(99 100)) x)",
        result: "'(1 99 100)",
    }
    // setcdr with a non-list value produces a dotted pair.
    tulisp_assert! {
        program: "(let ((x (list 1 2 3))) (setcdr x 99) x)",
        result: "'(1 . 99)",
    }
    // setcar / setcdr return their new value (Emacs matches).
    tulisp_assert! { program: "(setcar (list 1 2) 99)", result: "99" }

    // aset on strings — mutates in place, returns the new char.
    tulisp_assert! {
        program: r#"(let ((s "hello")) (aset s 0 65) s)"#,
        result: r#""Aello""#,
    }
    tulisp_assert! { program: r#"(aset (concat "foo") 0 65)"#, result: "65" }
    // Out-of-range / negative index errors.
    tulisp_assert! {
        program: r#"(aset (concat "abc") 5 65)"#,
        error: r#"ERR OutOfRange: aset: index 5 out of range for string of length 3
<eval_string>:1.1-1.26:  at (aset (concat "abc") 5 65)
"#,
    }
    // setcar on a non-cons errors. Numbers are filtered out of the
    // trace by `Error::format`; nil isn't, so the trace shows it.
    tulisp_assert! {
        program: "(setcar 5 99)",
        error: r#"ERR TypeMismatch: setcar: expected cons, got: 5
<eval_string>:1.1-1.13:  at (setcar 5 99)
"#,
    }
    tulisp_assert! {
        program: "(setcar nil 5)",
        error: r#"ERR TypeMismatch: setcar: expected cons, got: nil
<eval_string>:1.9-1.11:  at nil
<eval_string>:1.1-1.14:  at (setcar nil 5)
"#,
    }

    // Sequence operations on improper lists error like Emacs
    // (`wrong-type-argument`) instead of silently truncating.
    tulisp_assert! {
        program: "(length '(1 2 . 3))",
        error: r#"ERR TypeMismatch: expected list, got: 3
<eval_string>:1.1-1.19:  at (length '(1 2 . 3))
"#,
    }
    tulisp_assert! {
        program: "(reverse '(1 2 . 3))",
        error: r#"ERR TypeMismatch: Expected list, got: 3
<eval_string>:1.1-1.20:  at (reverse '(1 2 . 3))
"#,
    }
    // (funcall '<defun-with-required-args>) with too few args is an
    // error, not a panic. (`+` accepts zero args — `(+)` => 0 — so use
    // `1+`, which still requires one.)
    tulisp_assert! {
        program: "(funcall '1+)",
        error: r#"ERR ArityMismatch: Too few arguments
<eval_string>:1.1-1.13:  at (funcall '1+)
"#,
    }
    tulisp_assert! { program: "(min 12 5 45)",             result: "5"     }
    tulisp_assert! { program: "(max 12 5 45.2 8)",         result: "45.2"  }

    Ok(())
}

#[test]
fn test_rounding_operations() -> Result<(), Error> {
    tulisp_assert! { program: "(fround 3.14)",             result: "3.0"   }
    tulisp_assert! { program: "(fround 3.5)",              result: "4.0"   }

    tulisp_assert! { program: "(ftruncate 3.14)",          result: "3.0"   }
    tulisp_assert! { program: "(ftruncate 3.8)",           result: "3.0"   }
    tulisp_assert! { program: "(ftruncate -3.8)",          result: "-3.0"  }
    tulisp_assert! { program: "(ftruncate -3.14)",         result: "-3.0"  }

    tulisp_assert! { program: "(floor 3.7)",    result: "3"   }
    tulisp_assert! { program: "(floor -3.2)",   result: "-4"  }
    tulisp_assert! { program: "(floor 7 2)",    result: "3"   }
    tulisp_assert! { program: "(floor 5)",      result: "5"   }

    tulisp_assert! { program: "(ceiling 3.2)",  result: "4"   }
    tulisp_assert! { program: "(ceiling -3.7)", result: "-3"  }
    tulisp_assert! { program: "(ceiling 7 2)",  result: "4"   }

    tulisp_assert! { program: "(truncate 3.7)", result: "3"   }
    tulisp_assert! { program: "(truncate -3.7)",result: "-3"  }

    tulisp_assert! { program: "(round 3.4)",    result: "3"   }
    tulisp_assert! { program: "(round 3.6)",    result: "4"   }
    // Round half to even.
    tulisp_assert! { program: "(round 2.5)",    result: "2"   }
    tulisp_assert! { program: "(round 3.5)",    result: "4"   }
    tulisp_assert! { program: "(round -2.5)",   result: "-2"  }

    tulisp_assert! { program: "(ffloor 3.7)",   result: "3.0" }
    tulisp_assert! { program: "(fceiling 3.2)", result: "4.0" }

    tulisp_assert! {
        program: "(fround)",
        error: r#"ERR ArityMismatch: Too few arguments
<eval_string>:1.1-1.8:  at (fround)
"#,
    }
    tulisp_assert! {
        program: "(fround 3.14 3.14)",
        error: r#"ERR ArityMismatch: Too many arguments
<eval_string>:1.1-1.18:  at (fround 3.14 3.14)
"#,
    }

    Ok(())
}

#[test]
fn test_typed_defun_arity_checked_before_arg_eval() -> Result<(), Error> {
    // `TulispValue::Defun` carries arity metadata so `compile_form`
    // can reject mismatches before the user's closure runs, and
    // before any argument expression runs.
    //
    // Test shape: the defun takes 1 required + 0 optional + no rest.
    // Each arg expression bumps a counter so we can observe whether
    // it ran. With the arity check in place, a `(narrow 1 2 3)` call
    // never evaluates arg 2 or arg 3.
    use std::sync::Arc;
    use std::sync::atomic::{AtomicI64, Ordering};

    // Atomic-backed counter so the closure stays `Send + Sync` for
    // both the default (`Rc`) and `--features sync` (`Arc`) builds.
    let counter = Arc::new(AtomicI64::new(0));
    let counter_for_defun = counter.clone();
    let mut ctx = TulispContext::new();
    // `bump` increments the side-effect counter and returns its arg.
    // Used as the arg expression so we can detect whether the
    // dispatcher evaluated the arg before erroring.
    ctx.defun("bump", move |x: i64| {
        counter_for_defun.fetch_add(1, Ordering::Relaxed);
        x
    });
    // `narrow` requires exactly 1 arg.
    ctx.defun("narrow", |x: i64| -> i64 { x });

    // Happy-path baseline: 1 arg, evaluated once.
    counter.store(0, Ordering::Relaxed);
    let r = ctx.eval_string("(narrow (bump 7))")?;
    assert_eq!(i64::try_from(r)?, 7);
    assert_eq!(counter.load(Ordering::Relaxed), 1);

    // Too few is rejected at compile time.
    counter.store(0, Ordering::Relaxed);
    let err = ctx.eval_string("(narrow)");
    let msg = err.unwrap_err().to_string();
    assert!(
        msg.starts_with("ERR ArityMismatch: Too few arguments"),
        "expected too-few error, got: {}",
        msg
    );
    assert_eq!(counter.load(Ordering::Relaxed), 0);

    // Too many is rejected at compile time too, before any argument
    // runs.
    counter.store(0, Ordering::Relaxed);
    let err = ctx.eval_string("(narrow (bump 1) (bump 2) (bump 3))");
    let msg = err.unwrap_err().to_string();
    assert!(
        msg.starts_with("ERR ArityMismatch: Too many arguments"),
        "expected too-many error, got: {}",
        msg
    );
    assert_eq!(counter.load(Ordering::Relaxed), 0);

    Ok(())
}

#[test]
fn test_closure_invoked_in_fresh_ctx() -> Result<(), Error> {
    // A closure compiled in one ctx must run through a separate ctx
    // that never saw its `MakeLambda`: the jumps `cond`, `and` and
    // `or` emit must not depend on per-machine state.
    //
    // The canonical scenario is `tulisp-async`'s `run-with-timer`,
    // which creates a per-firing ctx and invokes the timer's lambda
    // (compiled in the parent ctx) via `ctx.funcall`. Reproduced
    // here without the async runtime by building a closure in
    // `ctx_a`, then invoking it through a freshly-constructed
    // `ctx_b`.
    for (body, expected) in [
        // cond: multi-target jump table
        ("(cond ((= 1 2) 'a) (t 'b))", "b"),
        // and: short-circuit
        ("(and 1 2 3)", "3"),
        // or: short-circuit
        ("(or nil nil 'found)", "found"),
    ] {
        let mut ctx_a = TulispContext::new();
        let prog = format!("(lambda () {body})");
        let closure = ctx_a.eval_string(&prog)?;

        let mut ctx_b = TulispContext::new();
        let result = ctx_b
            .funcall(&closure, ())
            .unwrap_or_else(|e| panic!("cross-ctx funcall of `{}` failed: {}", prog, e));
        assert_eq!(
            result.to_string(),
            expected,
            "cross-ctx funcall of `{prog}`"
        );
    }
    // A closure over variables, and one with variables of its own, run
    // through another ctx too.
    for (prog, expected) in [
        ("(let ((n 41)) (lambda () (1+ n)))", "42"),
        ("(let ((n 1)) (lambda () (let ((m (* n 2))) (+ m 1))))", "3"),
    ] {
        let mut ctx_a = TulispContext::new();
        let closure = ctx_a.eval_string(prog)?;
        let mut ctx_b = TulispContext::new();
        let result = ctx_b.funcall(&closure, ())?;
        assert_eq!(
            result.to_string(),
            expected,
            "cross-ctx funcall of `{prog}`"
        );
    }
    Ok(())
}

#[test]
fn test_owned_method() -> Result<(), Error> {
    struct Demo {
        vv: i64,
    }
    impl Demo {
        fn run(&self) -> i64 {
            self.vv
        }
    }

    let d = Demo { vv: 5 };

    let mut ctx = TulispContext::new();

    ctx.defspecial("d.run", move || d.run());

    tulisp_assert! {
        ctx: ctx,
        program: "(d.run)",
        result: "5",
    }

    Ok(())
}

#[test]
fn test_from_iter() -> Result<(), Error> {
    let obj: TulispObject = (1..10).map(|x| (x * 2).into()).collect();
    assert_eq!(obj.to_string(), "(2 4 6 8 10 12 14 16 18)");
    Ok(())
}

#[test]
fn test_any() -> Result<(), Error> {
    #[derive(Clone)]
    struct TestStruct {
        value: i64,
    }
    impl Display for TestStruct {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "(TestStruct {})", self.value)
        }
    }
    impl tulisp::TulispAny for TestStruct {}

    let mut ctx = TulispContext::new();

    ctx.defun("make_any", |value: i64| TestStruct { value });

    ctx.defun("get_int", |value: TestStruct| value.value);

    ctx.defun("maybe_add", |value: i64, maybe_num: TulispObject| {
        if maybe_num.null() {
            return Ok(value);
        }
        Ok(value + i64::try_from(maybe_num)?)
    });

    tulisp_assert! {
        ctx: ctx,
        program: "(get_int (make_any 22))",
        result: "22",
    }
    tulisp_assert! {
        ctx: ctx,
        program: "(make_any 55)",
        result_str: "'(TestStruct 55)",
    }
    tulisp_assert! {
        ctx: ctx,
        program: "(get_int 55)",
        error: r#"ERR TypeMismatch: Expected TestStruct, got: 55
<eval_string>:1.1-1.12:  at (get_int 55)
"#
    }
    tulisp_assert! {
        ctx: ctx,
        program: "(maybe_add 10 5)",
        result: "15",
    }
    tulisp_assert! {
        ctx: ctx,
        program: "(maybe_add 10 nil)",
        result: "10",
    }
    tulisp_assert! {
        ctx: ctx,
        program: "(maybe_add 10)",
        error: r#"ERR ArityMismatch: Too few arguments
<eval_string>:1.1-1.14:  at (maybe_add 10)
"#
    }

    Ok(())
}

#[test]
fn test_hash_table() -> Result<(), Error> {
    tulisp_assert! {
        program: r#"
        (let ((tbl (make-hash-table)))
          (puthash 'a 20 tbl)
          (puthash 'b 30 tbl)
          (gethash 'a tbl))
        "#,
        result: "20",
    }
    tulisp_assert! {
        program: r#"
        (let ((tbl (make-hash-table)))
          (puthash 2 20 tbl)
          (puthash 3 30 tbl)
          (list (gethash 4 tbl) (gethash 2 tbl)))
        "#,
        result: "'(nil 20)",
    }

    Ok(())
}

#[test]
fn test_symbol_creation() -> Result<(), Error> {
    tulisp_assert! {
        program: r#"
        (let ((sym (intern "hello")))
          (list (eq sym 'hello) (equal (format "%s" sym) "hello")))
        "#,
        result: "'(t t)"
    }

    tulisp_assert! {
        program: r#"
        (let ((sym (make-symbol "hello")))
          (list (eq sym 'hello) (equal (format "%s" sym) "hello")))
        "#,
        result: "'(nil t)"
    }

    tulisp_assert! {
        program: r#"
        (let ((sym (gensym "hello"))
              (sym2 (gensym)))
          (list
           (eq sym 'hello)
           (eq sym 'hello0)
           (equal (format "%s" sym2)             "g1")
           (equal (format "%s" sym)              "hello0")
           (equal (format "%s" (gensym "hello")) "hello2")))
        "#,
        result: r#"'(nil nil t t t)"#
    }

    Ok(())
}

// The exported macros name what they use through `$crate`, so a caller
// that imports nothing but the macro can expand them.
mod without_imports {
    pub fn third(ctx: &mut tulisp::TulispContext) -> Result<tulisp::TulispObject, tulisp::Error> {
        let args = tulisp::list!(,1 ,2)?;
        let words = tulisp::list!(ctx => ,"a" ,@args)?;
        let (_, _, third): (
            tulisp::TulispObject,
            tulisp::TulispObject,
            tulisp::TulispObject,
        ) = words.destructure(ctx)?;
        Ok(third)
    }
}

#[test]
fn test_exported_macros_expand_without_imports() {
    let mut ctx = tulisp::TulispContext::new();
    assert_eq!(without_imports::third(&mut ctx).unwrap().to_string(), "2");
}
