use std::fmt::Display;
use tulisp::{Error, Iter, TulispContext, TulispObject};

macro_rules! tulisp_assert {
    (@impl $ctx: expr, program:$input:expr, result:$result:expr $(,)?) => {
        let output = $ctx.eval_string($input).map_err(|err| {
            panic!("{}:{}: execution failed: {}", file!(), line!(),err.format(&$ctx));

        })?;
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
            println!("{}:{}: execution failed: {}", file!(), line!(),err.format(&$ctx));
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
        assert_eq!(output.unwrap_err().format(&$ctx), $desc);
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
        err.format(&ctx),
        r#"ERR TypeMismatch: Expected number, got: "one"
user-prelude.lisp:1.1-1.11:  at (+ 1 "one")
"#
    );
    Ok(())
}

#[test]
fn test_eval() -> Result<(), Error> {
    tulisp_assert! {
        program: "(eval '(mod 32 5))",
        result: "2",
    }

    tulisp_assert! {
        program: "(let ((j 10)) (+ j j))(+ j 1)",
        error: r#"ERR Uninitialized: Variable definition is void: j
<eval_string>:1.23-1.29:  at (+ j 1)
"#
    }
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

    tulisp_assert! {
        program: r##"(format "Hello, %s! %%%d %f %s %d" "world" 22.8 22.8 10 10)"##,
        result: r#""Hello, world! %22 22.8 10 10""#
    }

    // Width: right-aligned by default, left-aligned with `-`.
    tulisp_assert! {
        program: r#"(format "[%10s]" "hi")"#,
        result: r#""[        hi]""#
    }
    tulisp_assert! {
        program: r#"(format "[%-10s]" "hi")"#,
        result: r#""[hi        ]""#
    }
    // Zero-pad for numerics.
    tulisp_assert! {
        program: r#"(format "%05d" 42)"#,
        result: r#""00042""#
    }
    // Shorter than width stays as-is (no truncation).
    tulisp_assert! {
        program: r#"(format "[%3s]" "hello")"#,
        result: r#""[hello]""#
    }
    // Width applies to %d too.
    tulisp_assert! {
        program: r#"(format "[%5d]" 7)"#,
        result: r#""[    7]""#
    }
    tulisp_assert! {
        program: r#"(format "[%-5d]" 7)"#,
        result: r#""[7    ]""#
    }

    tulisp_assert! { program: "(prin1-to-string 'hello)", result: r#""hello""# }
    tulisp_assert! { program: "(prin1-to-string #'hello)", result: r#""hello""# }
    tulisp_assert! { program: "(prin1-to-string 25)", result: r#""25""# }
    tulisp_assert! { program: "(setq h 25)(prin1-to-string h)", result: r#""25""# }
    tulisp_assert! {
        program: "(setq h '(list 25 'hello))(prin1-to-string h)",
        result: r#""(list 25 'hello)""#
    }
    tulisp_assert! { program: r##"(setq h "hello")(prin1-to-string h)"##, result: r#""hello""# }
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
fn test_cxr_non_cons_input() -> Result<(), Error> {
    // Every cxr must propagate `TypeMismatch` rather than panic on a
    // non-cons argument. Regression: the VM's `Instruction::Cxr` arm
    // used to `.unwrap()` the result of `obj.car()` etc., which
    // crashed the process on plain Lisp like `(car 5)`.
    tulisp_assert! {
        program: "(car 5)",
        error: r#"ERR TypeMismatch: Expected list, got: 5
<eval_string>:1.1-1.7:  at (car 5)
"#
    };
    tulisp_assert! {
        program: "(cdr 5)",
        error: r#"ERR TypeMismatch: Expected list, got: 5
<eval_string>:1.1-1.7:  at (cdr 5)
"#
    };
    tulisp_assert! {
        program: "(cadr 7)",
        error: r#"ERR TypeMismatch: Expected list, got: 7
<eval_string>:1.1-1.8:  at (cadr 7)
"#
    };
    tulisp_assert! {
        program: "(cdddr \"abc\")",
        error: r#"ERR TypeMismatch: Expected list, got: "abc"
<eval_string>:1.1-1.13:  at (cdddr "abc")
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
fn test_lists() -> Result<(), Error> {
    tulisp_assert! {
        program: r##"
        (let ((items '(4 20 3 22 55)))
          (list
           (nth 0 items)
           (nth 2 items)
           (nth 4 items)
           (nth 5 items)))
        "##,
        result: "'(4 3 55 nil)"
    }

    tulisp_assert! {
        program: r##"
        (let ((items '(4 20 3 22 55)))
          (list (nthcdr 0 items)
                (nthcdr 2 items)
                (nthcdr 4 items)
                (nthcdr 5 items)))
        "##,
        result: "'((4 20 3 22 55) (3 22 55) (55) nil)"
    }

    Ok(())
}

#[test]
fn test_math() -> Result<(), Error> {
    // Character literals: `?X` reads as the character's code point.
    tulisp_assert! { program: "?A",       result: "65"  }
    tulisp_assert! { program: "?z",       result: "122" }
    tulisp_assert! { program: r#"?\n"#,   result: "10"  }
    tulisp_assert! { program: r#"?\t"#,   result: "9"   }
    tulisp_assert! { program: r#"?\\"#,   result: "92"  }
    tulisp_assert! { program: r#"?\0"#,   result: "0"   }
    // Radix-prefixed integer literals: `#x` / `#X` hex, `#o` octal,
    // `#b` binary. Sign goes between the prefix and digits.
    tulisp_assert! { program: "#x10",  result: "16"   }
    tulisp_assert! { program: "#xff",  result: "255"  }
    tulisp_assert! { program: "#xFF",  result: "255"  }
    tulisp_assert! { program: "#X10",  result: "16"   }
    tulisp_assert! { program: "#o10",  result: "8"    }
    tulisp_assert! { program: "#b1010",result: "10"   }
    tulisp_assert! { program: "#x-10", result: "-16"  }

    // Whole-value floats round-trip with a trailing `.0` (Emacs:
    // `(format "%S" 2.0) => "2.0"`).
    tulisp_assert! { program: r#"(format "%S" 1.0)"#,       result: r#""1.0""# }
    tulisp_assert! { program: r#"(format "%S" (+ 1.0 1))"#, result: r#""2.0""# }
    tulisp_assert! { program: r#"(format "%S" 0.5)"#,       result: r#""0.5""# }

    // Scientific notation parses as float (Emacs: `1e5 => 100000.0`).
    tulisp_assert! { program: "(+ 1e5 1)",          result: "100001.0" }
    tulisp_assert! { program: "(+ 1E5 1)",          result: "100001.0" }
    tulisp_assert! { program: "(+ 1.5e2 0)",        result: "150.0"    }
    tulisp_assert! { program: "(+ 1e+5 0)",         result: "100000.0" }
    tulisp_assert! { program: "(+ -1.5e-3 0)",      result: "-0.0015"  }
    tulisp_assert! { program: "(integerp 1e5)",     result: "nil"      }
    tulisp_assert! { program: "(floatp 1e5)",       result: "t"        }
    // `e5` and `1ee5` aren't scientific-notation floats — they read
    // as ordinary symbols (Emacs matches).
    tulisp_assert! { program: "(progn (setq e5 7) e5)",       result: "7" }
    tulisp_assert! { program: "(progn (setq 1ee5 9) 1ee5)",   result: "9" }
    // `1e` (no exponent digit) falls back to identifier rather than
    // erroring — Emacs reads it as a symbol too.
    tulisp_assert! { program: "(progn (setq 1e 11) 1e)",      result: "11" }

    // Emacs-style infinity / NaN literals — `<mantissa>e+INF` and
    // `<mantissa>e+NaN`, uppercase only, `e+` only. Mantissa value
    // is ignored; only its sign matters.
    tulisp_assert! { program: r#"(format "%S" 1.0e+INF)"#,    result: r#""1.0e+INF""# }
    tulisp_assert! { program: r#"(format "%S" -1.0e+INF)"#,   result: r#""-1.0e+INF""# }
    tulisp_assert! { program: r#"(format "%S" 0.0e+NaN)"#,    result: r#""0.0e+NaN""# }
    tulisp_assert! { program: r#"(format "%S" -0.0e+NaN)"#,   result: r#""-0.0e+NaN""# }
    // Mantissa is ignored — `5.5e+INF` and `1e+INF` both read as +INF.
    tulisp_assert! { program: r#"(format "%S" 5.5e+INF)"#,    result: r#""1.0e+INF""# }
    tulisp_assert! { program: r#"(format "%S" 1e+INF)"#,      result: r#""1.0e+INF""# }
    // Display of arithmetic-produced infinity matches the source form.
    tulisp_assert! { program: r#"(format "%S" (/ 1.0 0.0))"#,  result: r#""1.0e+INF""# }
    tulisp_assert! { program: r#"(format "%S" (/ -1.0 0.0))"#, result: r#""-1.0e+INF""# }
    // String Display escapes `"`, `\`, `\n`, `\t` so the printed
    // form parses back to the same value (round-trip).
    tulisp_assert! {
        program: r#"(format "%S" "a\"b\\c")"#,
        result: r#""\"a\\\"b\\\\c\"""#,
    }
    tulisp_assert! {
        program: r#"(format "%S" "with\nnewline")"#,
        result: r#""\"with\\nnewline\"""#,
    }

    // Lowercase / `e-` variants stay as identifiers (Emacs matches).
    tulisp_assert! { program: "(progn (setq 1.0e+inf 5) 1.0e+inf)",  result: "5" }
    tulisp_assert! { program: "(progn (setq 1.0e-INF 5) 1.0e-INF)",  result: "5" }
    tulisp_assert! { program: "(progn (setq inf 5) inf)",            result: "5" }

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
    tulisp_assert! { program: "(+ 40 (* 2.5 4) (- 4 12))", result: "42.0"  }
    tulisp_assert! { program: "(+ 40 (* 2.5 4) (- -1 7))", result: "42.0"  }
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

    tulisp_assert! { program: "1_000",            result: "1000"      }
    tulisp_assert! { program: "1_000_000",        result: "1000000"   }
    tulisp_assert! { program: "(+ 1_000 2_000)",  result: "3000"      }
    tulisp_assert! { program: "1_000.5",          result: "1000.5"    }
    tulisp_assert! { program: "1_000.000_1",      result: "1000.0001" }

    tulisp_assert! { program: ".5",               result: "0.5"       }
    tulisp_assert! { program: ".25",              result: "0.25"      }
    tulisp_assert! { program: "(+ .5 .25)",       result: "0.75"      }
    tulisp_assert! { program: ".1_5",             result: "0.15"      }

    tulisp_assert! {
        program: "99999999999999999999",
        error:
r#"ERR ParsingError: SyntaxError number too large to fit in target type: 99999999999999999999
<eval_string>:1.1-1.20:  at nil
"#,
    }
    tulisp_assert! {
        program: "-.",
        error: r#"ERR ParsingError: SyntaxError invalid float literal: -.
<eval_string>:1.1-1.2:  at nil
"#,
    }
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
fn test_let() -> Result<(), Error> {
    // `(append nil x)` returns `x` directly under Emacs semantics —
    // the last arg is shared, not wrapped.
    tulisp_assert! {
        program: "(let ((kk) (vv (+ 55 1)) (jj 20)) (append kk (+ vv jj 1)))",
        result: "77",
    }
    tulisp_assert! {
        program: "(let (kk (vv (+ 55 1)) (jj 20)) (append kk (+ vv jj 1)))",
        result: "77",
    }
    tulisp_assert! {
        program: r#"
        (let ((vv (+ 55 1))
              (jj 20))
          (append kk (+ vv jj 1)))
        "#,
        error: r#"ERR Uninitialized: Variable definition is void: kk
<eval_string>:4.11-4.33:  at (append kk (+ vv jj 1))
<eval_string>:2.9-4.34:  at (let ((vv (+ 55 1)) (jj 20)) (append kk (+ vv jj 1)))
"#
    }
    tulisp_assert! {
        program: "(let ((22 (+ 55 1)) (jj 20)) (+ vv jj 1))",
        error: r#"ERR TypeMismatch: Expected Symbol: Can't assign to 22
<eval_string>:1.1-1.41:  at (let ((22 (+ 55 1)) (jj 20)) (+ vv jj 1))
"#
    }
    tulisp_assert! {
        program: "(let (18 (vv (+ 55 1)) (jj 20)) (+ vv jj 1))",
        error: r#"ERR SyntaxError: varitems inside a let-varlist should be a var or a binding: 18
<eval_string>:1.1-1.44:  at (let (18 (vv (+ 55 1)) (jj 20)) (+ vv jj 1))
"#
    }

    tulisp_assert! {
        program: "(let ((vv (+ 55 1)) (jj 20)) (+ vv jj 1))",
        result: "77",
    }

    tulisp_assert! {
        program: "(let* ((vv 21) (jj (+ vv 1))) (setq jj (+ 21 jj)) jj)",
        result: "43",
    }

    Ok(())
}

#[test]
fn test_empty_body_yields_nil_in_keep_result_position() -> Result<(), Error> {
    // Regression: `compile_progn` returned an empty instruction
    // sequence for empty body lists, so callers in keep-result
    // position (e.g. inside a `(princ …)`) found nothing on the VM
    // stack and panicked with `attempt to subtract with overflow`.
    // Emacs evaluates each of these to nil; tulisp now matches.
    tulisp_assert! {
        program: "(progn)",
        result: "nil",
    }
    tulisp_assert! {
        program: "(let ((x 5)))",
        result: "nil",
    }
    tulisp_assert! {
        program: "(let* ((x 5)))",
        result: "nil",
    }
    // Empty progn nested in a position that needs a value.
    tulisp_assert! {
        program: "(if t (progn) 'else)",
        result: "nil",
    }
    Ok(())
}

#[test]
fn test_let_discard_result_runs_init_side_effects() -> Result<(), Error> {
    // Regression: a `let` whose result is discarded — top-level form
    // in a loaded file, body in a discard-result position — must
    // still evaluate the binding-init expressions. The bytecode
    // compiler used to short-circuit `let` whose body compiled to
    // zero instructions and drop the inits along with the body,
    // silently swallowing side effects like `(let ((x (mutate))) t)`.
    tulisp_assert! {
        program: "(progn (setq c 0) (let ((a (progn (setq c (1+ c)) c))) a) c)",
        result: "1",
    }
    tulisp_assert! {
        program: "(progn (setq c 0) (let* ((a (progn (setq c (1+ c)) c))) a) c)",
        result: "1",
    }
    // Body that doesn't reference the binding at all (just `t`) —
    // the init still has to run.
    tulisp_assert! {
        program: "(progn (setq c 0) (let ((_ (progn (setq c (1+ c)) c))) t) c)",
        result: "1",
    }
    // Sequential lets in one progn each contribute their side
    // effects; the bug used to elide every one of them.
    tulisp_assert! {
        program: "(progn
                    (setq c 0)
                    (defun bump () (setq c (1+ c)) c)
                    (let ((a (bump))) t)
                    (let ((b (bump))) t)
                    c)",
        result: "2",
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
    assert_eq!(r.try_int()?, 7);
    assert_eq!(counter.load(Ordering::Relaxed), 1);

    // Too few is rejected at compile time.
    counter.store(0, Ordering::Relaxed);
    let err = ctx.eval_string("(narrow)");
    let msg = err.unwrap_err().format(&ctx);
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
    let msg = err.unwrap_err().format(&ctx);
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
        let result = ctx_b.funcall(&closure, ()).unwrap_or_else(|e| {
            panic!(
                "cross-ctx funcall of `{}` failed: {}",
                prog,
                e.format(&ctx_b)
            )
        });
        assert_eq!(
            result.to_string(),
            expected,
            "cross-ctx funcall of `{prog}`"
        );
    }
    Ok(())
}

#[test]
fn test_threading_macros() -> Result<(), Error> {
    tulisp_assert! {
        program: r##"
        (macroexpand
         '(-> 9
              (expt 0.5)
              (equal 3)
              (if "true" "false")))
        "##, result: r##"
        '(if (equal (expt 9 0.5) 3)
            "true"
          "false")
        "##,
    };

    tulisp_assert! {
        program: r##"
        (macroexpand
         '(->> 0.5
               (expt 9)
               (equal 3)
               (if nil ())))
        "##, result: r##"
        '(if nil
            ()
          (equal 3 (expt 9 0.5)))
        "##,
    };

    tulisp_assert! {
        program: "(thread-last (- 5) (- 10) -)",
        result: "-15",
    }

    tulisp_assert! {
        program: "(thread-first (- 5) (- 10) -)",
        result: "15",
    }

    tulisp_assert! {
            program: r##"
            (macroexpand '(thread-last
                           (if-let (b) (print b))
                           (if-let (a) (print a))
                           (if-let ((a) (b))
                               (print a))))
            "##,
        result_str: r##"
        '(let* ((s (and t a))
                (s (and s b)))
           (if s
               (print a)
             (let* ((s (and t a)))
               (if s
                   (print a)
                 (let* ((s (and t b)))
                     (if s
                         (print b)
                       nil))))))
        "##,
    }

    tulisp_assert! {
        program: r#"
        (let ((vv 2) (jj 3))
                (thread-last
                  (setq vv 4)
                  (if-let ((a (> 20 10)))
                      (setq jj 5)))
                (list vv jj))
       "#,
        result: "'(2 5)",
    }

    tulisp_assert! {
        program: "(-> 10)",
        result: "10",
    }

    tulisp_assert! {
        program: "(->> 10)",
        result: "10",
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
fn test_typed_iter() -> Result<(), Error> {
    let mut ctx = TulispContext::new();

    ctx.defspecial("add_ints", |ints: TulispObject| -> Result<i64, Error> {
        let ints: Iter<i64> = ints.iter()?;
        let mut sums = 0;
        for next in ints {
            sums += next?;
        }
        Ok(sums)
    });

    tulisp_assert! {
        ctx: ctx,
        program: "(add_ints '(10 20 30))",
        result: "60",
    }
    tulisp_assert! {
        ctx: ctx,
        program: "(add_ints 20)",
        error: r#"ERR TypeMismatch: Expected list, got: 20
<eval_string>:1.1-1.13:  at (add_ints 20)
"#
    }
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

#[test]
fn test_underscore_ident() -> Result<(), Error> {
    // A lone underscore is a valid identifier, not a number.
    tulisp_assert! { program: "(let ((_ 42)) _)", result: "42" }
    // Leading underscore is also a valid identifier.
    tulisp_assert! { program: "(let ((_x 7)) _x)", result: "7" }
    // Underscore as numeric separator still works.
    tulisp_assert! { program: "1_000", result: "1000" }
    Ok(())
}

// The exported macros name what they use through `$crate`, so a caller
// that imports nothing but the macro can expand them.
mod without_imports {
    pub fn third(_ctx: &mut tulisp::TulispContext) -> Result<tulisp::TulispObject, tulisp::Error> {
        let args = tulisp::list!(,tulisp::TulispObject::from(1) ,tulisp::TulispObject::from(2))?;
        tulisp::destruct_bind!((first &optional second third) = args);
        let _ = (first, second);
        Ok(third)
    }
}

#[test]
fn test_exported_macros_expand_without_imports() {
    let mut ctx = tulisp::TulispContext::new();
    assert!(without_imports::third(&mut ctx).unwrap().null());
}
