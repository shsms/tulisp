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
fn test_conditionals() -> Result<(), Error> {
    tulisp_assert! { program: "(if t 10 15 20)",      result: "10" }
    tulisp_assert! { program: "(if nil 10 15 20)",    result: "20" }
    tulisp_assert! { program: "(if (> 20 10) 10 20)", result: "10" }
    tulisp_assert! { program: "(if (> 10 20) 10 20)", result: "20" }
    tulisp_assert! { program: r##"
       (defun cf (vv)
         (cond ((> vv 45) 'gt45)
               ((> vv 5) 'gt5)))

       (list (cf 2) (cf 200) (cf 8))
    "##, result: r#"'(nil gt45 gt5)"#}

    tulisp_assert! { program: "(when t 10 20 30)", result: "30" }
    tulisp_assert! { program: "(when nil 10 20 30)", result: "nil" }
    tulisp_assert! { program: "(when (> 20 10) 10 20 30)", result: "30" }
    tulisp_assert! { program: "(when (> 10 20) 10 20 30)", result: "nil" }

    tulisp_assert! { program: "(unless t 10 20 30)", result: "nil" }
    tulisp_assert! { program: "(unless nil 10 20 30)", result: "30" }
    tulisp_assert! { program: "(unless (> 20 10) 10 20 30)", result: "nil" }
    tulisp_assert! { program: "(unless (> 10 20) 10 20 30)", result: "30" }

    tulisp_assert! { program: "(not t)", result: "nil" }
    tulisp_assert! { program: "(not nil)", result: "t" }
    tulisp_assert! { program: "(not (< 10 20))", result: "nil" }
    tulisp_assert! { program: "(not (> 10 20))", result: "t" }

    tulisp_assert! { program: "(xor t t)", result: "nil" }
    tulisp_assert! { program: "(xor t nil)", result: "t" }
    tulisp_assert! { program: "(xor nil t)", result: "t" }
    tulisp_assert! { program: "(xor nil nil)", result: "nil" }
    tulisp_assert! { program: "(xor (> 10 5) (< 10 20))", result: "nil" }
    tulisp_assert! { program: "(xor (> 10 5) (> 10 20))", result: "t" }
    tulisp_assert! { program: "(xor (< 10 5) (< 10 20))", result: "t" }
    tulisp_assert! { program: "(xor (< 10 5) (> 10 20))", result: "nil" }
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
fn test_lexical_binding() -> Result<(), Error> {
    tulisp_assert! {
        program: r#"
        (setq some-var 0)
        (setq x 2)
        (+ x (funcall (let ((x 10)
                       (inc-some-var (lambda () (setq some-var (+ some-var x)))))
                   (funcall inc-some-var)
                   (let ((x 100))
                     (funcall inc-some-var))
                   inc-some-var)))
        "#,
        result: "32",
    }

    tulisp_assert! {
        program: r#"
        (setq n 2)
        (defun make-adder (n)
          (lambda (x) (+ x n)))

        (setq add2 (make-adder 2))
        (setq add10 (make-adder 10))

        (list (+ n (funcall add2 2))
              (funcall add10 2))
        "#,
        result: "'(6 12)",
    }

    tulisp_assert! {
        program: r#"
        (setq alist '((a . 1) (b . 2)))
        (let ((a 10) (b 20))
          (list (alist-get 'a alist)
                (alist-get 'b alist)))
        "#,
        result: "'(1 2)",
    }

    tulisp_assert! {
        program: r#"
        (let ((a (list '((a . nil)) '((a . t)))))
            (seq-filter (lambda (x) (alist-get 'a x)) a))
        "#,
        result: "'(((a . t)))",
    }

    // 'symbol is a literal — a defun param with the same name must not
    // rewrite it into a variable reference.
    tulisp_assert! {
        program: r#"
        (defun lookup-a (a data) (alist-get 'a data))
        (lookup-a 999 '((a . 1) (b . 2)))
        "#,
        result: "1",
    }

    // '(…) list literals stay literal even when they contain names that
    // match lex bindings in the surrounding scope.
    tulisp_assert! {
        program: r#"
        (defun keys-of (x) '(a b x))
        (keys-of 42)
        "#,
        result: "'(a b x)",
    }

    // Nested closures: each inner lambda captures its own enclosing var.
    tulisp_assert! {
        program: r#"
        (defun outer (x)
          (lambda (y)
            (lambda (z) (+ x y z))))
        (funcall (funcall (outer 100) 20) 3)
        "#,
        result: "123",
    }

    // Closure invoked after outer let scope has exited — captured slot
    // must still hold the value.
    tulisp_assert! {
        program: r#"
        (setq g (let ((k 7)) (lambda () k)))
        (funcall g)
        "#,
        result: "7",
    }

    // setq on a captured variable inside a closure persists across
    // invocations (classic counter pattern).
    tulisp_assert! {
        program: r#"
        (defun make-counter ()
          (let ((n 0))
            (lambda () (setq n (+ n 1)) n)))
        (setq c (make-counter))
        (list (funcall c) (funcall c) (funcall c))
        "#,
        result: "'(1 2 3)",
    }

    // Two counters built from the same factory are independent.
    tulisp_assert! {
        program: r#"
        (defun make-counter ()
          (let ((n 0))
            (lambda () (setq n (+ n 1)) n)))
        (setq a (make-counter))
        (setq b (make-counter))
        (funcall a) (funcall a) (funcall b)
        (list (funcall a) (funcall b))
        "#,
        result: "'(3 2)",
    }

    // A lambda parameter shadows an outer lex binding of the same name.
    tulisp_assert! {
        program: r#"
        (let ((x 100))
          (funcall (lambda (x) (* x 2)) 7))
        "#,
        result: "14",
    }

    // let* sequential binding — later bindings see earlier ones.
    tulisp_assert! {
        program: r#"
        (let* ((a 1) (b (+ a 10)) (c (+ a b))) (list a b c))
        "#,
        result: "'(1 11 12)",
    }

    // setq on a let-bound variable inside the let scope propagates to a
    // closure that captured the same binding (Emacs behavior — the
    // closure and the enclosing scope share the slot).
    tulisp_assert! {
        program: r#"
        (let ((x 1))
          (setq f (lambda () x))
          (setq x 42))
        (funcall f)
        "#,
        result: "42",
    }

    // setq on a defun parameter is visible to a closure constructed
    // earlier inside the same defun.
    tulisp_assert! {
        program: r#"
        (defun outer-mutating (x)
          (let ((g (lambda () x)))
            (setq x 99)
            (funcall g)))
        (outer-mutating 1)
        "#,
        result: "99",
    }

    // Two closures that captured the same let-binding share the slot,
    // so `setq` in one is visible to the other.
    tulisp_assert! {
        program: r#"
        (let ((n 0))
          (setq inc (lambda () (setq n (+ n 1)) n))
          (setq read-n (lambda () n)))
        (funcall inc)
        (funcall inc)
        (funcall read-n)
        "#,
        result: "2",
    }

    // Backquote constructed in one scope, eval'd inside another
    // function. The unquoted value is captured at construction time so
    // the inner eval only needs to see already-resolved literals.
    tulisp_assert! {
        program: r#"
        (defun run-eval (form) (eval form))
        (let ((id 99))
          (run-eval `(+ ,id 1)))
        "#,
        result: "100",
    }

    // Captured var reads the current value at capture time; later
    // rebinding of the original symbol does not affect the closure.
    tulisp_assert! {
        program: r#"
        (setq f (let ((x 1)) (lambda () x)))
        (setq x 999)
        (funcall f)
        "#,
        result: "1",
    }

    // A closure in a list can still be invoked via funcall after list
    // operations (doesn't rely on stack-top semantics).
    tulisp_assert! {
        program: r#"
        (setq fs (mapcar (lambda (n) (lambda () n)) '(10 20 30)))
        (mapcar 'funcall fs)
        "#,
        result: "'(10 20 30)",
    }

    // Recursive defun sees its own lex params correctly across calls.
    tulisp_assert! {
        program: r#"
        (defun fact (n)
          (if (<= n 1) 1 (* n (fact (- n 1)))))
        (fact 6)
        "#,
        result: "720",
    }

    // Regression: `(quote X)` written as a list form must not be
    // descended into for substitution, even if X names a defun param.
    // With the bug present, `(quote key)` would rewrite the literal
    // `key` symbol, breaking the subsequent `(assoc 'key ...)`.
    tulisp_assert! {
        program: r#"
        (defun pick (key alist)
          (cdr (assoc (quote key) alist)))
        (pick 'ignored '((key . the-key-value) (other . o)))
        "#,
        result: "'the-key-value",
    }

    // An anonymous lambda created inside a function body compiles
    // via the two-phase scheme (MakeLambda + inline Funcall). The
    // closure captures the enclosing defun param.
    tulisp_assert! {
        program: r#"
        (defun make-scaler (k)
          (lambda (x) (* k x)))
        (funcall (make-scaler 7) 6)
        "#,
        result: "42",
    }

    // Self-recursive via funcall-of-letrec-style closure. Exercises
    // the MakeLambda capturing its own just-bound slot.
    tulisp_assert! {
        program: r#"
        (setq fact (lambda (n) (if (<= n 1) 1 (* n (funcall fact (- n 1))))))
        (funcall fact 5)
        "#,
        result: "120",
    }

    // Regression: a closure captures a let-bound free var, takes a
    // param whose name matches that of the *caller's* defun param
    // (the caller's param is shadowed by its own let* with the same
    // name; also calls a defun — not defspecial — whose args list
    // carries placeholders that must be rewritten at phase 2).
    tulisp_assert! {
        program: r#"
        (defun make-scaler (seed)
          (let ((base (+ seed 100)))
            (lambda (v) (floor (+ v base)))))
        (defun wrap (id v)
          (let* ((fn (make-scaler 0))
                 (v (ftruncate (+ v 1))))
            (funcall fn v)))
        (wrap 1 5.5)
        "#,
        result: "106",
    }

    // Regression: a nested-closure scenario. An outer closure captures
    // a let-bound inner closure via `set`/`symbol-value` indirection,
    // and both closures take a param of the same name that is also
    // shadowed by a let*-bound var in the caller. Exercises label
    // registration + placeholder rewrite of the Push(args) AST inside
    // the closure's body.
    tulisp_assert! {
        program: r#"
        (defun sum-list (xs)
          (let ((acc 0))
            (dolist (x xs) (setq acc (+ acc x)))
            acc))
        (defun make-inner-check (xs)
          (let ((limit (sum-list xs)))
            (lambda (v) (<= v limit))))
        (defun install-outer-check (sym xs)
          (let ((inner-check (make-inner-check xs)))
            (set sym
              (lambda (v)
                (and (funcall inner-check v)
                     (> v 0))))))
        (install-outer-check 'my-check-fn '(10 20 30))
        (defun run-check (id v)
          (let* ((check-fn (symbol-value 'my-check-fn))
                 (v (ftruncate v)))
            (if (funcall check-fn v)
                'ok
              'out-of-bounds)))
        (list (run-check 1 30.5) (run-check 1 70.0) (run-check 1 -5.0))
        "#,
        result: "'(ok out-of-bounds out-of-bounds)",
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

#[track_caller]
fn assert_no_lex_stack_leak(ctx: &mut TulispContext, prog: &str, call: &str, label: &str) {
    let s0 = tulisp::debug_lex_stacks_total();
    for _ in 0..1000 {
        ctx.eval_string(call).unwrap_or_else(|e| {
            panic!("{}: eval failed: {}", label, e.format(ctx));
        });
    }
    let delta = tulisp::debug_lex_stacks_total() as i64 - s0 as i64;
    assert_eq!(
        delta, 0,
        "{}: leaked {} LEX_STACKS entries over 1000 calls. Program:\n{}",
        label, delta, prog
    );
}

/// Asserts that an erroring `call` doesn't leak either lex or special
/// (`defvar`) stack entries over 1000 invocations against a persistent
/// context. The call is *expected* to fail — `let _ = ...` swallows
/// the result so we measure cumulative state, not per-call success.
#[track_caller]
fn assert_no_scope_leak_on_error(ctx: &mut TulispContext, prog: &str, call: &str, label: &str) {
    let lex0 = tulisp::debug_lex_stacks_total();
    let spec0 = ctx.debug_special_stacks_total();
    for _ in 0..1000 {
        let _ = ctx.eval_string(call);
    }
    let lex_delta = tulisp::debug_lex_stacks_total() as i64 - lex0 as i64;
    let spec_delta = ctx.debug_special_stacks_total() as i64 - spec0 as i64;
    assert_eq!(
        (lex_delta, spec_delta),
        (0, 0),
        "{}: leaked lex={}, special={} entries over 1000 calls. Program:\n{}",
        label,
        lex_delta,
        spec_delta,
        prog
    );
}

#[test]
fn test_tail_call_does_not_leak_lex_stack() -> Result<(), Error> {
    // Regression: `mark_tail_calls` recurses into `let` / `let*` /
    // `progn` / `if` / `cond` bodies and rewrites the body's
    // tail-position call into a `Bounce`, which compiles to
    // `Instruction::TailCall`. That instruction unwinds the
    // surrounding `run_impl` directly, bypassing trailing
    // `Instruction::EndScope`s that `compile_fn_let_star` appends —
    // leaving let bindings stuck on `LEX_STACKS` permanently. The
    // fix injects the cleanup before each `TailCall` in the body.
    //
    // `dolist` / `dotimes` expand to `let` over `while`, and
    // `mark_tail_calls` does not enter `while`, so a loop body can't
    // contain a `tcall`. They're exercised here anyway as a guard
    // against a future regression and to confirm the
    // surrounding-let-scope fix still applies when the let body's tail
    // call comes after a loop form.

    // Helper: each case is a defun + a top-level call expression.
    // The defun's body shape is what we're testing.
    let cases: &[(&str, &str, &str)] = &[
        // Original repro: let body's tail is a tail-call.
        (
            "let_with_mapcar_tail",
            r#"(defvar v '(1.0 2.0 3.0))
               (defun f (power)
                 (let ((tot (seq-reduce '+ v 0.0)))
                   (mapcar (lambda (x) (* power (/ x tot))) v)))"#,
            "(f 10.0)",
        ),
        // let* with multiple bindings.
        (
            "let_star_multi_binding",
            r#"(defun f (n)
                 (let* ((a (* n 2))
                        (b (+ a 1)))
                   (mapcar (lambda (x) (+ x a b)) '(1 2 3))))"#,
            "(f 5)",
        ),
        // tcall through if both branches inside let.
        (
            "let_with_if_branches_tail",
            r#"(defun f (n)
                 (let ((acc (* n 2)))
                   (if (> n 0)
                       (mapcar (lambda (x) (+ x acc)) '(1 2 3))
                       (mapcar (lambda (x) (* x acc)) '(4 5 6)))))"#,
            "(f 5)",
        ),
        // tcall through cond branches inside let*.
        (
            "let_star_with_cond_branches_tail",
            r#"(defun f (n)
                 (let* ((a (* n 2)) (b (+ a 1)))
                   (cond ((= n 0) (mapcar (lambda (x) x) '(1 2 3)))
                         ((> n 0) (mapcar (lambda (x) (+ x a b)) '(1 2 3)))
                         (t (mapcar (lambda (x) (- x a)) '(1 2 3))))))"#,
            "(f 5)",
        ),
        // Nested let* — both layers must inject EndScopes before tcall.
        (
            "nested_let_star_tail",
            r#"(defun f (n)
                 (let ((a n))
                   (let ((b (* a 2)))
                     (mapcar (lambda (x) (+ x a b)) '(1 2 3)))))"#,
            "(f 5)",
        ),
        // A dolist body is NOT in tail position (mark_tail_calls
        // doesn't enter the `while` it expands to), so no tcall is
        // emitted inside the loop. But the loop can sit in a let whose
        // body's tail is a separate tcall after the loop.
        (
            "dolist_inside_let_with_trailing_tcall",
            r#"(defun f (xs)
                 (let ((acc 0))
                   (dolist (x xs) (setq acc (+ acc x)))
                   (mapcar (lambda (n) (+ n acc)) '(1 2 3))))"#,
            "(f '(1 2 3 4))",
        ),
        // Same with dotimes.
        (
            "dotimes_inside_let_with_trailing_tcall",
            r#"(defun f (n)
                 (let ((acc 0))
                   (dotimes (i n) (setq acc (+ acc i)))
                   (mapcar (lambda (x) (+ x acc)) '(1 2 3))))"#,
            "(f 5)",
        ),
        // dolist with no result form in tail position of a let —
        // nothing inside the loop is in tail position, so no tcall is
        // emitted in this defun's body. Confirms the no-leak baseline.
        (
            "dolist_as_tail",
            r#"(defun f (xs)
                 (let ((acc 0))
                   (dolist (x xs) (setq acc (+ acc x)))))"#,
            "(f '(1 2 3 4))",
        ),
        // When the loop is in tail position, so is its result form,
        // inside the loop's own bindings, so its tail call must pop
        // them first.
        (
            "dolist_result_is_tcall",
            r#"(defun f (n)
                 (if (= n 0) 0 (dolist (x '(1) (f (- n 1))))))"#,
            "(f 50)",
        ),
        (
            "dotimes_result_is_tcall",
            r#"(defun f (n)
                 (if (= n 0) 0 (dotimes (i 1 (f (- n 1))))))"#,
            "(f 50)",
        ),
        // Self tail-call from let body — `Bounce` form on the same
        // function name. The let bindings must be popped before the
        // function re-enters itself.
        (
            "let_body_self_tail_recursion",
            r#"(defun f (n acc)
                 (if (<= n 0)
                     acc
                     (let ((next (- n 1)))
                       (f next (+ acc n)))))"#,
            "(f 50 0)",
        ),
        // Lambda body with let* + tail-call. The lambda is materialized
        // per call to `g`; its compiled body must not leak either.
        (
            "lambda_body_let_star_tail",
            r#"(defun g (n)
                 (funcall (lambda (k)
                            (let* ((a (* k 2)) (b (+ a 1)))
                              (mapcar (lambda (x) (+ x a b)) '(1 2 3))))
                          n))"#,
            "(g 5)",
        ),
    ];

    // Fresh context per case so an earlier `(defun f ...)` doesn't
    // shadow the next case's `f` (and so `defvar`s don't leak between
    // shapes).
    for (label, prog, call) in cases {
        let mut ctx = TulispContext::new();
        eprintln!("case: {}", label);
        ctx.eval_string(prog)
            .unwrap_or_else(|e| panic!("{} setup failed: {}", label, e.format(&ctx)));
        // First, sanity-check: a single call works without panicking.
        ctx.eval_string(call)
            .unwrap_or_else(|e| panic!("{} sanity call failed: {}", label, e.format(&ctx)));
        assert_no_lex_stack_leak(&mut ctx, prog, call, label);
    }
    Ok(())
}

#[test]
fn test_error_escape_does_not_leak_scope() -> Result<(), Error> {
    // Regression: every `BeginScope` (let, let*, inline lambda body; dolist and
    // dotimes expand to let) used to leak its binding when the body errored
    // before the matching `EndScope`. `run_impl_inner` now tracks active scopes
    // via a Drop guard that unsets remaining entries on the error-unwind path.
    // See analysis.org a24.
    let cases: &[(&str, &str, &str)] = &[
        (
            "let_body_errors",
            "(defun f () (let ((y 5)) (error \"boom\")))",
            "(f)",
        ),
        (
            "let_multi_binding_later_rhs_errors",
            "(defun f () (let ((y 1) (z (error \"boom\"))) y))",
            "(f)",
        ),
        (
            "dolist_body_errors",
            "(defun f () (dolist (x '(1 2 3)) (if (= x 2) (error \"boom\") nil)))",
            "(f)",
        ),
        (
            "dotimes_body_errors",
            "(defun f () (dotimes (i 5) (if (= i 2) (error \"boom\") nil)))",
            "(f)",
        ),
        (
            "compiled_lambda_let_body_errors",
            "(defun caller () (funcall (lambda () (let ((y 5)) (error \"boom\")))))",
            "(caller)",
        ),
        (
            "closure_capture_then_inner_let_errors",
            "(defun caller () (let ((cap 1)) (funcall (lambda () (let ((y cap)) (error \"boom\"))))))",
            "(caller)",
        ),
        (
            "nested_let_outer_body_errors_after_inner_returns",
            "(defun f () (let ((a 1)) (let ((b 2)) b) (error \"boom\")))",
            "(f)",
        ),
        // Defvar (special) variants — the binding lives on the
        // symbol's `items` stack rather than `LEX_STACKS`. The
        // assert helper checks both.
        (
            "defvar_let_body_errors",
            "(progn (defvar yy 'g) (defun f () (let ((yy 'inner)) (error \"boom\"))))",
            "(f)",
        ),
        (
            "defvar_toplevel_let_body_errors",
            "(defvar yy 'g)",
            "(let ((yy 'inner)) (error \"boom\"))",
        ),
        (
            "defvar_multi_binding_later_rhs_errors",
            "(progn (defvar yy 'g) (defun f () (let ((yy 'inner) (z (error \"boom\"))) z)))",
            "(f)",
        ),
    ];
    for (label, prog, call) in cases {
        let mut ctx = TulispContext::new();
        eprintln!("case: {}", label);
        ctx.eval_string(prog)
            .unwrap_or_else(|e| panic!("{} setup failed: {}", label, e.format(&ctx)));
        // First, sanity-check: a single call really does error
        // (otherwise the test would tautologically pass).
        let single = ctx.eval_string(call);
        assert!(
            single.is_err(),
            "{}: expected error; got Ok({})",
            label,
            single.unwrap()
        );
        assert_no_scope_leak_on_error(&mut ctx, prog, call, label);
    }
    Ok(())
}

#[test]
fn test_missing_optional_does_not_leak_lex_stack() -> Result<(), Error> {
    // Regression: `init_defun_args` used to set_scope(nil) for a
    // missing `&optional` param then `continue` without pushing onto
    // `set_params`, so `SetParams::drop` never unset the binding.
    // Each call leaked one `LEX_STACKS` entry per missing optional.
    let cases: &[(&str, &str, &str)] = &[
        (
            "one_missing_optional",
            "(defun f (a &optional b) a)",
            "(f 1)",
        ),
        (
            "two_missing_optionals",
            "(defun f (a &optional b c) a)",
            "(f 1)",
        ),
        (
            "partial_optional_provided",
            "(defun f (a &optional b c) a)",
            "(f 1 2)",
        ),
        (
            "missing_optionals_with_rest",
            "(defun f (a &optional b c &rest r) a)",
            "(f 1)",
        ),
    ];
    for (label, prog, call) in cases {
        let mut ctx = TulispContext::new();
        ctx.eval_string(prog)
            .unwrap_or_else(|e| panic!("{} setup failed: {}", label, e.format(&ctx)));
        ctx.eval_string(call)
            .unwrap_or_else(|e| panic!("{} sanity call failed: {}", label, e.format(&ctx)));
        assert_no_lex_stack_leak(&mut ctx, prog, call, label);
    }
    Ok(())
}

#[test]
fn test_setq() -> Result<(), Error> {
    tulisp_assert! {
        program: r##"(let ((xx 10)) (setq zz (+ xx 10))) (* zz 3)"##,
        result: "60",
    }
    tulisp_assert! {
        program: r##"(let ((xx 10) (yy 'qq)) (setq zz (+ xx 10)) (set yy 20)) (* zz qq)"##,
        result: "400",
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
