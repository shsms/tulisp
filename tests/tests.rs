use std::fmt::Display;
use tulisp::{
    AsPlist, Error, Iter, Plist, Shared, TulispContext, TulispConvertible, TulispObject,
    destruct_eval_bind,
};

macro_rules! tulisp_assert {
    (@impl $ctx: expr, program:$input:expr, result:$result:expr $(,)?) => {
        let output = $ctx.tw_eval_string($input).map_err(|err| {
            panic!("{}:{}: execution failed: {}", file!(), line!(),err.format(&$ctx));

        })?;
        let expected = $ctx.tw_eval_string($result)?;
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

    (@impl_vm $ctx: expr, program:$input:expr, result:$result:expr $(,)?) => {
        let output = $ctx.eval_string($input).map_err(|err| {
            panic!("{}:{}: execution failed: {}", file!(), line!(),err.format(&$ctx));

        })?;
        let expected = $ctx.eval_string($result)?;
        assert!(
            output.equal(&expected),
            "\n{}:{}: program: {}\n  vm output: {},\n  expected: {}\n",
            file!(),
            line!(),
            $input,
            output,
            expected
        );
    };

    (@impl $ctx: expr, program:$input:expr, result_str:$result:expr $(,)?) => {
        let output = $ctx.tw_eval_string($input).map_err(|err| {
            println!("{}:{}: execution failed: {}", file!(), line!(),err.format(&$ctx));
            err
        })?;
        let expected = $ctx.tw_eval_string($result)?;
        assert_eq!(output.to_string(), expected.to_string(),
            "\n{}:{}: program: {}\n  output: {},\n  expected: {}\n",
            file!(),
            line!(),
            $input,
            output,
            expected
        );
    };

    (@impl_vm $ctx: expr, program:$input:expr, result_str:$result:expr $(,)?) => {
        let output = $ctx.eval_string($input).map_err(|err| {
            println!("{}:{}: execution failed: {}", file!(), line!(),err.format(&$ctx));
            err
        })?;
        let expected = $ctx.eval_string($result)?;
        assert_eq!(output.to_string(), expected.to_string(),
            "\n{}:{}: program: {}\n  vm output: {},\n  expected: {}\n",
            file!(),
            line!(),
            $input,
            output,
            expected
        );
    };

    (@impl $ctx: expr, program:$input:expr, error:$desc:expr $(,)?) => {
        let output = $ctx.tw_eval_string($input);
        assert!(output.is_err());
        assert_eq!(output.unwrap_err().format(&$ctx), $desc);
    };

    (@impl_vm $ctx: expr, program:$input:expr, error:$desc:expr $(,)?) => {
        let output = $ctx.eval_string($input);
        assert!(output.is_err());
        assert_eq!(output.unwrap_err().format(&$ctx), $desc);
    };

    (ctx: $ctx: expr, program: $($tail:tt)+) => {
        tulisp_assert!(@impl $ctx, program: $($tail)+);
        tulisp_assert!(@impl_vm $ctx, program: $($tail)+);
    };

    (program: $($tail:tt)+) => {
        let mut ctx = TulispContext::new();
        tulisp_assert!(@impl ctx, program: $($tail)+);
        let mut ctx = TulispContext::new();
        tulisp_assert!(@impl_vm ctx, program: $($tail)+);
    };
}

#[test]
fn test_comparison_of_numbers() -> Result<(), Error> {
    // Greater than
    tulisp_assert! { program: "(> 10 10)", result: "nil" }
    tulisp_assert! { program: "(> 10 5)", result: "t" }
    tulisp_assert! { program: "(> 5 10)", result: "nil" }
    tulisp_assert! { program: "(> 2 4 6)", result: "nil" }
    tulisp_assert! { program: "(> 2 6 4)", result: "nil" }
    tulisp_assert! { program: "(> 6 2 4)", result: "nil" }
    tulisp_assert! { program: "(> 6 4 2)", result: "t" }
    tulisp_assert! { program: "(> 10.0 5.0)", result: "t" }
    tulisp_assert! { program: "(> 5.0 10.0)", result: "nil" }
    tulisp_assert! {
        program: "(let ((a 10)) (> a))",
        error: r#"ERR OutOfRange: Comparison requires at least 2 arguments
<eval_string>:1.15-1.19:  at (> a)
<eval_string>:1.1-1.20:  at (let ((a 10)) (> a))
"#
    }
    tulisp_assert! {
        program: r#"(> 10 "hello")"#,
        error: r#"ERR TypeMismatch: Expected number, got: "hello"
<eval_string>:1.1-1.14:  at (> 10 "hello")
"#
    }

    // Greater than or equal
    tulisp_assert! { program: "(>= 10 10)", result: "t" }
    tulisp_assert! { program: "(>= 10 5)", result: "t" }
    tulisp_assert! { program: "(>= 5 10)", result: "nil" }
    tulisp_assert! { program: "(>= 2 4 6)", result: "nil" }
    tulisp_assert! { program: "(>= 2 6 4)", result: "nil" }
    tulisp_assert! { program: "(>= 6 2 4)", result: "nil" }
    tulisp_assert! { program: "(>= 6 4 2)", result: "t" }
    tulisp_assert! { program: "(>= 10.0 5.0)", result: "t" }
    tulisp_assert! { program: "(>= 5.0 10.0)", result: "nil" }
    tulisp_assert! {
        program: "(let ((a 10)) (>= a))",
        error: r#"ERR OutOfRange: Comparison requires at least 2 arguments
<eval_string>:1.15-1.20:  at (>= a)
<eval_string>:1.1-1.21:  at (let ((a 10)) (>= a))
"#
    }

    // Less than
    tulisp_assert! { program: "(< 10 10)", result: "nil" }
    tulisp_assert! { program: "(< 10 5)", result: "nil" }
    tulisp_assert! { program: "(< 5 10)", result: "t" }
    tulisp_assert! { program: "(< 2 4 6)", result: "t" }
    tulisp_assert! { program: "(< 2 6 4)", result: "nil" }
    tulisp_assert! { program: "(< 6 2 4)", result: "nil" }
    tulisp_assert! { program: "(< 6 4 2)", result: "nil" }
    tulisp_assert! { program: "(< 10.0 5.0)", result: "nil" }
    tulisp_assert! { program: "(< 5.0 10.0)", result: "t" }
    tulisp_assert! {
        program: "(let ((a 10)) (< a))",
        error: r#"ERR OutOfRange: Comparison requires at least 2 arguments
<eval_string>:1.15-1.19:  at (< a)
<eval_string>:1.1-1.20:  at (let ((a 10)) (< a))
"#
    }

    // Less than or equal
    tulisp_assert! { program: "(<= 10 10)", result: "t" }
    tulisp_assert! { program: "(<= 10 5)", result: "nil" }
    tulisp_assert! { program: "(<= 5 10)", result: "t" }
    tulisp_assert! { program: "(<= 2 4 6)", result: "t" }
    tulisp_assert! { program: "(<= 2 6 4)", result: "nil" }
    tulisp_assert! { program: "(<= 6 2 4)", result: "nil" }
    tulisp_assert! { program: "(<= 6 4 2)", result: "nil" }
    tulisp_assert! { program: "(<= 10.0 5.0)", result: "nil" }
    tulisp_assert! { program: "(<= 5.0 10.0)", result: "t" }
    tulisp_assert! {
        program: "(let ((a 10)) (<= a))",
        error: r#"ERR OutOfRange: Comparison requires at least 2 arguments
<eval_string>:1.15-1.20:  at (<= a)
<eval_string>:1.1-1.21:  at (let ((a 10)) (<= a))
"#
    }

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

    tulisp_assert! { program: "(and t t t)", result: "t" }
    tulisp_assert! { program: "(and t t nil)", result: "nil" }
    tulisp_assert! { program: "(and (> 10 5) (< 10 20))", result: "t" }
    tulisp_assert! { program: "(and (> 10 5) (> 10 20))", result: "nil" }
    tulisp_assert! { program: "(and (< 10 5) (> 10 20))", result: "nil" }

    tulisp_assert! { program: "(or t t t)", result: "t" }
    tulisp_assert! { program: "(or t t nil)", result: "t" }
    tulisp_assert! { program: "(or nil nil nil)", result: "nil" }
    tulisp_assert! { program: "(or (> 10 5) (< 10 20))", result: "t" }
    tulisp_assert! { program: "(or (> 10 5) (> 10 20))", result: "t" }
    tulisp_assert! { program: "(or (< 10 5) (> 10 20))", result: "nil" }

    tulisp_assert! { program: "(xor t t)", result: "nil" }
    tulisp_assert! { program: "(xor t nil)", result: "t" }
    tulisp_assert! { program: "(xor nil t)", result: "t" }
    tulisp_assert! { program: "(xor nil nil)", result: "nil" }
    tulisp_assert! { program: "(xor (> 10 5) (< 10 20))", result: "nil" }
    tulisp_assert! { program: "(xor (> 10 5) (> 10 20))", result: "t" }
    tulisp_assert! { program: "(xor (< 10 5) (< 10 20))", result: "t" }
    tulisp_assert! { program: "(xor (< 10 5) (> 10 20))", result: "nil" }

    tulisp_assert! {
        program: "(defun test (val) (if-let (a val) (+ a 10))) (test nil)",
        result: "nil",
    }
    tulisp_assert! {
        program: "(defun test (val) (if-let (a val) (+ a 10))) (test 10)",
        result: "20",
    }

    tulisp_assert! {
        program: "(macroexpand '(if-let (c) (+ c 10) 2))",
        result_str: "'(let* ((s (and t c))) (if s (+ c 10) 2))",
    }
    tulisp_assert! {
        program: "(defun test (&optional c) (if-let (c) (+ c 10) 2)) (list (test) (test 2))",
        result: "'(2 12)",
    }
    tulisp_assert! {
        program: "(defun test (&optional c) (if-let (q c) (+ q 10) 2)) (list (test) (test 2))",
        result: "'(2 12)",
    }
    tulisp_assert! {
        program: "(defun test (&optional c) (if-let ((q c)) (+ q 10) 2)) (list (test) (test 2))",
        result: "'(2 12)",
    }
    tulisp_assert! {
        program: "(defun test (&optional c d) (if-let ((q c) d) (+ q d 10) 2)) (list (test) (test 2) (test 2 3)) ",
        result: "'(2 2 15)",
    }
    tulisp_assert! {
        program: "(defun test (&optional c d) (if-let ((q c) d (w 10)) (+ q d w) 2)) (list (test) (test 2) (test 2 3)) ",
        result: "'(2 2 15)",
    }

    tulisp_assert! {
        program: "(macroexpand '(when-let (c) (+ c 10)))",
        result_str: "'(let* ((s (and t c))) (if s (+ c 10) nil))",
    }
    tulisp_assert! {
        program: "(macroexpand '(when-let (c) (+ c 10) 2))",
        result_str: "'(let* ((s (and t c))) (if s (progn (+ c 10) 2) nil))",
    }
    tulisp_assert! {
        program: "(macroexpand '(when-let ((q c) d (w 10)) 2 (+ c d w)))",
        result_str: r#"'
        (let* ((q (and t c))
               (d (and q d))
               (w (and d 10)))
          (if w
              (progn 2 (+ c d w))
            nil))
        "#,
    }

    tulisp_assert! {
        program: "(macroexpand '(while-let (c) (+ c 10)))",
        result_str: "'(while (let* ((s (and t c))) (if s (progn (+ c 10) t) nil)))",
    }
    tulisp_assert! {
        program: "(let ((ll '(1 2 3)) (vv 0)) (while-let (x (car ll))  (setq ll (cdr ll)) (setq vv (+ vv x))) vv)",
        result: "'6",
    }
    Ok(())
}

#[test]
fn test_defun() -> Result<(), Error> {
    tulisp_assert! {
        program: "(defun num () 4) (num)",
        result: "4",
    }
    tulisp_assert! {
        program: "(defun add (x y) (+ x y)) (add 10 20)",
        result: "30",
    }
    tulisp_assert! {
        program: r##"
            (defun add (x &optional y z)
              "Have a docstring."
              (+ x
                 (if y y -10)
                 (if z z -10)))

            (add
             (add 100)
             (add 10 20)
             (add 1 2 3))
        "##,
        result: "106",
    }
    tulisp_assert! {
        program: r##"
        (let ((res (test)))

        (defun test ()
          (let* ((a 5)
                 (c 7))
            (when-let ((b 6))
              (list a b c))))
        res)
        "##,
        result: "'(5 6 7)",
    }
    tulisp_assert! {
        program: r##"
            (defun add (x &rest y)
             (if y
                 (append y (list x))
               (list x)))

            (list (add 100) (add 10 20) (add 1 2 3))
        "##,
        result: "'((100) (20 10) (2 3 1))",
    }
    tulisp_assert! {
        program: "(defun j (&rest x y) nil) (j)",
        error: r#"ERR TypeMismatch: Too many &rest parameters
<eval_string>:1.1-1.25:  at (defun j (&rest x y) nil)
"#
    }
    tulisp_assert! {
        program: r##"(defun j (&rest x) x) (list (j) (j 10) (j 100 200))"##,
        result: "'(nil (10) (100 200))",
    }
    tulisp_assert! {
    program: r##"
        (defun add (x &optional y z &rest rest)
          (let ((ret (list (+ x (if y y 0) (if z z 0)))))
            (if rest (append ret rest) ret)))

        (list (add 100)
              (add 10 20)
              (add 1 2 3 4 5))
    "##,
    result: "'((100) (30) (6 4 5))",
        }
    tulisp_assert! {
        program: "(defun add (x y) (+ x y)) (add 10)",
        error: r#"ERR MissingArgument: Too few arguments
<eval_string>:1.27-1.34:  at (add 10)
"#
    }
    tulisp_assert! {
        program: "(defun add (x y) (+ x y)) (add 10 20 30)",
        error: r#"ERR InvalidArgument: Too many arguments
<eval_string>:1.27-1.40:  at (add 10 20 30)
"#
    }
    tulisp_assert! {
        program: "(defmacro num ()  4) (macroexpand '(num))",
        result: "4",
    }
    tulisp_assert! {
        program: r##"
        (defmacro inc (var)
          "Have a docstring"
          (list 'setq var (list '+ 1 var)))

        (macroexpand '(inc x))
        "##,
        result: "'(setq x (+ 1 x))",
    }
    tulisp_assert! {
        program: "(defmacro inc (var)  (list 'setq var (list '+ 1 var))) (let ((x 4)) (inc x))",
        result: "5",
    }
    tulisp_assert! {
        program: "(defmacro inc (var)  (list 'setq var (list '+ 1 var))) (let ((x 4)) (inc))",
        error: r#"ERR MissingArgument: Too few arguments
<eval_string>:1.69-1.73:  at (inc)
"#
    }
    tulisp_assert! {
        program: "(defmacro inc (var)  (list 'setq var (list '+ 1 var))) (let ((x 4)) (inc 4 5))",
        error: r#"ERR InvalidArgument: Too many arguments
<eval_string>:1.69-1.77:  at (inc 4 5)
"#
    }
    tulisp_assert! {
        program: "((lambda (v1 v2) (+ v1 v2)) 10 20)",
        result: "30",
    }

    Ok(())
}

#[test]
fn test_tco() -> Result<(), Error> {
    tulisp_assert! {
        program: r##"
        (defun if-tail (n acc)
          (if (equal n 0) acc (if-tail (- n 1) (+ acc 1))))

        (if-tail 30000 0)
        "##,
        result: "30000",
    }
    tulisp_assert! {
        program: r##"
        (defun cond-tail (n acc)
          (cond ((equal n 0) acc)
                (t (cond-tail (- n 1) (+ acc 1)))))

        (cond-tail 30000 0)
        "##,
        result: "30000",
    }
    tulisp_assert! {
        program: r##"
        (defun progn-tail (n acc)
          (let (next-n)
            (progn
              (setq next-n (- n 1))
              (setq n next-n)
              (if (equal n 0)
                  acc
                (progn-tail next-n (+ acc 1))))))

        (progn-tail 20001 0)
        "##,
        result: "20000",
    }
    tulisp_assert! {
        program: r##"
        (defun my-even (n) (if (equal n 0) t (my-odd (- n 1))))
        (defun my-odd (n) (if (equal n 0) nil (my-even (- n 1))))
        (list (my-even 5) (my-odd 5) (my-even 30000) (my-odd 10000))
        "##,
        result: "'(nil t t nil)",
    }
    Ok(())
}

#[test]
fn test_eval() -> Result<(), Error> {
    tulisp_assert! {
        program: "(eval '(mod 32 5))",
        result: "2",
    }

    tulisp_assert! {
        program: r#"(setq f 'list)(funcall f 'x 'y 'z)"#,
        result: "'(x y z)",
    }

    tulisp_assert! {
        program: r#"(funcall '1+ 4)"#,
        result: "5",
    }

    tulisp_assert! {
        program: r#"(funcall '+ 4 5)"#,
        result: "9",
    }

    tulisp_assert! {
        program: "(let ((x 10)) (funcall '+ x 2))",
        result: "12",
    }

    tulisp_assert! {
        program: "(let ((x 10)) (funcall (lambda (x y) (+ x y)) x 2))",
        result: "12",
    }

    tulisp_assert! {
        program: "(let ((x 10)) (funcall '(lambda (x y) (+ x y)) x 2))",
        result: "12",
    }
    tulisp_assert! {
        program: "(let ((y j) (j 10)) (funcall j))",
        error: r#"ERR Uninitialized: Variable definition is void: j
<eval_string>:1.1-1.32:  at (let ((y j) (j 10)) (funcall j))
"#
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
        error: r#"ERR MissingArgument: Too few arguments
<eval_string>:1.1-1.8:  at (cons 1)
"#
    };
    tulisp_assert! {
        program: "(cons 1 2 3)",
        error: r#"ERR InvalidArgument: Too many arguments
<eval_string>:1.1-1.12:  at (cons 1 2 3)
"#
    };
    Ok(())
}

#[test]
fn test_setq_invalid_target() -> Result<(), Error> {
    // `setq` rejects non-symbol and constant-symbol targets at compile
    // time; `set` rejects them at runtime. Regression: the VM used to
    // `.unwrap()` the result of `obj.set(...)`, which crashed on
    // `(setq t 5)`, `(setq nil 5)`, `(setq :foo 5)`, etc.
    tulisp_assert! {
        program: "(setq t 5)",
        error: r#"ERR TypeMismatch: Expected Symbol: Can't assign to t
<eval_string>:1.7-1.7:  at t
<eval_string>:1.1-1.10:  at (setq t 5)
"#
    };
    tulisp_assert! {
        program: "(setq nil 5)",
        error: r#"ERR TypeMismatch: Expected Symbol: Can't assign to nil
<eval_string>:1.7-1.9:  at nil
<eval_string>:1.1-1.12:  at (setq nil 5)
"#
    };
    tulisp_assert! {
        program: "(setq :foo 5)",
        error: r#"ERR TypeMismatch: Can't set constant symbol: :foo
<eval_string>:1.1-1.13:  at (setq :foo 5)
"#
    };
    tulisp_assert! {
        program: r#"(setq "x" 5)"#,
        error: r#"ERR TypeMismatch: Expected Symbol: Can't assign to "x"
<eval_string>:1.1-1.12:  at (setq "x" 5)
"#
    };
    tulisp_assert! {
        program: "(set 't 5)",
        error: r#"ERR TypeMismatch: Expected Symbol: Can't assign to t
<eval_string>:1.7-1.7:  at t
<eval_string>:1.1-1.10:  at (set 't 5)
"#
    };
    tulisp_assert! {
        program: "(set ':foo 5)",
        error: r#"ERR TypeMismatch: Can't set constant symbol: :foo
<eval_string>:1.1-1.13:  at (set ':foo 5)
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
        error: r#"ERR TypeMismatch: cxr: Not a Cons: 5
<eval_string>:1.1-1.7:  at (car 5)
"#
    };
    tulisp_assert! {
        program: "(cdr 5)",
        error: r#"ERR TypeMismatch: cxr: Not a Cons: 5
<eval_string>:1.1-1.7:  at (cdr 5)
"#
    };
    tulisp_assert! {
        program: "(cadr 7)",
        error: r#"ERR TypeMismatch: cxr: Not a Cons: 7
<eval_string>:1.1-1.8:  at (cadr 7)
"#
    };
    tulisp_assert! {
        program: "(cdddr \"abc\")",
        error: r#"ERR TypeMismatch: cxr: Not a Cons: "abc"
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
        (let ((items (list 10 20)))
          (setq items
                (append items
                        '(30 40)
                        (list (+ 8 42) 60)))
          items)
        "##,
        result: "'(10 20 30 40 50 60)",
    }

    tulisp_assert! {
        program: "(let ((items '(4 20 3 22 55))) (length items))",
        result: "5"
    }

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

    tulisp_assert! {
        program: r#"
        (setq items
              (append items '(10)))
        "#,
        error: r#"ERR Uninitialized: Variable definition is void: items
<eval_string>:3.15-3.34:  at (append items '(10))
<eval_string>:2.9-3.35:  at (setq items (append items '(10)))
"#
    }

    // Emacs `append` semantics: empty / single-arg / shared last arg /
    // dotted tail / no input mutation.
    tulisp_assert! { program: "(append)", result: "nil" }
    tulisp_assert! { program: "(append '(1 2 3))", result: "'(1 2 3)" }
    tulisp_assert! { program: "(append nil 77)", result: "77" }
    tulisp_assert! { program: "(append '(1 2) 3)", result: "'(1 2 . 3)" }
    tulisp_assert! {
        program: r##"
            (let ((xs '(1 2)))
              (append xs '(3 4))
              xs)
        "##,
        result: "'(1 2)",
    }

    tulisp_assert! {
        program: "(consp '(20))",
        result: "t",
    }

    tulisp_assert! {
        program: "(consp '20)",
        result: "nil",
    }

    tulisp_assert! {
        program: r##"(let ((res 0)) (dolist (vv '(20 30 50 33) res) (setq res (+ res vv))))"##,
        result: "133",
    }

    tulisp_assert! {
        program: "(let ((res 0)) (list (dotimes (vv 4) (setq res (+ res vv))) res))",
        result: "'(nil 6)",
    }

    tulisp_assert! {
        program: "(let ((res 0)) (list (dotimes (vv 4 res) (setq res (+ res vv))) res))",
        result: "'(6 6)",
    }

    tulisp_assert! {
        program: r##"
        (let ((vv '((name . "person") (age . 120))))
          (list (assoc 'age vv) (alist-get 'name vv) (alist-get 'names vv) (alist-get 'names vv "something") (alist-get 'name nil)))
        "##,
        result: r##"'((age . 120) "person" nil "something" nil)"##,
    }

    tulisp_assert! {
        program: r##"
        (let ((vv '((20 . "person") (30 . 120))))
          (list (assoc 30 vv 'eq)
                (assoc 30 vv 'equal)

                (alist-get 20 vv nil nil 'eq)
                (alist-get 20 vv nil nil 'equal)

                (alist-get 40 vv)
                (alist-get 40 vv nil nil 'equal)

                (alist-get 40 vv "something")
                (alist-get 40 vv "something" nil 'equal)))
        "##,
        result: r##"'((30 . 120) (30 . 120) "person" "person" nil nil "something" "something")"##,
    }

    tulisp_assert! {
        program: r#"
        (let ((plist '(:name person :dob "01.01.2001")))
          (list (plist-get plist :dob)
                (plist-get plist :name)
                (plist-get plist :age)))
        "#,
        result: r#"'("01.01.2001" person nil)"#,
    }

    tulisp_assert! {
        program: "(mapcar '1+ '(10 20 30))",
        result: "'(11 21 31)",
    }

    tulisp_assert! {
        program: r#"(mapcar (lambda (vv) (plist-get vv :age)) '((:name "person" :age 20) (:name "person2" :age 30)))"#,
        result: "'(20 30)",
    }
    Ok(())
}

#[test]
fn test_backquotes() -> Result<(), Error> {
    tulisp_assert! {
        program: "(let ((vv '(12 20 30))) `(,(car vv) ,@(cdr vv) ,(cdr vv)))",
        result: "'(12 20 30 (20 30))",
    }

    tulisp_assert! {
        program: r#"
        (let ((a 10))
          (eq 'a (cdr `(a . a))))
        "#,
        result: r#"t"#,
    }

    tulisp_assert! {
        program: r#"
        (let ((a 10))
          (cdr `(a . ,a)))
        "#,
        result: r#"10"#,
    }

    tulisp_assert! {
        program: r#"`(1 2 '(+ 10 20)  ',(+ 10 20)  (quote ,(+ 20 20)))"#,
        result: r#"'(1 2 '(+ 10 20) '30 (quote 40))"#,
    }

    tulisp_assert! {
        program: r#"`(1 2 ,,(+ 10 20))"#,
        error: r#"ERR SyntaxError: Unquote without backquote
<eval_string>:1.7-1.7:  at ,,(+ 10 20)
<eval_string>:1.1-1.1:  at `(1 2 ,,(+ 10 20))
"#,
    }

    // Nested backquote: `,,x` (depth 2 → 1 → 0) evaluates `x` at the
    // outer backquote level; `,y` at depth 2 reduces to depth 1 and
    // is preserved for the inner backquote to resolve later.
    // Matches Emacs (verified with `emacs --batch`).
    tulisp_assert! {
        program: r#"
        (let ((x 1) (y 2))
          `(a `(b ,,x ,y) c))
        "#,
        result: r#"'(a `(b ,1 ,y) c)"#,
    }

    // Single-comma at depth 2 stays as data (no eval) — both `,x`
    // and `,y` reduce to depth 1, preserved for the inner backquote.
    tulisp_assert! {
        program: r#"
        (let ((x 1) (y 2))
          `(a `(b ,x ,y) c))
        "#,
        result: r#"'(a `(b ,x ,y) c)"#,
    }

    // Dotted-tail double-comma resolves at the outer level.
    tulisp_assert! {
        program: r#"
        (let ((x 1))
          `(a `(b . ,,x)))
        "#,
        result: r#"'(a `(b . ,1))"#,
    }

    // `,x` inside `(quote ...)` inside outer backquote: the quote's
    // content is walked, `,x` evaluates at the outer level, and the
    // result wraps in a Quote. Matches Emacs.
    tulisp_assert! {
        program: r#"
        (let ((x 1))
          `(a (quote (,x)) c))
        "#,
        result: r#"'(a (quote (1)) c)"#,
    }

    // Outer `'` makes everything inside data: nothing evaluates, no
    // matter how deeply nested the backquote / unquote forms are.
    // `(let ((x 5)) '...)` shows the let-bound `x` is *not* picked
    // up by `,,x` inside the quote.
    tulisp_assert! {
        program: r#"
        (let ((x 5))
          '(`(,,x)))
        "#,
        result: r#"'(`(,,x))"#,
    }

    // Nested-backquote double-comma evaluating a `CompiledDefun`
    // (anonymous lambdas compile to bytecode): native compilation
    // emits `Funcall` for `(funcall f)`, no re-entry into `ctx.vm`.
    tulisp_assert! {
        program: r#"
        (setq f (lambda () 42))
        ``(,,(funcall f))
        "#,
        result: r#"'`(,42)"#,
    }

    // Same case but via runtime `(eval ...)` — the form is wrapped
    // in `'` so the VM compiler doesn't see the inner backquotes;
    // at runtime the `eval` defun receives the quoted data and
    // hands it to `ctx.eval` (TW), which walks the nested backquote
    // and reaches the `CompiledDefun` for `f` while the outer
    // `eval_string` is already running on the VM. The TW
    // `funcall::CompiledDefun` arm then re-enters via
    // `bytecode::run_lambda`, sharing `ctx.vm` with the outer run.
    tulisp_assert! {
        program: r#"
        (setq f (lambda () 42))
        (eval '``(,,(funcall f)))
        "#,
        result: r#"'`(,42)"#,
    }

    // Top-level `(defun …)` stores a `TulispValue::Lambda` (not a
    // `CompiledDefun`), so the same shape resolves through the TW
    // `Lambda` arm without re-entering the VM.
    tulisp_assert! {
        program: r#"
        (defun f () 42)
        (eval '``(,,(f)))
        "#,
        result: r#"'`(,42)"#,
    }

    Ok(())
}

#[test]
fn test_math() -> Result<(), Error> {
    tulisp_assert! {
        program: "(/ 10 0)",
        error: r#"ERR OutOfRange: Division by zero
<eval_string>:1.1-1.8:  at (/ 10 0)
"#,
    }
    tulisp_assert! {
        program: "(/ 0 10)",
        result: "0",
    }
    tulisp_assert! {
        program: "(let ((a 10) (b 0)) (/ a b))",
        error: r#"ERR OutOfRange: Division by zero
<eval_string>:1.21-1.27:  at (/ a b)
<eval_string>:1.1-1.28:  at (let ((a 10) (b 0)) (/ a b))
"#,
    }

    tulisp_assert! { program: "(/ 24 2 2)",                result: "6"     }
    tulisp_assert! { program: "(+ 40 (* 2.5 4) (- 4 12))", result: "42.0"  }
    tulisp_assert! { program: "(+ 40 (* 2.5 4) (- -1 7))", result: "42.0"  }
    tulisp_assert! { program: "(mod 32 5)",                result: "2"     }
    tulisp_assert! { program: "(min 12 5 45)",             result: "5"     }
    tulisp_assert! { program: "(max 12 5 45.2 8)",         result: "45.2"  }

    tulisp_assert! { program: "(< 8 32)",      result: "t"   }
    tulisp_assert! { program: "(< 80 32)",     result: "nil" }
    tulisp_assert! { program: "(<= 32 32)",    result: "t"   }
    tulisp_assert! { program: "(<= 8 32)",     result: "t"   }
    tulisp_assert! { program: "(<= 80 32)",    result: "nil" }
    tulisp_assert! { program: "(> 8 32)",      result: "nil" }
    tulisp_assert! { program: "(> 80 32)",     result: "t"   }
    tulisp_assert! { program: "(>= 32 32)",    result: "t"   }
    tulisp_assert! { program: "(>= 8 32)",     result: "nil" }
    tulisp_assert! { program: "(>= 80 32)",    result: "t"   }
    tulisp_assert! { program: "(equal 8 8)",   result: "t"   }
    tulisp_assert! { program: "(equal 8 4)",   result: "nil" }
    tulisp_assert! { program: "(equal 8.0 8)", result: "t"   }
    tulisp_assert! { program: "(equal 8.0 4)", result: "nil" }

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
        error: r#"ERR MissingArgument: Too few arguments
<eval_string>:1.1-1.8:  at (fround)
"#,
    }
    tulisp_assert! {
        program: "(fround 3.14 3.14)",
        error: r#"ERR InvalidArgument: Too many arguments
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

    // Quote inside backquote: 'a stays literal, ,b is substituted.
    tulisp_assert! {
        program: r#"
        (let ((b 42))
          `('a ,b))
        "#,
        result: "'('a 42)",
    }

    // let* sequential binding — later bindings see earlier ones.
    tulisp_assert! {
        program: r#"
        (let* ((a 1) (b (+ a 10)) (c (+ a b))) (list a b c))
        "#,
        result: "'(1 11 12)",
    }

    // dolist under Emacs' `lexical-binding: t` binds the loop variable
    // freshly at each iteration (roughly `(while tail (let ((i (car
    // tail))) body))`), so each captured closure sees its own value.
    tulisp_assert! {
        program: r#"
        (setq fns nil)
        (dolist (i '(1 2 3))
          (setq fns (cons (lambda () i) fns)))
        (mapcar 'funcall fns)
        "#,
        result: "'(3 2 1)",
    }

    // dotimes behaves like dolist: fresh binding per iteration.
    tulisp_assert! {
        program: r#"
        (setq fns nil)
        (dotimes (j 3)
          (setq fns (cons (lambda () j) fns)))
        (mapcar 'funcall fns)
        "#,
        result: "'(2 1 0)",
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

    // Regression: literal symbol in a backquote alist key position must
    // NOT be rewritten even when it shares a name with a lambda param.
    // With the bug present, `k` in `(k . literal)` would be substituted
    // to a LexicalBinding wrapper, so `(assoc 'k entry)` would miss.
    tulisp_assert! {
        program: r#"
        (defun make-entry (k v)
          `((key . ,k) (value . ,v) (k . literal-k)))
        (let ((entry (make-entry 'foo 42)))
          (list (cdr (assoc 'key entry))
                (cdr (assoc 'value entry))
                (cdr (assoc 'k entry))))
        "#,
        result: "'(foo 42 literal-k)",
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

    // Regression: a backquote whose unquoted data contains literal
    // symbols matching enclosing lambda params must stay usable as a
    // lambda form. The bug turned inner literal keys into
    // LexicalBindings, which then errored when the stored form was
    // re-evaluated by funcall.
    tulisp_assert! {
        program: r#"
        (defun make-resetter (items)
          `(lambda ()
             (dolist (item (quote ,items))
               (cdr (assoc 'value item)))))
        (let ((form (make-resetter '(((value . 1)) ((value . 2))))))
          (funcall (eval form))
          'ok)
        "#,
        result: "'ok",
    }

    // VM-specific: an anonymous lambda created inside a function body
    // must compile via the two-phase scheme (MakeLambda + inline
    // Funcall) without falling back to the TW. Closure captures the
    // enclosing defun param.
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
fn test_vm_reentry_during_run() -> Result<(), Error> {
    // VM re-entry from inside a VM run. The outer `eval_string`
    // is mid-run on `ctx.vm` when the `eval` defun receives the
    // quoted form and hands it to `ctx.eval` (TW); TW's `funcall`
    // arm reaches a callable defined on the same context and
    // dispatches it, ultimately landing back in the VM. The inner
    // and outer runs share the same machine — the inner sees the
    // outer's bytecode/labels and pushes/pops on the shared stack.
    //
    // Pre-rewrite (when `ctx.vm` was `Option<Machine>` taken at
    // the run boundary), the inner entry found `ctx.vm = None`
    // and panicked with "ctx.vm taken twice — VM re-entered
    // during a run". Free-function dispatch with a direct
    // `ctx.vm` field makes re-entry transparent.

    // Lambda case: `f` holds a `CompiledDefun` (anonymous lambda
    // materialized by `Instruction::MakeLambda`). TW funcall hits
    // the `CompiledDefun` arm in `eval::funcall`, which calls
    // `bytecode::run_lambda` — that's the inner VM run.
    let mut ctx = TulispContext::new();
    let result: i64 = ctx
        .eval_string(
            r#"
        (setq f (lambda () 42))
        (eval '(funcall f))
        "#,
        )?
        .try_into()?;
    assert_eq!(result, 42);

    // Defun case: top-level `(defun g …)` registers a
    // `CompiledDefun` in `ctx.vm.bytecode.functions` and stores a
    // `TulispValue::Lambda` on the symbol's function slot. TW
    // funcall on the symbol resolves to the `Lambda` (not the
    // `CompiledDefun`), dispatches via `eval::funcall`'s `Lambda`
    // arm, and the body call eventually reaches the VM-registered
    // function — exercising the same re-entry pathway with a
    // different setup shape.
    let mut ctx = TulispContext::new();
    let result: i64 = ctx
        .eval_string(
            r#"
        (defun g () 99)
        (eval '(funcall 'g))
        "#,
        )?
        .try_into()?;
    assert_eq!(result, 99);
    Ok(())
}

#[test]
fn test_funcall_compiled_defun_through_tw() -> Result<(), Error> {
    // Regression for the cross-path bug: a lambda stored on a symbol
    // via VM evaluation materializes as a `CompiledDefun`. Then
    // calling a TW-evaluated defun (here: via `ctx.funcall` from
    // Rust) whose body funcalls through `(symbol-value '…)` exposed
    // the bug — the TW `funcall` defspecial was dispatching
    // `CompiledDefun` through `DummyEval`, leaving `LexicalBinding`
    // AST nodes in the args. The VM then `set_scope`'d the
    // CompiledDefun's params with those LexicalBindings, and the
    // first typed-arg call downstream surfaced as
    // `TypeMismatch: Expected number, got: <name>`.
    //
    // Single-path tulisp_assert! can't reproduce this — VM funcalls
    // go through `funcall_inline` (which handles values correctly),
    // and TW evaluation of `(lambda …)` produces a `Lambda` not a
    // `CompiledDefun`. The cross-path is unique to "VM-eval the
    // setup, then TW-eval the call" — exactly what microsim does
    // when its gRPC handlers `ctx.funcall(set-power-active, …)`.
    let mut ctx = TulispContext::new();
    ctx.defun("rust-needs-num", |v: f64| -> f64 { v });
    // VM-eval: `inner-fn` ends up holding a `CompiledDefun`
    // (materialized by `Instruction::MakeLambda` at runtime), and
    // `outer` is set up via the TW `defun` defspecial during parse
    // so calling it through `ctx.funcall` runs the TW path.
    ctx.eval_string(
        r#"
        (set 'inner-fn (lambda (x) (rust-needs-num x)))
        (defun outer (id v)
          (funcall (symbol-value 'inner-fn) v))
        "#,
    )?;
    let outer = ctx.intern("outer");
    let args = TulispObject::nil();
    args.push(1i64.into())?;
    args.push(42.0_f64.into())?;
    let result = ctx.funcall(&outer, &args)?;
    assert!(
        result.equal(&42.0_f64.into()),
        "expected 42, got {}",
        result
    );
    Ok(())
}

#[test]
fn test_plist_defun_callable_from_vm_run() -> Result<(), Error> {
    // Regression for the cross-path bug specific to `Plist<T>`-arg
    // defuns. Pre-rewrite, the `plist_args` arms in
    // `context/callable.rs` registered via `ctx.defspecial` and
    // produced a `TulispValue::Func`. From inside a VM run, calling
    // such a defun went through `RustCall` → the closure's
    // `Plist::new(ctx, rest)` → `ctx.eval(value)` per pair → if
    // `value` was itself a `CompiledDefun` call, `eval::funcall`
    // re-acquired `ctx.vm.borrow_mut()` — deadlock under `sync`,
    // RefCell panic otherwise.
    //
    // The fix routes Plist-arg defuns through the typed-arg path
    // (`define_typed_defun` → `RustCallTyped`). Args arrive
    // already-evaluated; the callback rebuilds the plist shape with
    // values wrapped in `quote` so `Plist::new`'s per-value eval is
    // a no-op.

    AsPlist! {
        struct Cfg {
            x: i64,
            y: i64,
            tag: String,
            xs: Vec<i64>,
        }
    }

    let mut ctx = TulispContext::new();
    ctx.defun("cfg-summary", |c: Plist<Cfg>| -> String {
        format!(
            "{}={}({} sum={})",
            c.tag,
            c.x + c.y,
            c.xs.len(),
            c.xs.iter().sum::<i64>(),
        )
    });

    // VM-compile a defun whose body calls `cfg-summary` with arg
    // expressions that are themselves CompiledDefun calls (`mkx`,
    // `mky`, `mktag`) and a quoted list value (`'(1 2 3)`). Each
    // arg's evaluated form ends up in the typed-defun's args slice
    // and must round-trip through Plist::new without re-eval.
    // `(defun mktag () "answer")` would strip the string as a
    // docstring and leave the body empty. Use `progn` to force the
    // string to be the actual return value.
    ctx.eval_string(
        r#"
        (defun mkx () 10)
        (defun mky () 32)
        (defun mktag () (progn "answer"))
        (defun outer ()
          (cfg-summary :x (mkx) :y (mky) :tag (mktag) :xs '(1 2 3)))
        "#,
    )?;

    let outer = ctx.intern("outer");
    let result = ctx.funcall(&outer, &TulispObject::nil())?;
    assert_eq!(result.as_string()?, "answer=42(3 sum=6)");

    // Also exercise the inverse path: TW-eval the call to make sure
    // the typed-arg path also works through `eval::funcall`'s
    // `Defun` arm.
    let tw_result = ctx.eval_string(r#"(cfg-summary :x 1 :y 2 :tag "tw" :xs '(4 5 6))"#)?;
    assert_eq!(tw_result.as_string()?, "tw=3(3 sum=15)");

    Ok(())
}

#[test]
fn test_mutual_tail_recursion_is_tco() -> Result<(), Error> {
    // `mark_tail_calls` now also marks tail calls to other VM defuns
    // as `Bounce`, so mutual recursion compiles to `Instruction::TailCall`
    // (loop-style unwind) rather than nested `Instruction::Call`
    // (per-cycle Rust frame). A pre-pass in `compile_progn` registers
    // every top-level `(defun NAME PARAMS …)`'s arity before
    // compiling any body, so cycles get full TCO without forward
    // declarations.
    //
    // Pin the behavior with a depth that would blow a non-TCO Rust
    // stack: 200,000 alternations of even?/odd? = 200,000 hops.
    // Without TCO the test thread's stack overflows.
    let mut ctx = TulispContext::new();
    ctx.eval_string(
        r#"
        (defun even? (n)
          (if (= n 0) t (odd? (- n 1))))
        (defun odd? (n)
          (if (= n 0) nil (even? (- n 1))))
        "#,
    )?;
    let r = ctx.eval_string("(even? 200000)")?;
    assert!(r.is_truthy(), "even? 200000 should be true, got {}", r);
    let r = ctx.eval_string("(odd? 200001)")?;
    assert!(r.is_truthy(), "odd? 200001 should be true, got {}", r);

    Ok(())
}

#[test]
fn test_mutual_tail_call_arity_checked_at_compile_time() -> Result<(), Error> {
    // Non-self bounce path now arity-checks at compile time when the
    // target is a known VM defun. Catches mismatches without running
    // the program — same shape as the self-bounce and
    // `TulispValue::Defun` checks.

    // Too few: helper takes 2, called with 1 in tail position.
    let mut ctx = TulispContext::new();
    let err = ctx.eval_string(
        r#"
        (defun helper (a b) (+ a b))
        (defun caller () (helper 1))
        "#,
    );
    let msg = err.unwrap_err().format(&ctx);
    assert!(
        msg.contains("too few arguments") || msg.contains("Too few arguments"),
        "expected too-few error from mutual tail-call, got: {}",
        msg
    );

    // Too many: helper takes 1, called with 3.
    let mut ctx = TulispContext::new();
    let err = ctx.eval_string(
        r#"
        (defun helper (a) a)
        (defun caller () (helper 1 2 3))
        "#,
    );
    let msg = err.unwrap_err().format(&ctx);
    assert!(
        msg.contains("too many arguments") || msg.contains("Too many arguments"),
        "expected too-many error from mutual tail-call, got: {}",
        msg
    );

    // Cyclic mutual recursion (a calls b, b calls a) defined in
    // either order — pre-pass populates both arities first, so
    // mark_tail_calls catches mismatches whichever direction is
    // wrong.
    let mut ctx = TulispContext::new();
    let err = ctx.eval_string(
        r#"
        (defun a (n) (if (= n 0) 'done (b)))     ; b takes 1, called with 0
        (defun b (n) (if (= n 0) 'done (a (- n 1))))
        "#,
    );
    let msg = err.unwrap_err().format(&ctx);
    assert!(
        msg.contains("too few arguments") || msg.contains("Too few arguments"),
        "expected too-few error in cyclic case, got: {}",
        msg
    );

    Ok(())
}

#[test]
fn test_self_tail_recursion_arity_checked_at_compile_time() -> Result<(), Error> {
    // `mark_tail_calls` rewrites self-recursive tail calls into
    // `(Bounce f args …)`, which `compile_fn_defun_bounce_call`
    // compiles into the in-place arg-rebind + `Jump(Pos::Abs(0))`
    // shape (no `Instruction::Call`). That path arity-checks against
    // `compiler.defun_args[name]` at compile time and reports the
    // mismatch instead of silently wrapping a usize subtraction.

    // Too many: 2 required, called with 3.
    let mut ctx = TulispContext::new();
    let err = ctx.eval_string(
        r#"
        (defun f (a b)
          (if (= a 0) b (f (- a 1) (+ b a) (* b 2))))
        "#,
    );
    let msg = err.unwrap_err().format(&ctx);
    assert!(
        msg.contains("too many arguments") || msg.contains("Too many arguments"),
        "expected too-many error from self tail-call, got: {}",
        msg
    );

    // Too few: 2 required, called with 1. This previously underflowed
    // `args_count - params.required.len()` (usize) and surfaced a
    // misleading "too many" error in release mode.
    let mut ctx = TulispContext::new();
    let err = ctx.eval_string(
        r#"
        (defun f (a b)
          (if (= a 0) b (f (- a 1))))
        "#,
    );
    let msg = err.unwrap_err().format(&ctx);
    assert!(
        msg.contains("too few arguments") || msg.contains("Too few arguments"),
        "expected too-few error from self tail-call, got: {}",
        msg
    );

    // Sanity: matching arity compiles + runs cleanly.
    let mut ctx = TulispContext::new();
    let r = ctx.eval_string(
        r#"
        (defun sum-to (n acc)
          (if (= n 0) acc (sum-to (- n 1) (+ acc n))))
        (sum-to 10 0)
        "#,
    )?;
    assert_eq!(r.try_int()?, 55);

    Ok(())
}

#[test]
fn test_trace_distinguishes_call_sites_of_same_function() -> Result<(), Error> {
    // Two call sites of `bad` in the same defun body, each at a
    // different source position. When the error fires, the
    // backtrace must name the *specific* call site that ran, not
    // collapse them into a single representative entry. Pins both
    // the TW path (`eval_basic`'s recursive `with_trace`) and the
    // VM path (`strip_trace_markers` lifting per-form ranges) into
    // exact match — which only works if both attribute the call
    // form's distinct `TulispObject` (with its own span) for each
    // hit.
    //
    // The leading newlines in the program string anchor the
    // expected source positions: `bad` sits on line 2, `caller` on
    // line 3, so the call sites have stable column ranges across
    // runs.

    // Error on the *second* call site `(bad 2)`.
    let mut ctx = TulispContext::new();
    ctx.eval_string(
        r#"
(defun bad (n) (if (= n 2) (error "boom-on-2") nil))
(defun caller () (progn (bad 1) (bad 2) 'done))
"#,
    )?;
    let expected_call2 = "ERR LispError: boom-on-2\n\
        <eval_string>:2.28-2.46:  at (error \"boom-on-2\")\n\
        <eval_string>:2.16-2.51:  at (if (= n 2) (error \"boom-on-2\") nil)\n\
        <eval_string>:3.33-3.39:  at (bad 2)\n\
        <eval_string>:3.18-3.46:  at (progn (bad 1) (bad 2) 'done)\n\
        <eval_string>:1.1-1.8:  at (caller)\n";
    assert_eq!(
        ctx.eval_string("(caller)").unwrap_err().format(&ctx),
        expected_call2,
        "TW trace for boom-on-2"
    );
    assert_eq!(
        ctx.eval_string("(caller)").unwrap_err().format(&ctx),
        expected_call2,
        "VM trace for boom-on-2"
    );

    // Same shape, but error on the *first* call site `(bad 1)`.
    // The expected backtrace differs only at the call-site frame:
    // span and form text both move from `(bad 2)` to `(bad 1)`.
    let mut ctx = TulispContext::new();
    ctx.eval_string(
        r#"
(defun bad (n) (if (= n 1) (error "boom-on-1") nil))
(defun caller () (progn (bad 1) (bad 2) 'done))
"#,
    )?;
    let expected_call1 = "ERR LispError: boom-on-1\n\
        <eval_string>:2.28-2.46:  at (error \"boom-on-1\")\n\
        <eval_string>:2.16-2.51:  at (if (= n 1) (error \"boom-on-1\") nil)\n\
        <eval_string>:3.25-3.31:  at (bad 1)\n\
        <eval_string>:3.18-3.46:  at (progn (bad 1) (bad 2) 'done)\n\
        <eval_string>:1.1-1.8:  at (caller)\n";
    assert_eq!(
        ctx.eval_string("(caller)").unwrap_err().format(&ctx),
        expected_call1,
        "TW trace for boom-on-1"
    );
    assert_eq!(
        ctx.eval_string("(caller)").unwrap_err().format(&ctx),
        expected_call1,
        "VM trace for boom-on-1"
    );

    // Nested same-function call: `(bad (bad -1))`. The inner
    // `(bad -1)` is in argument position (non-tail) and the outer
    // `(bad …)` is in tail position. The inner call's call-site
    // form `(bad -1)` shows up in the trace; the outer collapses
    // into the `mark_tail_calls`-rewritten `(list Bounce bad …)`
    // form. (Tail-position call sites generally don't preserve
    // their original `(NAME ARGS…)` text in traces because the
    // rewrite replaces it with the Bounce shape — that's a
    // pre-existing TW property the VM mirrors.)
    let mut ctx = TulispContext::new();
    ctx.eval_string(
        r#"
(defun bad (n) (if (= n -1) (error "nested-boom") n))
(defun caller () (bad (bad -1)))
"#,
    )?;
    let expected_nested = "ERR LispError: nested-boom\n\
        <eval_string>:2.29-2.49:  at (error \"nested-boom\")\n\
        <eval_string>:2.16-2.52:  at (if (= n -1) (error \"nested-boom\") n)\n\
        <eval_string>:3.23-3.30:  at (bad -1)\n\
        <eval_string>:3.18-3.31:  at (list Bounce bad (bad -1))\n\
        <eval_string>:1.1-1.8:  at (caller)\n";
    assert_eq!(
        ctx.eval_string("(caller)").unwrap_err().format(&ctx),
        expected_nested,
        "TW trace for nested same-function call"
    );
    assert_eq!(
        ctx.eval_string("(caller)").unwrap_err().format(&ctx),
        expected_nested,
        "VM trace for nested same-function call"
    );

    Ok(())
}

#[test]
fn test_typed_defun_arity_checked_before_arg_eval() -> Result<(), Error> {
    // `TulispValue::Defun` carries arity metadata so the dispatchers
    // (compile_form for VM, eval::funcall for TW) can reject
    // mismatches before the user's closure runs. The TW path also
    // checks BEFORE evaluating any arg expression, so a too-many-
    // args call doesn't side-effect through the extras.
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
    let r = ctx.tw_eval_string("(narrow (bump 7))")?;
    assert_eq!(r.try_int()?, 7);
    assert_eq!(counter.load(Ordering::Relaxed), 1);

    // TW path: too-many should error before any (bump …) runs.
    counter.store(0, Ordering::Relaxed);
    let err = ctx.tw_eval_string("(narrow (bump 1) (bump 2) (bump 3))");
    let msg = err.unwrap_err().format(&ctx);
    assert!(
        msg.starts_with("ERR InvalidArgument: Too many arguments"),
        "expected too-many error, got: {}",
        msg
    );
    assert_eq!(
        counter.load(Ordering::Relaxed),
        0,
        "args evaluated before arity check fired"
    );

    // TW path: too-few also errors before (bump …) for the args
    // that were present.
    counter.store(0, Ordering::Relaxed);
    let err = ctx.tw_eval_string("(narrow)");
    let msg = err.unwrap_err().format(&ctx);
    assert!(
        msg.starts_with("ERR MissingArgument: Too few arguments"),
        "expected too-few error, got: {}",
        msg
    );
    assert_eq!(counter.load(Ordering::Relaxed), 0);

    // VM path: the same call through the VM-backed `eval_string`
    // should be rejected at compile time, also before any arg
    // side-effects.
    counter.store(0, Ordering::Relaxed);
    let err = ctx.eval_string("(narrow (bump 1) (bump 2) (bump 3))");
    let msg = err.unwrap_err().format(&ctx);
    assert!(
        msg.starts_with("ERR InvalidArgument: Too many arguments"),
        "expected vm too-many error, got: {}",
        msg
    );
    assert_eq!(counter.load(Ordering::Relaxed), 0);

    Ok(())
}

#[test]
fn test_substitute_lexical_skips_binders() -> Result<(), Error> {
    // `substitute_lexical` used to descend into the parameter /
    // varname positions of `lambda` / `let` / `let*` / `dolist` /
    // `dotimes` and substitute names there too. The inner form's
    // compiler then wrapped the already-`LexicalBinding` name in a
    // fresh `LexicalBinding` — a double wrap. A `debug_assert!` in
    // `TulispObject::lexical_binding` panics on any double-wrap, so
    // these compile-and-run shapes are the regression test.
    //
    // Each shape has an outer binder (defun param or let-bound var)
    // whose name is reused as a binder *inside* the body. Before
    // the fix, the outer substitute_lexical wrote into the inner
    // binder's declaration position, then the inner compiler
    // double-wrapped.

    // 1. defun param `x` reused as a let* var.
    tulisp_assert! {
        program: r#"
        (defun shadow-let (x)
          (let* ((x (+ x 1)))
            x))
        (shadow-let 10)
        "#,
        result: "11",
    }

    // 2. defun param `&optional sep` shadowed by `(let* ((sep (or sep "")))…)`.
    //    This is the `mapconcat` shape from the prelude.
    tulisp_assert! {
        program: r#"
        (defun joiner (xs &optional sep)
          (let* ((sep (or sep "-")))
            (mapconcat (lambda (x) (format "%S" x)) xs sep)))
        (list (joiner '(a b c)) (joiner '(a b c) "/"))
        "#,
        result: r##"'("a-b-c" "a/b/c")"##,
    }

    // 3. defun param `x` reused as a nested lambda's param.
    tulisp_assert! {
        program: r#"
        (defun shadow-lambda (x)
          (let ((fn (lambda (x) (* x 10))))
            (funcall fn 5)))
        (shadow-lambda 99)
        "#,
        result: "50",
    }

    // 4. defun param `x` reused as a `dolist` var.
    tulisp_assert! {
        program: r#"
        (defun shadow-dolist (x)
          (let ((acc 0))
            (dolist (x '(1 2 3))
              (setq acc (+ acc x)))
            acc))
        (shadow-dolist 99)
        "#,
        result: "6",
    }

    // 5. defun param `i` reused as a `dotimes` var.
    tulisp_assert! {
        program: r#"
        (defun shadow-dotimes (i)
          (let ((acc 0))
            (dotimes (i 4)
              (setq acc (+ acc i)))
            acc))
        (shadow-dotimes 99)
        "#,
        result: "6",
    }

    // 6. let* binder `x` referenced inside its own init expression
    //    (the prior x), then shadowed for the body.
    tulisp_assert! {
        program: r#"
        (let* ((x 1)
               (x (+ x 10))
               (x (* x 2)))
          x)
        "#,
        result: "22",
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

#[test]
fn test_tail_call_does_not_leak_lex_stack() -> Result<(), Error> {
    // Regression: `mark_tail_calls` recurses into `let` / `let*` /
    // `progn` / `if` / `cond` bodies and rewrites the body's
    // tail-position call into a `Bounce`, which compiles to
    // `Instruction::TailCall`. That instruction unwinds the
    // surrounding `run_function` directly, bypassing trailing
    // `Instruction::EndScope`s that `compile_fn_let_star` appends —
    // leaving let bindings stuck on `LEX_STACKS` permanently. The
    // fix injects the cleanup before each `TailCall` in the body.
    //
    // `dolist` / `dotimes` aren't recursed into by `mark_tail_calls`,
    // so the body of those forms can't contain a `tcall`. They're
    // exercised here anyway as a guard against a future regression
    // and to confirm the surrounding-let-scope fix still applies
    // when the let body's tail call comes after a loop form.

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
        // dolist body is NOT in tail position (mark_tail_calls doesn't
        // recurse into dolist), so no tcall is emitted inside the
        // dolist. But dolist itself can sit in a let whose body's
        // tail is a separate tcall after the loop.
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
        // dolist itself in tail position of a let — mark_tail_calls
        // does NOT mark anything inside the dolist, so no tcall is
        // emitted in this defun's body. Confirms the no-leak baseline.
        (
            "dolist_as_tail",
            r#"(defun f (xs)
                 (let ((acc 0))
                   (dolist (x xs) (setq acc (+ acc x)))))"#,
            "(f '(1 2 3 4))",
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

// `defvar`-declared variables are dynamic (special) — references resolve
// through the symbol's own stack, so eval-in-a-different-scope sees the
// enclosing binding. This matches Emacs' behavior under `lexical-binding: t`.
#[test]
fn test_defvar_dynamic_binding() -> Result<(), Error> {
    // With defvar, eval in a different function scope sees the let
    // binding through the symbol's dynamic stack — the let binding pushes onto
    // the symbol's dynamic stack, and eval inside run-eval sees it.
    tulisp_assert! {
        program: r#"
        (defvar xdyn nil)
        (defun run-eval (form) (eval form))
        (let ((xdyn 7))
          (run-eval 'xdyn))
        "#,
        result: "7",
    }

    // The backquote-quoted-and-eval'd-later pattern — works once the
    // variable is defvar'd so dynamic binding survives the scope jump.
    tulisp_assert! {
        program: r#"
        (defvar xbq nil)
        (defun run-eval (form) (eval form))
        (let ((xbq 42))
          (run-eval '`(+ ,xbq 1)))
        "#,
        result: "'(+ 42 1)",
    }

    // setq on a dynamic var in outer scope is visible after let scope
    // exits: the global slot gets updated, not a fresh lex slot.
    tulisp_assert! {
        program: r#"
        (defvar counter 0)
        (defun bump () (setq counter (+ counter 1)))
        (bump) (bump) (bump)
        counter
        "#,
        result: "3",
    }

    // Dynamic binding unwinds properly on let exit — outer value is
    // restored.
    tulisp_assert! {
        program: r#"
        (defvar k 100)
        (let ((k 1))
          (let ((k 2)) k)
          k)
        "#,
        result: "1",
    }

    // A closure referencing a dynamic var reads the current dynamic
    // binding at call time, not a snapshot from capture time.
    tulisp_assert! {
        program: r#"
        (defvar d 1)
        (setq f (lambda () d))
        (let ((d 99))
          (funcall f))
        "#,
        result: "99",
    }

    // Defun/lambda parameters are lexically bound even when the name
    // was declared `defvar` — matching Emacs' byte-compiler under
    // `lexical-binding: t`. The param binding does not shadow the
    // dynamic global, so another function that references the same
    // symbol sees the outer dynamic value, not the caller's arg.
    tulisp_assert! {
        program: r#"
        (defvar p 10)
        (defun observe () p)
        (defun with-p (p) (observe))
        (with-p 55)
        "#,
        result: "10",
    }

    // Direct reference to the param inside the defun sees the lex
    // binding even though the name is defvar'd.
    tulisp_assert! {
        program: r#"
        (defvar p2 10)
        (defun read-param (p2) p2)
        (read-param 55)
        "#,
        result: "55",
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
fn test_defvar() -> Result<(), Error> {
    // Sets when unbound.
    tulisp_assert! {
        program: "(defvar foo-new 42) foo-new",
        result: "42",
    }
    // Leaves an already-bound value alone.
    tulisp_assert! {
        program: "(setq foo-existing 1) (defvar foo-existing 99) foo-existing",
        result: "1",
    }
    // Accepts an optional docstring.
    tulisp_assert! {
        program: r#"(defvar foo-doc 7 "docs") foo-doc"#,
        result: "7",
    }
    // Returns the symbol.
    tulisp_assert! {
        program: "(defvar foo-sym 1)",
        result: "'foo-sym",
    }
    Ok(())
}

#[test]
fn test_while() -> Result<(), Error> {
    tulisp_assert! {
        program: "(let ((vv 0)) (while (< vv 42) (setq vv (+ 1 vv))) vv)",
        result: "42",
    }
    Ok(())
}

#[test]
fn test_sequences() -> Result<(), Error> {
    tulisp_assert! {
        program: "(seq-map #'1+ '(2 4 6))",
        result: "'(3 5 7)",
    }
    tulisp_assert! {
        program: r#"(seq-filter #'numberp '(2 4 6 "hello" 8))"#,
        result: "'(2 4 6 8)",
    }
    tulisp_assert! {
        program: r#"(seq-filter (lambda (x) (> x 5)) '(2 4 6 8))"#,
        result: "'(6 8)",
    }
    tulisp_assert! {
        program: r##"
        (let
            ((items '(2 4 6 "hello" 8)))
         (list (seq-find #'numberp items) (seq-find #'stringp items)))
        "##,
        result: r#"'(2 "hello")"#,
    }
    tulisp_assert! {
        program: r##"
        (seq-reduce #'+ '(2 4 6 8) 0)
        "##,
        result: "20",
    }
    tulisp_assert! {
        program: r##"
        (seq-reduce (lambda (x y) (+ x y)) '(2 4 6 8) 5)
        "##,
        result: "25",
    }
    Ok(())
}

#[test]
fn test_sort() -> Result<(), Error> {
    tulisp_assert! {
        program: "(sort '(20 10 30 15 45) '<)",
        result: "'(10 15 20 30 45)",
    }
    tulisp_assert! {
        program: "(sort '(20 10 30 15 45) '>)",
        result: "'(45 30 20 15 10)",
    }
    // With a `>`-typed predicate applied to strings, the inner
    // `funcall` hits a number-only operator and errors. The exact
    // string that trips it depends on the sort walk order; `hello`
    // happens to be first under the current Lisp implementation.
    //
    // The trace frames inside the prelude carry the crate-absolute
    // path to `prelude.lisp` (see `vm_eval_prelude` in `context.rs`),
    // so we inject that at compile time via `CARGO_MANIFEST_DIR`.
    let prelude = concat!(env!("CARGO_MANIFEST_DIR"), "/src/builtin/prelude.lisp");
    tulisp_assert! {
        program: r#"(sort '("sort" "hello" "a" "world") '>)"#,
        error: format!(r#"ERR TypeMismatch: Expected number, got: "hello"
{0}:67.35-67.55:  at (funcall pred item x)
{0}:67.15-67.56:  at (and (not inserted) (funcall pred item x))
{0}:67.11-71.36:  at (if (and (not inserted) (funcall pred item x)) (progn (setq new (cons item new))...
{0}:66.9-71.37:  at (dolist (x out) (if (and (not inserted) (funcall pred item x)) (progn (setq new ...
{0}:65.7-74.33:  at (let ((inserted nil) (new nil)) (dolist (x out) (if (and (not inserted) (funcall...
{0}:64.5-74.34:  at (dolist (item seq) (let ((inserted nil) (new nil)) (dolist (x out) (if (and (not...
{0}:63.3-75.8:  at (let ((out nil)) (dolist (item seq) (let ((inserted nil) (new nil)) (dolist (x o...
<eval_string>:1.1-1.39:  at (sort '("sort" "hello" "a" "world") '>)
"#, prelude),
    }
    tulisp_assert! {
        program: r#"(sort '("sort" "hello" "a" "world") 'string<)"#,
        result: r#"'("a" "hello" "sort" "world")"#,
    }
    tulisp_assert! {
        program: r#"(sort '("sort" "hello" "a" "world") 'string>)"#,
        result: r#"'("world" "sort" "hello" "a")"#,
    }
    // With sort now implemented in Lisp, resolution of an unknown
    // predicate fails at the inner `funcall` callsite rather than at
    // the outer Rust-side arg-eval, so the trace has the sort body's
    // inner frames. The root error (unbound `<<`) is unchanged.
    tulisp_assert! {
        program: "(sort '(20 10 30 15 45) '<<)",
        error: format!(r#"ERR Uninitialized: Variable definition is void: <<
{0}:67.35-67.55:  at (funcall pred item x)
{0}:67.15-67.56:  at (and (not inserted) (funcall pred item x))
{0}:67.11-71.36:  at (if (and (not inserted) (funcall pred item x)) (progn (setq new (cons item new))...
{0}:66.9-71.37:  at (dolist (x out) (if (and (not inserted) (funcall pred item x)) (progn (setq new ...
{0}:65.7-74.33:  at (let ((inserted nil) (new nil)) (dolist (x out) (if (and (not inserted) (funcall...
{0}:64.5-74.34:  at (dolist (item seq) (let ((inserted nil) (new nil)) (dolist (x out) (if (and (not...
{0}:63.3-75.8:  at (let ((out nil)) (dolist (item seq) (let ((inserted nil) (new nil)) (dolist (x o...
<eval_string>:1.1-1.28:  at (sort '(20 10 30 15 45) '<<)
"#, prelude)
    }
    tulisp_assert! {
        program: "(sort '(20 10 30 15 45))",
        error: r#"ERR MissingArgument: Too few arguments
<eval_string>:1.1-1.24:  at (sort '(20 10 30 15 45))
"#,
    }
    tulisp_assert! {
        program: "(defun << (v1 v2) (> v1 v2)) (sort '(20 10 30 15 45) '<<)",
        result: "'(45 30 20 15 10)",
    }

    tulisp_assert! {
        program: "(sort '(20 10 30 15 45) '(lambda (v1 v2) (> v1 v2)))",
        result: "'(45 30 20 15 10)",
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

    ctx.defspecial("d.run", move |_, _| Ok(d.run().into()));

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

    ctx.defspecial("add_ints", |_ctx, args| {
        destruct_eval_bind!(_ctx, (ints) = args);

        let ints: Iter<i64> = ints.iter()?;

        Ok({
            let mut sums = 0;
            for next in ints {
                sums += next?;
            }
            sums
        }
        .into())
    });

    tulisp_assert! {
        ctx: ctx,
        program: "(add_ints '(10 20 30))",
        result: "60",
    }
    tulisp_assert! {
        ctx: ctx,
        program: "(add_ints 20)",
        error: r#"ERR TypeMismatch: Expected a list, got 20
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

    impl TulispConvertible for TestStruct {
        fn from_tulisp(value: &TulispObject) -> Result<Self, Error> {
            match value.as_any() {
                Ok(value) => match value.downcast_ref::<TestStruct>() {
                    Some(v) => Ok(v.clone()),
                    None => Err(Error::type_mismatch("Expected TestStruct".to_string())),
                },
                Err(_) => Err(Error::type_mismatch("Expected TestStruct".to_string())),
            }
        }
        fn into_tulisp(self) -> TulispObject {
            Shared::new(self).into()
        }
    }

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
        error: r#"ERR TypeMismatch: Expected TestStruct
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
        error: r#"ERR MissingArgument: Too few arguments
<eval_string>:1.1-1.14:  at (maybe_add 10)
"#
    }

    Ok(())
}

#[test]
fn test_load() -> Result<(), Error> {
    let mut ctx = TulispContext::new();

    tulisp_assert! {
        ctx: ctx,
        program: r#"(load "tests/good-load.lisp")"#,
        result: "'(1 2 3)",
    }

    tulisp_assert! {
        ctx: ctx,
        program: r#"(load "tests/bad-load.lisp")"#,
        error: r#"ERR ParsingError: Unexpected closing parenthesis
tests/bad-load.lisp:1.9-1.9:  at nil
<eval_string>:1.1-1.28:  at (load "tests/bad-load.lisp")
"#
    }

    ctx.set_load_path(Some("tests/"))?;
    tulisp_assert! {
        ctx: ctx,
        program: r#"(load "good-load.lisp")"#,
        result: "'(1 2 3)",
    }

    Ok(())
}

#[test]
fn test_macroexpand() -> Result<(), Error> {
    let mut ctx = TulispContext::new();
    tulisp_assert! {
        ctx: ctx,
        program: r#"
        (defmacro make (alist)
          (setq make-args alist)
          t)

        (eval (list 'make `(,(cons 'a 1) ,(cons 'b 2))))

        make-args
        "#,
        result: r#"'((a . 1) (b . 2))"#,
    }

    tulisp_assert! {
        ctx: ctx,
        program: r#"
        (macroexpand '(make ((a . 1) (b . 2) (c . 3))))

        make-args
        "#,
        result: r#"'((a . 1) (b . 2) (c . 3))"#,
    }

    tulisp_assert! {
        ctx: ctx,
        program: r#"
        (make ((a . 1) (b . 2) (c . 3) (d . 4)))

        make-args
        "#,
        result: r#"'((a . 1) (b . 2) (c . 3) (d . 4))"#,
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
fn test_predicates() -> Result<(), Error> {
    tulisp_assert! {
        program: "(list (keywordp :a) (keywordp ':abcd) (keywordp 'abcd) (keywordp nil))",
        result: "'(t t nil nil)"
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

#[cfg(feature = "etags")]
mod etags_tests {
    use std::io::Write;
    use tulisp::TulispContext;

    fn write_temp_file(name: &str, content: &str) -> (std::path::PathBuf, impl Drop + use<>) {
        let dir = std::env::temp_dir().join("tulisp_etags_test");
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join(name);
        let mut f = std::fs::File::create(&path).unwrap();
        write!(f, "{}", content).unwrap();
        struct Cleanup(std::path::PathBuf);
        impl Drop for Cleanup {
            fn drop(&mut self) {
                std::fs::remove_file(&self.0).ok();
            }
        }
        let cleanup = Cleanup(path.clone());
        (path, cleanup)
    }

    /// Assert that the tags output contains a tag entry line with the given
    /// function/macro name between the \x7f and \x01 delimiters.
    #[track_caller]
    fn assert_tag_entry(tags: &str, name: &str) {
        let pattern = format!("\x7f{}\x01", name);
        assert!(
            tags.contains(&pattern),
            "tags output should contain an entry for `{name}`, got: {tags}"
        );
    }

    #[test]
    fn test_etags_defun_tracking() -> Result<(), tulisp::Error> {
        let (path, _cleanup) =
            write_temp_file("defun_test.el", "(defun my-test-func (x) (+ x 1))\n");
        let mut ctx = TulispContext::new();
        let path_str = path.to_str().unwrap();
        let tags = ctx.tags_table(Some(&[path_str]))?;
        assert_tag_entry(&tags, "my-test-func");
        Ok(())
    }

    #[test]
    fn test_etags_defmacro_tracking() -> Result<(), tulisp::Error> {
        let (path, _cleanup) =
            write_temp_file("defmacro_test.el", "(defmacro my-test-macro (x) x)\n");
        let mut ctx = TulispContext::new();
        let path_str = path.to_str().unwrap();
        let tags = ctx.tags_table(Some(&[path_str]))?;
        assert_tag_entry(&tags, "my-test-macro");
        Ok(())
    }

    #[test]
    fn test_etags_multiple_definitions() -> Result<(), tulisp::Error> {
        let (path, _cleanup) = write_temp_file(
            "multi_test.el",
            "(defun func-a () 1)\n(defun func-b () 2)\n(defmacro macro-c (x) x)\n",
        );
        let mut ctx = TulispContext::new();
        let path_str = path.to_str().unwrap();
        let tags = ctx.tags_table(Some(&[path_str]))?;
        assert_tag_entry(&tags, "func-a");
        assert_tag_entry(&tags, "func-b");
        assert_tag_entry(&tags, "macro-c");
        Ok(())
    }

    #[test]
    fn test_etags_defvar_tracking() -> Result<(), tulisp::Error> {
        let (path, _cleanup) = write_temp_file(
            "defvar_test.el",
            r#"(defvar my-test-var 42)
(defvar my-test-var-with-doc 7 "docs")
"#,
        );
        let mut ctx = TulispContext::new();
        let path_str = path.to_str().unwrap();
        let tags = ctx.tags_table(Some(&[path_str]))?;
        assert_tag_entry(&tags, "my-test-var");
        assert_tag_entry(&tags, "my-test-var-with-doc");
        Ok(())
    }

    #[test]
    fn test_etags_builtin_functions_tracked() -> Result<(), tulisp::Error> {
        let mut ctx = TulispContext::new();
        let tags = ctx.tags_table(None)?;
        assert!(
            !tags.is_empty(),
            "tags table should contain builtin entries"
        );
        // Verify at least some well-known builtins have proper tag entries.
        assert_tag_entry(&tags, "if");
        assert_tag_entry(&tags, "let");
        Ok(())
    }

    #[test]
    fn test_etags_output_contains_filename() -> Result<(), tulisp::Error> {
        let (path, _cleanup) =
            write_temp_file("filename_test.el", "(defun filename-test-fn () 42)\n");
        let mut ctx = TulispContext::new();
        let path_str = path.to_str().unwrap();
        let tags = ctx.tags_table(Some(&[path_str]))?;
        assert!(
            tags.contains(path_str),
            "tags output should reference the filename, got: {tags}"
        );
        assert_tag_entry(&tags, "filename-test-fn");
        Ok(())
    }

    #[test]
    fn test_etags_multiple_files() -> Result<(), tulisp::Error> {
        let (path1, _c1) = write_temp_file("file1.el", "(defun fn-from-file1 () 1)\n");
        let (path2, _c2) = write_temp_file("file2.el", "(defun fn-from-file2 () 2)\n");
        let mut ctx = TulispContext::new();
        let p1 = path1.to_str().unwrap();
        let p2 = path2.to_str().unwrap();
        let tags = ctx.tags_table(Some(&[p1, p2]))?;
        assert_tag_entry(&tags, "fn-from-file1");
        assert_tag_entry(&tags, "fn-from-file2");
        assert!(tags.contains(p1), "should contain first file path");
        assert!(tags.contains(p2), "should contain second file path");
        Ok(())
    }

    #[test]
    fn test_etags_follow_load() -> Result<(), tulisp::Error> {
        let (path2, _c2) = write_temp_file("loaded.el", "(defun loaded-fn () 42)\n");
        let p2_str = path2.to_str().unwrap();
        let (path1, _c1) = write_temp_file(
            "loader.el",
            &format!("(defun loader-fn () 1)\n(load \"{}\")\n", p2_str),
        );
        let mut ctx = TulispContext::new();
        let p1_str = path1.to_str().unwrap();
        let tags = ctx.tags_table(Some(&[p1_str]))?;
        assert_tag_entry(&tags, "loader-fn");
        assert_tag_entry(&tags, "loaded-fn");
        assert!(
            tags.contains(p2_str),
            "should contain the loaded file's path, got: {tags}"
        );
        Ok(())
    }
}
