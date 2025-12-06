use std::{fmt::Display, rc::Rc};
use tulisp::{Error, Iter, TulispAny, TulispContext, TulispObject, destruct_eval_bind};

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
        tulisp_assert!(@impl $ctx, program: $($tail)+)
    };

    (program: $($tail:tt)+) => {
        let mut ctx = TulispContext::new();
        tulisp_assert!(ctx: ctx, program: $($tail)+)
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
        error: r#"ERR OutOfRange: > requires at least 2 arguments
<eval_string>:1.15-1.20:  at (> a)
<eval_string>:1.1-1.21:  at (let ((a 10)) (> a))
"#
    }
    tulisp_assert! {
        program: r#"(> 10 "hello")"#,
        error: r#"ERR TypeMismatch: Expected number, found: "hello"
<eval_string>:1.8-1.14:  at "hello"
<eval_string>:1.1-1.15:  at (> 10 "hello")
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
        error: r#"ERR OutOfRange: >= requires at least 2 arguments
<eval_string>:1.15-1.21:  at (>= a)
<eval_string>:1.1-1.22:  at (let ((a 10)) (>= a))
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
        error: r#"ERR OutOfRange: < requires at least 2 arguments
<eval_string>:1.15-1.20:  at (< a)
<eval_string>:1.1-1.21:  at (let ((a 10)) (< a))
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
        error: r#"ERR OutOfRange: <= requires at least 2 arguments
<eval_string>:1.15-1.21:  at (<= a)
<eval_string>:1.1-1.22:  at (let ((a 10)) (<= a))
"#
    }

    Ok(())
}

#[test]
fn test_comparison_of_strings() -> Result<(), Error> {
    tulisp_assert! { program: r#"(string< "hello" "world")"#, result: "t" }
    tulisp_assert! { program: r#"(string< "hello" "hello")"#, result: "nil" }
    tulisp_assert! { program: r#"(string< "world" "hello")"#, result: "nil" }
    tulisp_assert! { program: r#"(string> "hello" "world")"#, result: "nil" }
    tulisp_assert! { program: r#"(string> "hello" "hello")"#, result: "nil" }
    tulisp_assert! { program: r#"(string> "world" "hello")"#, result: "t" }
    tulisp_assert! { program: r#"(string= "hello" "world")"#, result: "nil" }
    tulisp_assert! { program: r#"(string= "hello" "hello")"#, result: "t" }
    tulisp_assert! { program: r#"(string= "world" "hello")"#, result: "nil" }
    tulisp_assert! { program: r#"(string-lessp "hello" "world")"#, result: "t" }
    tulisp_assert! { program: r#"(string-lessp "hello" "hello")"#, result: "nil" }
    tulisp_assert! { program: r#"(string-lessp "world" "hello")"#, result: "nil" }
    tulisp_assert! { program: r#"(string-greaterp "hello" "world")"#, result: "nil" }
    tulisp_assert! { program: r#"(string-greaterp "hello" "hello")"#, result: "nil" }
    tulisp_assert! { program: r#"(string-greaterp "world" "hello")"#, result: "t" }
    tulisp_assert! { program: r#"(string-equal "hello" "world")"#, result: "nil" }
    tulisp_assert! { program: r#"(string-equal "hello" "hello")"#, result: "t" }
    tulisp_assert! { program: r#"(string-equal "world" "hello")"#, result: "nil" }
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
<eval_string>:1.1-1.26:  at (defun j (&rest x y) nil)
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
        error: r#"ERR TypeMismatch: Too few arguments
<eval_string>:1.27-1.35:  at (add 10)
"#
    }
    tulisp_assert! {
        program: "(defun add (x y) (+ x y)) (add 10 20 30)",
        error: r#"ERR TypeMismatch: Too many arguments
<eval_string>:1.27-1.41:  at (add 10 20 30)
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
        error: r#"ERR TypeMismatch: Too few arguments
<eval_string>:1.69-1.74:  at (inc)
"#
    }
    tulisp_assert! {
        program: "(defmacro inc (var)  (list 'setq var (list '+ 1 var))) (let ((x 4)) (inc 4 5))",
        error: r#"ERR TypeMismatch: Too many arguments
<eval_string>:1.69-1.78:  at (inc 4 5)
"#
    }
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
        program: "((lambda (v1 v2) (+ v1 v2)) 10 20)",
        result: "30",
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
        error: r#"ERR TypeMismatch: Variable definition is void: j
<eval_string>:1.10-1.11:  at j
<eval_string>:1.1-1.33:  at (let ((y j) (j 10)) (funcall j))
"#
    }
    tulisp_assert! {
        program: "(let ((j 10)) (+ j j))(+ j j)",
        error: r#"ERR TypeMismatch: Variable definition is void: j
<eval_string>:1.26-1.27:  at j
<eval_string>:1.23-1.30:  at (+ j j)
"#
    }
    Ok(())
}

#[test]
fn test_strings() -> Result<(), Error> {
    tulisp_assert! {
        program: r##"(concat 'hello 'world)"##,
        error: r#"ERR TypeMismatch: Not a string: hello
<eval_string>:1.1-1.23:  at (concat 'hello 'world)
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
        error: r#"ERR TypeMismatch: cons requires exactly 2 arguments
<eval_string>:1.1-1.9:  at (cons 1)
"#
    };
    tulisp_assert! {
        program: "(cons 1 2 3)",
        error: r#"ERR TypeMismatch: cons requires exactly 2 arguments
<eval_string>:1.1-1.13:  at (cons 1 2 3)
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
<eval_string>:1.1-1.8:  at (quote)
"#
    };
    tulisp_assert! {
        program: "(quote 1 2)",
        error: r#"ERR TypeMismatch: quote: expected one argument
<eval_string>:1.1-1.12:  at (quote 1 2)
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
        error: r#"ERR TypeMismatch: Variable definition is void: items
<eval_string>:3.23-3.28:  at items
<eval_string>:3.15-3.35:  at (append items '(10))
<eval_string>:2.9-3.36:  at (setq items (append items '(10)))
"#
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
        program: r#"`(1 2 '(+ 10 20)  ',(+ 10 20)  (quote ,(+ 20 20)))"#,
        result: r#"'(1 2 '(+ 10 20) '30 (quote 40))"#,
    }

    tulisp_assert! {
        program: r#"`(1 2 ,,(+ 10 20))"#,
        error: r#"ERR TypeMismatch: Unquote without backquote
<eval_string>:1.7-1.8:  at ,,(+ 10 20)
<eval_string>:1.1-1.2:  at `(1 2 ,,(+ 10 20))
"#,
    }

    Ok(())
}

#[test]
fn test_math() -> Result<(), Error> {
    tulisp_assert! {
        program: "(/ 10 0)",
        error: r#"ERR Undefined: Division by zero
<eval_string>:1.1-1.9:  at (/ 10 0)
"#,
    }
    tulisp_assert! {
        program: "(/ 0 10)",
        result: "0",
    }
    tulisp_assert! {
        program: "(let ((a 10) (b 0)) (/ a b))",
        error: r#"ERR Undefined: Division by zero
<eval_string>:1.21-1.28:  at (/ a b)
<eval_string>:1.1-1.29:  at (let ((a 10) (b 0)) (/ a b))
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

    tulisp_assert! {
        program: "(fround)",
        error: r#"ERR MissingArgument: fround: expected 1 argument.
<eval_string>:1.1-1.9:  at (fround)
"#,
    }
    tulisp_assert! {
        program: "(fround 3.14 3.14)",
        error: r#"ERR MissingArgument: fround: expected only 1 argument.
<eval_string>:1.1-1.19:  at (fround 3.14 3.14)
"#,
    }

    Ok(())
}

#[test]
fn test_let() -> Result<(), Error> {
    tulisp_assert! {
        program: "(let ((kk) (vv (+ 55 1)) (jj 20)) (append kk (+ vv jj 1)))",
        result: "'(77)",
    }
    tulisp_assert! {
        program: "(let (kk (vv (+ 55 1)) (jj 20)) (append kk (+ vv jj 1)))",
        result: "'(77)",
    }
    tulisp_assert! {
        program: r#"
        (let ((vv (+ 55 1))
              (jj 20))
          (append kk (+ vv jj 1)))
        "#,
        error: r#"ERR TypeMismatch: Variable definition is void: kk
<eval_string>:4.19-4.21:  at kk
<eval_string>:4.11-4.34:  at (append kk (+ vv jj 1))
<eval_string>:2.9-4.35:  at (let ((vv (+ 55 1)) (jj 20)) (append kk (+ vv jj 1)))
"#
    }
    tulisp_assert! {
        program: "(let ((22 (+ 55 1)) (jj 20)) (+ vv jj 1))",
        error: r#"ERR TypeMismatch: Expected Symbol: Can't assign to 22
<eval_string>:1.8-1.10:  at 22
<eval_string>:1.1-1.42:  at (let ((22 (+ 55 1)) (jj 20)) (+ vv jj 1))
"#
    }
    tulisp_assert! {
        program: "(let (18 (vv (+ 55 1)) (jj 20)) (+ vv jj 1))",
        error: r#"ERR SyntaxError: varitems inside a let-varlist should be a var or a binding: 18
<eval_string>:1.1-1.45:  at (let (18 (vv (+ 55 1)) (jj 20)) (+ vv jj 1))
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
    tulisp_assert! {
        program: r#"(sort '("sort" "hello" "a" "world") '>)"#,
        error: r#"ERR TypeMismatch: Expected number, found: "world"
<eval_string>:1.29-1.35:  at "world"
<eval_string>:1.1-1.40:  at (sort '("sort" "hello" "a" "world") '>)
"#,
    }
    tulisp_assert! {
        program: r#"(sort '("sort" "hello" "a" "world") 'string<)"#,
        result: r#"'("a" "hello" "sort" "world")"#,
    }
    tulisp_assert! {
        program: r#"(sort '("sort" "hello" "a" "world") 'string>)"#,
        result: r#"'("world" "sort" "hello" "a")"#,
    }
    tulisp_assert! {
        program: "(sort '(20 10 30 15 45) '<<)",
        error: r#"ERR TypeMismatch: Variable definition is void: <<
<eval_string>:1.26-1.28:  at <<
<eval_string>:1.1-1.29:  at (sort '(20 10 30 15 45) '<<)
"#
    }
    tulisp_assert! {
        program: "(sort '(20 10 30 15 45))",
        error: r#"ERR TypeMismatch: Too few arguments
<eval_string>:1.1-1.25:  at (sort '(20 10 30 15 45))
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

    ctx.add_special_form("d.run", move |_, _| Ok(d.run().into()));

    tulisp_assert! {
        ctx: ctx,
        program: "(d.run)",
        result: "5",
    }

    Ok(())
}

#[test]
fn test_from_iter() -> Result<(), Error> {
    let obj: TulispObject = (1..10).into_iter().map(|x| (x * 2).into()).collect();
    assert_eq!(obj.to_string(), "(2 4 6 8 10 12 14 16 18)");
    Ok(())
}

#[test]
fn test_typed_iter() -> Result<(), Error> {
    let mut ctx = TulispContext::new();

    ctx.add_special_form("add_ints", |_ctx, args| {
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
<eval_string>:1.1-1.14:  at (add_ints 20)
"#
    }
    Ok(())
}

#[test]
fn test_any() -> Result<(), Error> {
    struct TestStruct {
        value: i64,
    }
    impl Display for TestStruct {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "TestStruct({})", self.value)
        }
    }

    let mut ctx = TulispContext::new();

    ctx.add_special_form("make_any", |ctx, args| {
        destruct_eval_bind!(ctx, (inp) = args);
        let res: Rc<dyn TulispAny> = Rc::new(TestStruct {
            value: inp.try_into()?,
        });

        Ok(res.into())
    });

    ctx.add_special_form("get_int", |_ctx, args| {
        destruct_eval_bind!(_ctx, (inp) = args);
        let inp = inp.as_any()?;

        inp.downcast::<TestStruct>()
            .map(|vv| vv.value)
            .map_err(|_| {
                Error::new(
                    tulisp::ErrorKind::TypeMismatch,
                    "Not the any thing we wanted.".to_owned(),
                )
            })
            .map(TulispObject::from)
    });

    tulisp_assert! {
        ctx: ctx,
        program: "(get_int (make_any 22))",
        result: "22",
    }
    tulisp_assert! {
        ctx: ctx,
        program: "(get_int 55)",
        error: r#"ERR TypeMismatch: Expected Any(Rc<dyn Any>): 55
<eval_string>:1.10-1.12:  at 55
<eval_string>:1.1-1.13:  at (get_int 55)
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
tests/bad-load.lisp:1.9-1.10:  at nil
<eval_string>:1.1-1.29:  at (load "tests/bad-load.lisp")
"#
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
