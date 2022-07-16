use tulisp::{tulisp_add_func, tulisp_fn, Error, Iter, TulispContext};

macro_rules! tulisp_assert {
    (@impl $ctx: expr, program:$input:expr, result:$result:expr $(,)?) => {
        let output = $ctx.eval_string($input).map_err(|err| {
            println!("{}:{}: execution failed: {}", file!(), line!(),err.to_string());
            err
        })?;
        let expected = $ctx.eval_string($result)?;
        assert!(
            output == expected,
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
        assert_eq!(output.unwrap_err().to_string(), $desc);
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
fn test_if() -> Result<(), Error> {
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
        error: "ERROR:TypeMismatch: Too many &rest parameters, in Some(Span { start: 18, end: 19 })",
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
        error: "ERROR:TypeMismatch: Too few arguments, in Some(Span { start: 26, end: 34 })",
    }
    tulisp_assert! {
        program: "(defun add (x y) (+ x y)) (add 10 20 30)",
        error: "ERROR:TypeMismatch: Too many arguments, in Some(Span { start: 37, end: 39 })",
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
        error: "ERROR:TypeMismatch: Too few arguments, in Some(Span { start: 68, end: 73 })",
    }
    tulisp_assert! {
        program: "(defmacro inc (var)  (list 'setq var (list '+ 1 var))) (let ((x 4)) (inc 4 5))",
        error: "ERROR:TypeMismatch: Too many arguments, in Some(Span { start: 75, end: 76 })",
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
    Ok(())
}

#[test]
fn test_eval() -> Result<(), Error> {
    tulisp_assert! {
        program: "(eval '(mod 32 5))",
        result: "2",
    }

    Ok(())
}
#[test]
fn test_strings() -> Result<(), Error> {
    tulisp_assert! {
        program: r##"(concat 'hello 'world)"##,
        error: "ERROR:TypeMismatch: Not a string: 'hello, in Some(Span { start: 0, end: 22 })"
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
        program: "(setq items (append items '(10)))",
        error: "ERROR:TypeMismatch: variable definition is void: items, in Some(Span { start: 20, end: 25 })",
    }

    tulisp_assert! {
        program: "(let ((vv '(12 20 30))) `(,(car vv) ,@(cdr vv) ,(cdr vv)))",
        result: "'(12 20 30 (20 30))",
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
        program: r##"
        (let ((vv '((name . "person") (age . 120))))
          (list (assoc 'age vv) (alist-get 'name vv) (alist-get 'names vv) (alist-get 'names vv "something")))
        "##,
        result: r##"'((age . 120) "person" nil "something")"##,
    }

    Ok(())
}

#[test]
fn test_math() -> Result<(), Error> {
    tulisp_assert! {
        program: "(/ 10 0)",
        error: "ERROR:Undefined: Division by zero, in Some(Span { start: 0, end: 8 })",
    }

    tulisp_assert! { program: "(/ 24 2 2)",                result: "6"     }
    tulisp_assert! { program: "(+ 40 (* 2.5 4) (- 4 12))", result: "42.0"  }
    tulisp_assert! { program: "(+ 40 (* 2.5 4) (- -1 7))", result: "42.0"  }
    tulisp_assert! { program: "(expt 2 3)",                result: "8.0"   }
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
        program: "(let ((vv (+ 55 1)) (jj 20)) (append kk (+ vv jj 1)))",
        error: "ERROR:TypeMismatch: variable definition is void: kk, in Some(Span { start: 37, end: 39 })",
    }
    tulisp_assert! {
        program: "(let ((22 (+ 55 1)) (jj 20)) (+ vv jj 1))",
        error: "ERROR:TypeMismatch: Expected symbol: 22, in Some(Span { start: 7, end: 9 })",
    }
    tulisp_assert! {
        program: "(let (18 (vv (+ 55 1)) (jj 20)) (+ vv jj 1))",
        error: "ERROR:SyntaxError: varitems inside a let-varlist should be a var or a binding: 18, in Some(Span { start: 5, end: 31 })",
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
fn test_setq() -> Result<(), Error> {
    tulisp_assert! {
        program: r##"(let ((xx 10)) (setq zz (+ xx 10))) (* zz 3)"##,
        result: "60",
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
        program: "(sort '(20 10 30 15 45) '<<)",
        error: "ERROR:Undefined: Unknown predicate: <<, in Some(Span { start: 0, end: 28 })",
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
fn test_tulisp_fn() -> Result<(), Error> {
    struct Demo {
        vv: i64,
    }
    impl Demo {
        #[tulisp_fn]
        fn run(&self) -> i64 {
            self.vv
        }
    }

    let d = Demo { vv: 5 };

    let mut ctx = TulispContext::new();

    tulisp_add_func!(ctx, d.run);

    tulisp_assert! {
        ctx: ctx,
        program: "(d.run)",
        result: "5",
    }

    Ok(())
}

#[test]
fn test_typed_iter() -> Result<(), Error> {
    let mut ctx = TulispContext::new();

    #[tulisp_fn(add_func = "ctx")]
    fn add_ints(ints: Option<Iter<i64>>) -> Result<i64, Error> {
        Ok(match ints {
            Some(ints) => {
                let mut sums = 0;
                for next in ints {
                    sums += next?;
                }
                sums
            }
            None => -1,
        })
    }

    #[tulisp_fn(add_func = "ctx")]
    fn add_ints_no_default(ints: Iter<i64>) -> Result<i64, Error> {
        Ok({
            let mut sums = 0;
            for next in ints {
                sums += next?;
            }
            sums
        })
    }

    tulisp_assert! {
        ctx: ctx,
        program: "(add_ints_no_default '(10 20 30))",
        result: "60",
    }
    tulisp_assert! {
        ctx: ctx,
        program: "(add_ints '(10 20 30))",
        result: "60",
    }
    tulisp_assert! {
        ctx: ctx,
        program: "(add_ints)",
        result: "-1",
    }
    tulisp_assert! {
        ctx: ctx,
        program: "(add_ints_no_default 20)",
        error: "ERROR:TypeMismatch: In call to \"add_ints_no_default\", arg \"ints\" needs to be a list, in Some(Span { start: 21, end: 23 })",
    }
    tulisp_assert! {
        ctx: ctx,
        program: "(add_ints 20)",
        error: "ERROR:TypeMismatch: In call to \"add_ints\", arg \"ints\" needs to be a list, in Some(Span { start: 10, end: 12 })",
    }
    Ok(())
}
