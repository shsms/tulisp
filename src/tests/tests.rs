use crate::parser::parse_string;
use crate::{cons::*, eval::eval_string, new_context, Error};

macro_rules! tulisp_assert {
    (program:$input:expr, result:$result:expr $(,)?) => {
        let mut ctx = new_context();
        let output = eval_string(&mut ctx, $input)?;
        let expected = parse_string(&mut ctx, $result)?;
        let expected = car(&expected)?;
        assert_eq!(&output, expected);
    };
    (program:$input:expr, error:$desc:expr $(,)?) => {
        let mut ctx = new_context();
        let output = eval_string(&mut ctx, $input);
        assert!(output.is_err());
        assert_eq!(output.unwrap_err().desc, $desc);
    };
}

#[test]
fn test_if() -> Result<(), Error> {
    tulisp_assert! {
        program: "(if t 10 20)",
        result: "10",
    }
    tulisp_assert! {
        program: "(if nil 10 20)",
        result: "20",
    }

    tulisp_assert! {
        program: "(if (> 20 10) 10 20)",
        result: "10",
    }

    tulisp_assert! {
        program: "(if (> 10 20) 10 20)",
        result: "20",
    }

    Ok(())
}

#[test]
fn test_math() -> Result<(), Error> {
    tulisp_assert!{
        program: "(/ 10 0)",
        error: "Division by zero",
    }
    Ok(())
}
#[test]
fn test_setq() -> Result<(), Error> {
    tulisp_assert!{
        program: r##"(let ((xx 10)) (setq zz (+ xx 10))) (* zz 3)"##,
        result: "60",
    }

    Ok(())
}

#[test]
fn test_while() -> Result<(), Error> {
    tulisp_assert!{
        program: "(let ((vv 0)) (while (< vv 42) (setq vv (+ 1 vv))) vv)",
        result: "42",
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
        (if (equal (expt 9 0.5) 3)
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
        (if nil
            ()
          (equal 3 (expt 9 0.5)))
        "##,
    };

    Ok(())
}
