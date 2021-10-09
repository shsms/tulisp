#![warn(rust_2018_idioms)]

#[macro_use]
extern crate pest_derive;

mod cons;
mod context;
mod parser;
mod value;
mod eval;
mod builtin;

use builtin::make_context;
use eval::eval_string;

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    NotImplemented(String),
    ParsingError(String),
    TypeMismatch(String),
    Undefined(String),
    Uninitialized(String),
}

fn main() -> Result<(), Error> {
    // let string = r#"(+ 10 20 -50 (/ 4.0 -2))"#;
    // let string = "(let ((vv (+ 55 1)) (jj 20)) (+ vv jj 1))";
    // let string = "(defun test (a) (+ a 1)) (let ((vv 20)) (let ((zz (+ vv 10))) (test zz)))";
    // let string = "(defun inc-to-20 (a) (print (+ 10.0 a)) (if (< a 10) (inc-to-20 (+ a 1)) a)) (let* ((vv 2) (zz (+ vv 2))) (inc-to-20 zz))";
    // let string = "(let ((vv (+ 55 1)) (jj 20)) (setq vv (+ vv 10)) (+ vv jj 1))";
    // let string = "(let ((vv 0)) (while (< vv 10) (setq vv (+ 1 vv))) vv)";
    // let string = "(min 10 44 2 150 89)";

    let string = r##"
        (defun fibonacci (n)
          (cond
           ((<= n 1) 0)
           ((equal n 2) 1)
           (t (+ (fibonacci (- n 1))
                 (fibonacci (- n 2))))))

        (let ((n 1))
          (while (< n 10)
            (print (concat "Next:" (prin1-to-string n) ":" (prin1-to-string (fibonacci n))))
            (setq n (+ n 1))))
    "##;

    let mut ctx = make_context();
    eval_string(&mut ctx, string)?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::value::TulispValue;
    use crate::{eval_string, make_context, Error};

    #[test]
    fn test_if() -> Result<(), Error> {
        let mut ctx = make_context();
        let prog = "(if t 10 20)";
        assert_eq!(eval_string(&mut ctx, prog)?, TulispValue::Int(10));

        let prog = "(if nil 10 20)";
        assert_eq!(eval_string(&mut ctx, prog)?, TulispValue::Int(20));

        let prog = "(if (> 20 10) 10 20)";
        assert_eq!(eval_string(&mut ctx, prog)?, TulispValue::Int(10));

        let prog = "(if (> 10 20) 10 20)";
        assert_eq!(eval_string(&mut ctx, prog)?, TulispValue::Int(20));

        Ok(())
    }

    #[test]
    fn test_setq() -> Result<(), Error> {
        let mut ctx = make_context();
        let prog = r##"(let ((xx 10)) (setq zz (+ xx 10))) (* zz 3)"##;
        assert_eq!(eval_string(&mut ctx, prog)?, TulispValue::Int(60));

        Ok(())
    }

    #[test]
    fn test_while() -> Result<(), Error> {
        let mut ctx = make_context();
        let prog = "(let ((vv 0)) (while (< vv 42) (setq vv (+ 1 vv))) vv)";
        assert_eq!(eval_string(&mut ctx, prog)?, TulispValue::Int(42));

        Ok(())
    }
}
