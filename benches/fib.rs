#[path = "common/mod.rs"]
mod common;

use std::hint::black_box;
use tulisp::{Error, TulispContext};

const FIB_N: i64 = 25;

const FIB_INT_DEFUN: &str = "(defun fib (n)
  (if (<= n 2)
      1
      (+ (fib (- n 1))
         (fib (- n 2)))))";

const FIB_FLOAT_DEFUN: &str = "(defun fib (n)
  (if (<= n 2.0)
      1.0
      (+ (fib (- n 1.0))
         (fib (- n 2.0)))))";

fn main() -> Result<(), Error> {
    let mut results = Vec::new();

    results.push(common::run("fib_int_tw_full", || {
        let mut ctx = TulispContext::new();
        ctx.tw_eval_string(FIB_INT_DEFUN)?;
        let result: i64 = ctx
            .tw_eval_string(&format!("(fib {})", black_box(FIB_N)))?
            .try_into()?;
        Ok(black_box(result).to_string())
    })?);

    {
        let mut ctx = TulispContext::new();
        ctx.tw_eval_string(FIB_INT_DEFUN)?;
        let program = ctx.tw_eval_string(&format!("'(fib {})", FIB_N))?;
        results.push(common::run("fib_int_tw_call", || {
            let result: i64 = ctx.eval(&program)?.try_into()?;
            Ok(black_box(result).to_string())
        })?);
    }

    results.push(common::run("fib_int_vm_full", || {
        let mut ctx = TulispContext::new();
        ctx.eval_string(FIB_INT_DEFUN)?;
        let result: i64 = ctx
            .eval_string(&format!("(fib {})", black_box(FIB_N)))?
            .try_into()?;
        Ok(black_box(result).to_string())
    })?);

    {
        let mut ctx = TulispContext::new();
        ctx.eval_string(FIB_INT_DEFUN)?;
        let call = format!("(fib {})", FIB_N);
        results.push(common::run("fib_int_vm_call", || {
            let result: i64 = ctx.eval_string(&call)?.try_into()?;
            Ok(black_box(result).to_string())
        })?);
    }

    results.push(common::run("fib_float_tw_full", || {
        let mut ctx = TulispContext::new();
        ctx.tw_eval_string(FIB_FLOAT_DEFUN)?;
        let result: f64 = ctx
            .tw_eval_string(&format!("(fib {})", black_box(FIB_N)))?
            .try_into()?;
        Ok(black_box(result).to_string())
    })?);

    {
        let mut ctx = TulispContext::new();
        ctx.tw_eval_string(FIB_FLOAT_DEFUN)?;
        let program = ctx.tw_eval_string(&format!("'(fib {})", FIB_N))?;
        results.push(common::run("fib_float_tw_call", || {
            let result: f64 = ctx.eval(&program)?.try_into()?;
            Ok(black_box(result).to_string())
        })?);
    }

    results.push(common::run("fib_float_vm_full", || {
        let mut ctx = TulispContext::new();
        ctx.eval_string(FIB_FLOAT_DEFUN)?;
        let result: f64 = ctx
            .eval_string(&format!("(fib {})", black_box(FIB_N)))?
            .try_into()?;
        Ok(black_box(result).to_string())
    })?);

    {
        let mut ctx = TulispContext::new();
        ctx.eval_string(FIB_FLOAT_DEFUN)?;
        let call = format!("(fib {})", FIB_N);
        results.push(common::run("fib_float_vm_call", || {
            let result: f64 = ctx.eval_string(&call)?.try_into()?;
            Ok(black_box(result).to_string())
        })?);
    }

    if let Some(path) = std::env::args().nth(1) {
        common::save_results(&path, &results);
    }
    common::print_summary(&results);
    Ok(())
}
