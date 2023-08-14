# _Tulisp_

[<img alt="docs.rs" src="https://img.shields.io/docsrs/tulisp">](https://docs.rs/tulisp/latest/tulisp/)
[<img alt="Crates.io" src="https://img.shields.io/crates/v/tulisp">](https://crates.io/crates/tulisp)

Tulisp is a Lisp interpreter that can be embedded into Rust programs.  The
syntax tries to closely match that of Emacs Lisp.  It was primarily designed to
be a configuration language, but it also works well as a general purpose
embedded scripting language.

One of the many benefits of using the Emacs Lisp syntax is that we can reuse its
documentation for the builtin functions and macros.  And for people who are
already familiar with Emacs Lisp, there's no need to learn an extra language.

## Getting started

Tulisp requires `rustc` version 1.70 or higher.

It is very easy to get started.  Here's an example:

  ```rust
  use tulisp::{TulispContext, tulisp_fn, Error};

  fn main() -> Result<(), Error> {
      // Create a new Tulisp execution context.
      let mut ctx = TulispContext::new();

      // Add a function called `add_nums` to `ctx`.
      #[tulisp_fn(add_func = "ctx")]
      fn add_nums(num1: i64, num2: i64) -> i64 {
          num1 + num2
      }

      // Write a lisp program that calls `add_nums`
      let program = "(add_nums 10 20)";

      // Evaluate the program, and save the result.
      let sum: i64 = ctx.eval_string(program)?.try_into()?;

      assert_eq!(sum, 30);
      Ok(())
  }
  ```

## Builtin functions

A list of currently available builtin functions can be found [here](https://docs.rs/tulisp/latest/tulisp/builtin).
