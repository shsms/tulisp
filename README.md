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

Tulisp requires `rustc` version 1.88 or higher.

It is very easy to get started.  Here's an example:

  ```rust
  use tulisp::{TulispContext, TulispObject, Error, destruct_eval_bind};

  fn main() -> Result<(), Error> {
      // Create a new Tulisp execution context.
      let mut ctx = TulispContext::new();

      // Add a function called `add_nums` to `ctx`.
      ctx.add_function("add_round", |a: f64, b: f64| -> i64 {
          (a + b).round() as i64
      });

      // Write a lisp program that calls `add_nums`
      let program = "(add_round 10.2 20.0)";

      // Evaluate the program, and save the result.
      let sum: i64 = ctx.eval_string(program)?.try_into()?;

      assert_eq!(sum, 30);
      Ok(())
  }
  ```

## Features

- `defun`s, `defmacro`s and `lambda`s
- `intern` to find/create symbols dynamically
- Backquote/Unquote forms (for example `` `(answer . ,(+ 2 3)) ``)
- Threading macros (`thread-first` and `thread-last`)
- Methods for reading from alists and plists
- Lexical scopes and lexical binding
- Tailcall Optimization
- Proc macros for exposing rust functions to tulisp
- Decl macros for
  [creating lists](https://docs.rs/tulisp/latest/tulisp/macro.list.html)
  and
  [destructuring lists](https://docs.rs/tulisp/latest/tulisp/macro.destruct_bind.html)
  quickly.
- Easy to use [interpreter](https://docs.rs/tulisp/latest/tulisp/struct.TulispContext.html) and [object](https://docs.rs/tulisp/latest/tulisp/struct.TulispObject.html)s
- Backtraces for errors

## Performance

Tulisp has a light-weight tree-walking interpreter with very low startup times and sufficient speed for many config/simulation needs.

## Builtin functions

A list of currently available builtin functions can be found [here](https://docs.rs/tulisp/latest/tulisp/builtin).

## Projects using Tulisp

- [slippy](https://github.com/shsms/slippy) - a configuration tool for the Sway window manager.
- [microsim](https://github.com/shsms/microsim) - a microgrid simulator.
