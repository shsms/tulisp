/*!
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
*/

/*!
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

A list of currently available builtin functions can be found [here](builtin).
*/

/*!
## Next steps

1. Values in _Tulisp_ are represented in rust as [`TulispObject`]s.  That struct
   implements methods for performing operations on Tulisp values.

1. [`TulispContext`] tracks the state of the interpreter and provides methods
   for executing _Tulisp_ programs.

1. [`#[tulisp_fn]`](tulisp_fn) and [`#[tulisp_fn_no_eval]`](tulisp_fn_no_eval)
   are flexible attribute macros for adding many different kinds of functions to
   a `TulispContext` object, so that they can be called from lisp code.
*/

pub(crate) mod bytecode;
mod eval;
mod macros;
mod parse;

pub use tulisp_proc_macros::{tulisp_add_func, tulisp_add_macro, tulisp_fn, tulisp_fn_no_eval};

pub mod builtin;

mod cons;
pub use cons::{BaseIter, Iter};

mod context;
pub use context::TulispContext;

mod error;
pub use error::{Error, ErrorKind};

pub mod lists;

mod value;
#[doc(hidden)]
pub use value::TulispValue;

mod object;
pub use object::TulispObject;
