# _Tulisp_

[<img alt="docs.rs" src="https://img.shields.io/docsrs/tulisp">](https://docs.rs/tulisp/latest/tulisp/)
[<img alt="Crates.io" src="https://img.shields.io/crates/v/tulisp">](https://crates.io/crates/tulisp)

Tulisp is an embeddable Lisp interpreter for Rust with Emacs Lisp-compatible
syntax.  It is designed as a configuration and scripting layer for Rust
applications — zero external dependencies, low startup cost, and a clean API
for exposing Rust functions to Lisp code.

## Quick start

Requires Rust 1.88 or higher.

```rust
use std::process;
use tulisp::{TulispContext, Error};

fn run(ctx: &mut TulispContext) -> Result<(), Error> {
    ctx.defun("add-round", |a: f64, b: f64| -> i64 {
        (a + b).round() as i64
    });

    let result: i64 = ctx.eval_string("(add-round 10.2 20.0)")?.try_into()?;
    assert_eq!(result, 30);
    Ok(())
}

fn main() {
    let mut ctx = TulispContext::new();
    if let Err(e) = run(&mut ctx) {
        println!("{}", e.format(&ctx));
        process::exit(-1);
    }
}
```

## Exposing Rust functions

[`TulispContext::defun`](https://docs.rs/tulisp/latest/tulisp/struct.TulispContext.html#method.defun)
handles argument evaluation, arity checking, and type conversion
automatically.  Built-in arg/return types include `i64`, `f64`, `bool`,
`String`,
[`Number`](https://docs.rs/tulisp/latest/tulisp/enum.Number.html),
`Vec<T>`, and
[`TulispObject`](https://docs.rs/tulisp/latest/tulisp/struct.TulispObject.html).
Use `Option<T>` for `&optional` parameters,
[`Rest<T>`](https://docs.rs/tulisp/latest/tulisp/struct.Rest.html) for
`&rest`, a `Result<T, Error>` return type for fallible functions, and
`&mut TulispContext` as the first parameter to access the interpreter
from the function body.  Custom Rust types become passable by
implementing
[`TulispConvertible`](https://docs.rs/tulisp/latest/tulisp/trait.TulispConvertible.html)
— most commonly via opaque `Shared<dyn TulispAny>` storage for
arbitrary `Clone + Display` values.

For raw argument lists and code transformation, see
[`defspecial`](https://docs.rs/tulisp/latest/tulisp/struct.TulispContext.html#method.defspecial)
and
[`defmacro`](https://docs.rs/tulisp/latest/tulisp/struct.TulispContext.html#method.defmacro).

## Keyword-argument and alist-shaped structs

When a function accepts many keyword-style parameters, derive
[`Plistable`](https://docs.rs/tulisp/latest/tulisp/trait.Plistable.html)
on a struct via
[`AsPlist!`](https://docs.rs/tulisp/latest/tulisp/macro.AsPlist.html)
and use
[`Plist<T>`](https://docs.rs/tulisp/latest/tulisp/struct.Plist.html) as
the parameter type:

```rust
use tulisp::{TulispContext, Plist, AsPlist};

AsPlist! {
    struct ServerConfig { host: String, port: i64 {= 8080} }
}

let mut ctx = TulispContext::new();
ctx.defun("connect", |cfg: Plist<ServerConfig>| -> String {
    format!("{}:{}", cfg.host, cfg.port)
});
// (connect :host "example.com" :port 443)  =>  "example.com:443"
```

`{= expr}` provides a default, `field<":custom-key">` overrides the
keyword name, and `Option<T>` fields read explicit `nil` as `None`.

[`AsAlist!`](https://docs.rs/tulisp/latest/tulisp/macro.AsAlist.html) /
[`Alistable`](https://docs.rs/tulisp/latest/tulisp/trait.Alistable.html)
mirror the design for alist-shaped values that arrive as a single
argument (a list of dotted pairs).  For converting a plist or alist
held in a free variable rather than from a defun call, both traits
also expose `from_plist` / `from_alist` standalone.

## Built-in Lisp features

Tulisp covers the standard Emacs Lisp shapes — control flow, bindings,
functions and macros, list / string / arithmetic / hash-table
operations, threading macros, backquote / unquote, error handling
(`error`, `catch`, `throw`, `condition-case`), tail-call optimisation,
and lexical scoping.  See the
[`builtin`](https://docs.rs/tulisp/latest/tulisp/builtin) module for
the full list of forms and functions.

## Cargo features

| Feature         | Description                                                                  |
|-----------------|------------------------------------------------------------------------------|
| `sync`          | Makes the interpreter thread-safe (`Arc`/`RwLock` instead of `Rc`/`RefCell`) |
| `big_functions` | Increases the maximum number of defun parameters from 5 to 10                |
| `etags`         | Enables TAGS file generation for Lisp source files                           |

## Next steps

- [`TulispContext`](https://docs.rs/tulisp/latest/tulisp/struct.TulispContext.html) — interpreter state, evaluation methods, and function registration
- [`TulispObject`](https://docs.rs/tulisp/latest/tulisp/struct.TulispObject.html) — the core Lisp value type
- [`TulispConvertible`](https://docs.rs/tulisp/latest/tulisp/trait.TulispConvertible.html) — how Rust types map to Lisp values
- [`builtin`](https://docs.rs/tulisp/latest/tulisp/builtin) — all built-in functions and macros

## Projects using Tulisp

- [slippy](https://github.com/shsms/slippy) — a configuration tool for the Sway window manager
- [microsim](https://github.com/shsms/microsim) — a microgrid simulator
