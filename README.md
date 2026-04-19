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
is the primary way to register Rust functions.
Argument evaluation, arity checking, and type conversion are handled
automatically.

```rust
use tulisp::{TulispContext, Rest};

let mut ctx = TulispContext::new();

// Fixed arguments
ctx.defun("add", |a: i64, b: i64| a + b);

// Optional arguments (Lisp &optional)
ctx.defun("greet", |name: String, greeting: Option<String>| {
    format!("{}, {}!", greeting.unwrap_or("Hello".into()), name)
});

// Variadic arguments (Lisp &rest)
ctx.defun("sum", |items: Rest<f64>| -> f64 { items.into_iter().sum() });

// Fallible function
ctx.defun("safe-div", |a: i64, b: i64| -> Result<i64, tulisp::Error> {
    if b == 0 {
        Err(tulisp::Error::invalid_argument("Division by zero"))
    } else {
        Ok(a / b)
    }
});
```

Supported argument and return types include `i64`, `f64`, `bool`, `String`,
[`Number`](https://docs.rs/tulisp/latest/tulisp/enum.Number.html),
`Vec<T>`, and [`TulispObject`](https://docs.rs/tulisp/latest/tulisp/struct.TulispObject.html).
Add `&mut TulispContext` as the first parameter to access the interpreter.
Any type can be made passable by implementing
[`TulispConvertible`](https://docs.rs/tulisp/latest/tulisp/trait.TulispConvertible.html).

For advanced use cases — custom evaluation order, implementing control flow —
use [`defspecial`](https://docs.rs/tulisp/latest/tulisp/struct.TulispContext.html#method.defspecial)
(raw argument list) or
[`defmacro`](https://docs.rs/tulisp/latest/tulisp/struct.TulispContext.html#method.defmacro)
(code transformation before evaluation).

## Keyword-argument functions with `AsPlist!`

When a function accepts many optional parameters, use a plist as the argument
list.  The [`AsPlist!`](https://docs.rs/tulisp/latest/tulisp/macro.AsPlist.html)
macro derives the required
[`Plistable`](https://docs.rs/tulisp/latest/tulisp/trait.Plistable.html) trait
for a struct, and
[`Plist<T>`](https://docs.rs/tulisp/latest/tulisp/struct.Plist.html) as a
parameter type wires it up automatically.

```rust
use tulisp::{TulispContext, Plist, AsPlist};

AsPlist! {
    struct ServerConfig {
        host: String,
        port: i64 {= 8080},
    }
}

let mut ctx = TulispContext::new();
ctx.defun("connect", |cfg: Plist<ServerConfig>| -> String {
    format!("{}:{}", cfg.host, cfg.port)
});
// (connect :host "localhost")          => "localhost:8080"
// (connect :host "example.com" :port 443)  => "example.com:443"
```

## Opaque Rust values

Any `Clone + Display` type can be stored in a
[`TulispObject`](https://docs.rs/tulisp/latest/tulisp/struct.TulispObject.html)
and passed between Rust and Lisp transparently via
[`Shared::new`](https://docs.rs/tulisp/latest/tulisp/struct.Shared.html) and
[`TulispObject::as_any`](https://docs.rs/tulisp/latest/tulisp/struct.TulispObject.html#method.as_any).

```rust
use std::fmt;
use tulisp::{TulispContext, TulispConvertible, TulispObject, Shared, Error};

#[derive(Clone)]
struct Point { x: i64, y: i64 }

impl fmt::Display for Point {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(Point {} {})", self.x, self.y)
    }
}

impl TulispConvertible for Point {
    fn from_tulisp(value: &TulispObject) -> Result<Self, Error> {
        value.as_any().ok()
            .and_then(|v| v.downcast_ref::<Point>().cloned())
            .ok_or_else(|| Error::type_mismatch("Expected Point"))
    }
    fn into_tulisp(self) -> TulispObject { Shared::new(self).into() }
}

let mut ctx = TulispContext::new();
ctx.defun("make-point", |x: i64, y: i64| Point { x, y });
ctx.defun("point-x", |p: Point| p.x);
// (point-x (make-point 3 4)) => 3
```

## Built-in Lisp features

- **Control flow**: `if`, `cond`, `when`, `unless`, `while`, `progn`
- **Binding**: `let`, `let*`, `setq`, `set`
- **Functions and macros**: `defun`, `defmacro`, `lambda`, `funcall`, `eval`,
  `macroexpand`
- **Lists**: `cons`, `list`, `append`, `nth`, `nthcdr`, `last`, `length`,
  `mapcar`, `dolist`, `dotimes`
- **Alists and plists**: `assoc`, `alist-get`, `plist-get`
- **Strings**: `concat`, `format`, `prin1-to-string`, `princ`, `print`
- **Arithmetic**: `+`, `-`, `*`, `/`, `mod`, `1+`, `1-`, `abs`, `max`, `min`,
  `sqrt`, `expt`, `fround`, `ftruncate`
- **Comparison**: `=`, `/=`, `<`, `<=`, `>`, `>=` (numbers); `string<`,
  `string>`, `string=` (strings); `eq`, `equal`
- **Logic**: `and`, `or`, `not`, `xor`
- **Conditionals**: `if-let`, `if-let*`, `when-let`, `while-let`
- **Symbols**: `intern`, `make-symbol`, `gensym`
- **Hash tables**: `make-hash-table`, `gethash`, `puthash`
- **Error handling**: `error`, `catch`, `throw`
- **Threading macros**: `->` / `thread-first`, `->>` / `thread-last`
- **Time**: `current-time`, `time-add`, `time-subtract`, `time-less-p`,
  `time-equal-p`, `format-seconds`
- **Quoting**: `'`, `` ` ``, `,`, `,@` (backquote/unquote/splice)
- **Tail-call optimisation** (TCO) for recursive functions
- **Lexical scoping** and lexical binding

A full reference is available in the [`builtin`](https://docs.rs/tulisp/latest/tulisp/builtin) module docs.

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
