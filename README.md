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

fn run() -> Result<(), Error> {
    let ctx = &mut TulispContext::new();
    ctx.defun("add-round", |a: f64, b: f64| -> i64 {
        (a + b).round() as i64
    });

    let result: i64 = ctx.eval_string("(add-round 10.2 20.0)")?.convert(ctx)?;
    assert_eq!(result, 30);
    Ok(())
}

fn main() {
    if let Err(e) = run() {
        println!("{e}");
        process::exit(-1);
    }
}
```

## Exposing Rust functions

[`TulispContext::defun`](https://docs.rs/tulisp/latest/tulisp/struct.TulispContext.html#method.defun)
handles argument evaluation, arity checking, and type conversion
automatically, for up to twelve parameters.  Built-in arg/return
types include `i64`, `f64`, `bool`, `String`,
[`Number`](https://docs.rs/tulisp/latest/tulisp/enum.Number.html),
`Vec<T>`, `Option<T>` and
[`TulispObject`](https://docs.rs/tulisp/latest/tulisp/struct.TulispObject.html).
An `Option<T>` parameter that no required parameter follows may be
left out of the call (right before a `Plist<T>` tail, pass `nil` when
keywords follow),
[`Rest<T>`](https://docs.rs/tulisp/latest/tulisp/struct.Rest.html)
takes the remaining arguments, a `Result<T, Error>` return type makes
the function fallible, a unit return is `nil`, and `&mut
TulispContext` as the first parameter gives the body the interpreter.

A Rust type crosses the boundary by implementing
[`TulispConvertible`](https://docs.rs/tulisp/latest/tulisp/trait.TulispConvertible.html).
For an opaque value that Lisp only passes around, a `Clone + Display`
type instead opts in with one empty `TulispAny` impl and gets the
conversion for free:

```rust
use tulisp::{TulispAny, TulispContext};

#[derive(Clone, Debug)]
struct Handle { id: u64 }
impl std::fmt::Display for Handle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#<handle {}>", self.id)
    }
}
impl TulispAny for Handle {}

let mut ctx = TulispContext::new();
ctx.defun("make-handle", |id: i64| Handle { id: id as u64 });
ctx.defun("handle-id", |h: Handle| -> i64 { h.id as i64 });
assert_eq!(ctx.eval_string("(handle-id (make-handle 7))").unwrap().to_string(), "7");
```

An enum whose variants are symbols is declared with
[`AsSymbol!`](https://docs.rs/tulisp/latest/tulisp/macro.AsSymbol.html):

```rust
use tulisp::{AsSymbol, TulispContext};

AsSymbol! {
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub enum Mode { Fast<"fast">, Careful<"careful"> }
}

let mut ctx = TulispContext::new();
ctx.defun("careful-p", |m: Mode| -> bool { m == Mode::Careful });
assert_eq!(ctx.eval_string("(careful-p 'careful)").unwrap().to_string(), "t");
```

The enum also gets `Display` and `FromStr` with the same spellings. A
`#[lisp(strings)]` marker, after the enum's doc comment and before its other
attributes, makes it read a string as well as a symbol, and write a string.

For arguments that are passed unevaluated, see
[`defspecial`](https://docs.rs/tulisp/latest/tulisp/struct.TulispContext.html#method.defspecial);
for code transformation, see
[`defmacro`](https://docs.rs/tulisp/latest/tulisp/struct.TulispContext.html#method.defmacro).

## Building and reading lists

[`list!`](https://docs.rs/tulisp/latest/tulisp/macro.list.html) builds a
list with a backquote-like syntax: `,x` adds an element and `,@x`
splices in the elements of a list, a `Vec`, a `Rest` or an array. With
`ctx =>`, any `TulispConvertible` value can be an element.
[`destructure`](https://docs.rs/tulisp/latest/tulisp/struct.TulispObject.html#method.destructure)
reads a list back into typed values, by the same rules `defun` applies
to its parameters.

```rust
use tulisp::{list, Rest, TulispContext, TulispObject};

let ctx = &mut TulispContext::new();
ctx.defmacro("my-when", |ctx, args| {
    let (cond, body): (TulispObject, Rest<TulispObject>) = args.destructure(ctx)?;
    list!(,ctx.intern("if") ,cond ,list!(,ctx.intern("progn") ,@body)?)
});
assert_eq!(ctx.eval_string("(my-when t 1 2)").unwrap().to_string(), "2");

let form = list!(ctx => ,ctx.intern("message") ,"sizes" ,vec![1, 2]).unwrap();
assert_eq!(form.to_string(), r#"(message "sizes" (1 2))"#);
```

## Keyed-list structs

A struct declared with
[`AsList!`](https://docs.rs/tulisp/latest/tulisp/macro.AsList.html)
reads from a plist or an alist and writes back the shape it names
(plist unless `#[lisp(alist)]` is given).  As a
[`Plist<T>`](https://docs.rs/tulisp/latest/tulisp/struct.Plist.html)
parameter it takes a function's remaining arguments as keyword/value
pairs; as a plain parameter it takes one list value; and it can be a
field of another `AsList!` struct.

```rust
use tulisp::{AsList, Plist, TulispContext};

AsList! {
    struct ServerConfig { host: String, port: i64 {= 8080}, tag: Option<String> }
}

let mut ctx = TulispContext::new();
ctx.defun("connect", |cfg: Plist<ServerConfig>| -> String {
    format!("{}:{}", cfg.host, cfg.port)
});
ctx.defun("connect-to", |cfg: ServerConfig| -> String {
    format!("{}:{}", cfg.host, cfg.port)
});
let addr = ctx.eval_string(r#"(connect :host "example.com" :port 443)"#).unwrap();
assert_eq!(addr.convert::<String>(&mut ctx).unwrap(), "example.com:443");
let addr = ctx.eval_string(r#"(connect-to '((host . "example.com")))"#).unwrap();
assert_eq!(addr.convert::<String>(&mut ctx).unwrap(), "example.com:8080");
```

`{= expr}` provides a default, `field<":custom-key">` overrides the
key, and an `Option<T>` field with no default is `None` when absent
or nil.  For a
list held in a free variable, the generated
[`Plistable`](https://docs.rs/tulisp/latest/tulisp/trait.Plistable.html)
and
[`Alistable`](https://docs.rs/tulisp/latest/tulisp/trait.Alistable.html)
impls expose `from_plist` / `from_alist` directly.

## Built-in Lisp features

Tulisp covers the standard Emacs Lisp shapes — control flow, bindings,
functions and macros, list / string / arithmetic / hash-table
operations, threading macros, backquote / unquote, error handling
(`error`, `signal`, `define-error`, `error-message-string`,
`user-error`, `catch`, `throw`, `condition-case`, `unwind-protect`),
tail-call optimisation, and lexical scoping.  See the
[`builtin`](https://docs.rs/tulisp/latest/tulisp/builtin) module for
the full list of forms and functions.

Errors are error symbols, as in Emacs: `define-error` adds one under a parent,
and a `condition-case` handler for a symbol catches every error defined under
it. `quit` is not under `error`, so a handler for `error` lets it through. From
Rust, `TulispContext::signal` and `TulispContext::define_error` raise and define
them, and `Error::is_a` tests which handler would catch an error. An error
raised with `signal`, even a built-in error caught and raised again, has the
kind `ErrorKind::Signal`, so use `is_a` rather than the kind to tell errors
apart. A `throw` with no `catch` for its tag running is a `no-catch` error, as
in Emacs.

A host can stop Lisp code that runs too long:
`TulispContext::set_interrupt_check` takes a closure that tulisp calls every so
often while Lisp code runs, and when it returns true the evaluation raises
`quit`. The closure can read a flag another thread sets, a pending signal or a
deadline. It should clear what made it return true. A closure that keeps
returning true also stops cleanups and handlers that run long, and later
evaluations when they reach the next check.

A handler for `quit` or `t` catches `quit`, so code that catches it and goes on
runs past every check. For a stop that no handler catches, the closure returns
`Interrupt::Stop` with a message instead of true. The evaluation then ends with
an `ErrorKind::Interrupted` error that has the message as its description, and
`unwind-protect` cleanups still run on the way out.

A macro defined in Lisp with `defmacro` compiles its body on its first
expansion. It keeps the compiled body, unless that expansion failed or
the body has a call to a name that had no value yet. The macros a kept
body uses stay as they were: redefining one later does not change the
macro that uses it, as with Emacs's byte-compiler. A macro whose body
uses that macro itself is refused at its first expansion.

## Editor support

Tulisp can tell an editor what a context defines and what is at a place in a
file, for completion, argument hints and hover. A language server builds on
these; nothing here evaluates code or interns a name.

[`TulispContext::describe`](https://docs.rs/tulisp/latest/tulisp/struct.TulispContext.html#method.describe)
says what a name holds, its signature and its docstring, and
[`symbols`](https://docs.rs/tulisp/latest/tulisp/struct.TulispContext.html#method.symbols)
lists every defined name. A Rust function shows its parameters' Lisp types,
unless its definition names them. A definition can carry a docstring too, as
[`FunctionName`](https://docs.rs/tulisp/latest/tulisp/trait.FunctionName.html)
describes:

```rust
use tulisp::TulispContext;

let mut ctx = TulispContext::new();
ctx.defun("connect", |host: String, port: Option<i64>| {
    format!("{host}:{}", port.unwrap_or(80))
});
let info = ctx.describe("connect").unwrap();
assert_eq!(info.signature.unwrap().render("connect"), "(connect STRING &optional INTEGER)");

ctx.defun(
    ("connect", ["host", "port"], "Connect to HOST."),
    |host: String, port: Option<i64>| format!("{host}:{}", port.unwrap_or(80)),
);
let info = ctx.describe("connect").unwrap();
assert_eq!(info.doc.as_deref(), Some("Connect to HOST."));
assert_eq!(info.signature.unwrap().render("connect"), "(connect HOST &optional PORT)");
```

[`syntax::read`](https://docs.rs/tulisp/latest/tulisp/syntax/fn.read.html) reads
source, finished or not, into a tree that keeps comments and the exact text of
each literal, and records what it cannot read instead of stopping. The
[`analysis`](https://docs.rs/tulisp/latest/tulisp/analysis/) functions take a
context, a tree and a byte offset:

```rust
use tulisp::{TulispContext, analysis, syntax};

let ctx = TulispContext::new();
let source = "(let ((total 1)) (car tot";
let tree = syntax::read(source);
let names: Vec<String> = analysis::completions(&ctx, &tree, source.len())
    .items
    .into_iter()
    .map(|item| item.name)
    .collect();
assert_eq!(names, ["total"]);

let hint = analysis::signature_help(&ctx, &tree, source.len()).unwrap();
assert_eq!(hint.name, "car");
assert_eq!(hint.active, Some(0));
```

## Cargo features

| Feature         | Description                                                                  |
|-----------------|------------------------------------------------------------------------------|
| `sync`          | Makes the interpreter thread-safe (`Arc`/`RwLock` instead of `Rc`/`RefCell`) |
| `etags`         | Enables TAGS file generation for Lisp source files                           |

`sync` is not additive: besides the pointer types, it makes `TulispAny` values
and registered closures need `Send + Sync`, and an interrupt check `Send`. Every
crate in a build shares one choice of it, so a library built on Tulisp should
leave the feature to the application that uses it.

## Next steps

- [`TulispContext`](https://docs.rs/tulisp/latest/tulisp/struct.TulispContext.html) — interpreter state, evaluation methods, and function registration
- [`TulispObject`](https://docs.rs/tulisp/latest/tulisp/struct.TulispObject.html) — the core Lisp value type
- [`TulispConvertible`](https://docs.rs/tulisp/latest/tulisp/trait.TulispConvertible.html) — how Rust types map to Lisp values
- [`builtin`](https://docs.rs/tulisp/latest/tulisp/builtin) — all built-in functions and macros

## Projects using Tulisp

- [slippy](https://github.com/shsms/slippy) — a configuration tool for the Sway window manager
- [microsim](https://github.com/shsms/microsim) — a microgrid simulator
