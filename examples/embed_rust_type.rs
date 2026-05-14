//! Embedding an opaque Rust type. The struct lives in Lisp as a
//! black-box value — Rust-defined functions construct it, query it,
//! and pass it around. Useful when the host owns complex state (a
//! database connection, a UI handle, a domain model) and only Rust
//! knows how to operate on it.
//!
//! Run with `cargo run --example embed_rust_type`.

use std::fmt;

use tulisp::{Error, Shared, TulispContext, TulispConvertible, TulispObject};

#[derive(Clone)]
struct Color {
    r: u8,
    g: u8,
    b: u8,
}

impl fmt::Display for Color {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<color r={} g={} b={}>", self.r, self.g, self.b)
    }
}

// The bridge: how Tulisp recognizes a `Color` argument and how a
// `Color` value gets wrapped to live inside a `TulispObject`.
impl TulispConvertible for Color {
    fn from_tulisp(value: &TulispObject) -> Result<Self, Error> {
        value
            .as_any()
            .ok()
            .and_then(|v| v.downcast_ref::<Color>().cloned())
            .ok_or_else(|| Error::type_mismatch(format!("Expected Color, got: {value}")))
    }
    fn into_tulisp(self) -> TulispObject {
        Shared::new(self).into()
    }
}

fn main() -> Result<(), Error> {
    let mut ctx = TulispContext::new();

    ctx.defun("make-color", |r: i64, g: i64, b: i64| Color {
        r: r as u8,
        g: g as u8,
        b: b as u8,
    });
    ctx.defun("color-r", |c: Color| -> i64 { c.r as i64 });
    ctx.defun("color-g", |c: Color| -> i64 { c.g as i64 });
    ctx.defun("color-b", |c: Color| -> i64 { c.b as i64 });
    ctx.defun("color-brightness", |c: Color| -> i64 {
        ((c.r as u32 + c.g as u32 + c.b as u32) / 3) as i64
    });

    // The opaque value flows through `let`, gets stashed in a
    // variable, and is retrieved later — Lisp doesn't peek inside.
    let program = r#"
        (let ((c (make-color 200 100 50)))
          (list (color-r c)
                (color-g c)
                (color-b c)
                (color-brightness c)))
    "#;
    let parts = ctx.eval_string(program)?;
    println!("(r g b brightness) => {parts}");

    // The Display impl above is what renders the opaque value when
    // Lisp prints it.
    let single = ctx.eval_string("(make-color 0 255 0)")?;
    println!("constructed => {single}");

    Ok(())
}
