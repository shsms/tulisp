//! Defuns can take a typed Lisp plist directly. `AsPlist!` derives
//! the conversion, optional fields get defaults, and the call site
//! reads as Emacs Lisp keyword arguments.
//!
//! Run with `cargo run --example plist_args`.

use tulisp::{AsPlist, Error, Plist, TulispContext};

AsPlist! {
    struct ServerConfig {
        host: String,
        // Override the keyword spelling — Lisp side uses `:port-number`.
        port<":port-number">: i64 {= 8080},
        // Optional field. Absent → None; present and nil → None;
        // present with a string → Some(...).
        scheme: Option<String> {= None},
    }
}

fn main() -> Result<(), Error> {
    let mut ctx = TulispContext::new();

    ctx.defun("make-server", |cfg: Plist<ServerConfig>| -> String {
        let scheme = cfg.scheme.clone().unwrap_or_else(|| "http".to_string());
        format!("{scheme}://{}:{}", cfg.host, cfg.port)
    });

    for program in [
        r#"(make-server :host "localhost")"#,
        r#"(make-server :host "example.com" :port-number 443 :scheme "https")"#,
        r#"(make-server :host "api.local" :port-number 3000)"#,
    ] {
        let url = ctx.eval_string(program)?.as_string()?;
        println!("{program}\n  => {url}");
    }

    Ok(())
}
