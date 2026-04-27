#[path = "common/mod.rs"]
mod common;

use std::hint::black_box;
use tulisp::{Error, TulispContext};

const N: usize = 500;

fn main() -> Result<(), Error> {
    let mut results = Vec::new();

    // Parsing a long flat list — exercises `parse_list`.
    {
        let mut src = String::with_capacity(N * 4 + 2);
        src.push('\'');
        src.push('(');
        for i in 0..N {
            if i > 0 {
                src.push(' ');
            }
            src.push_str(&i.to_string());
        }
        src.push(')');
        let mut ctx = TulispContext::new();
        results.push(common::run("parse_long_list", || {
            let result = ctx.eval_string(black_box(&src))?;
            Ok(black_box(result.fmt_string().len()).to_string())
        })?);
    }

    // Backquote with a long literal list — exercises `eval_back_quote`.
    {
        let mut src = String::with_capacity(N * 4 + 2);
        src.push('`');
        src.push('(');
        for i in 0..N {
            if i > 0 {
                src.push(' ');
            }
            src.push_str(&i.to_string());
        }
        src.push(')');
        let mut ctx = TulispContext::new();
        results.push(common::run("backquote_long_list", || {
            let result = ctx.eval_string(black_box(&src))?;
            Ok(black_box(result.fmt_string().len()).to_string())
        })?);
    }

    // TW `(list ...)` with many args — exercises `eval_each`.
    {
        let mut src = String::with_capacity(N * 4 + 7);
        src.push_str("(list");
        for i in 0..N {
            src.push(' ');
            src.push_str(&i.to_string());
        }
        src.push(')');
        let mut ctx = TulispContext::new();
        results.push(common::run("tw_list_call", || {
            let result = ctx.eval_string(black_box(&src))?;
            Ok(black_box(result.fmt_string().len()).to_string())
        })?);
    }

    // Defun whose body is a long flat literal — exercises
    // `substitute_lexical_inner` (per top-level form during
    // defun preprocessing).
    {
        let mut src = String::with_capacity(N * 4 + 64);
        src.push_str("(defun bench-fn () '(");
        for i in 0..N {
            if i > 0 {
                src.push(' ');
            }
            src.push_str(&i.to_string());
        }
        src.push_str("))");
        results.push(common::run("defun_long_body", || {
            let mut ctx = TulispContext::new();
            ctx.eval_string(black_box(&src))?;
            Ok(black_box("done").to_string())
        })?);
    }

    // `mapcar` over a long sequence — exercises
    // `eval_function_args`'s `&rest` builder (the `list` literal
    // is splatted into a Rest argument) and `TulispContext::map`.
    {
        let mut src = String::with_capacity(N * 4 + 32);
        src.push_str("(mapcar (lambda (x) x) (list");
        for i in 0..N {
            src.push(' ');
            src.push_str(&i.to_string());
        }
        src.push_str("))");
        let mut ctx = TulispContext::new();
        results.push(common::run("mapcar_long", || {
            let result = ctx.eval_string(black_box(&src))?;
            Ok(black_box(result.fmt_string().len()).to_string())
        })?);
    }

    // Parsing a file with many top-level forms — exercises
    // `Parser::parse`'s outer loop.
    {
        let mut src = String::with_capacity(N * 4);
        for i in 0..N {
            if i > 0 {
                src.push('\n');
            }
            src.push_str(&i.to_string());
        }
        let mut ctx = TulispContext::new();
        results.push(common::run("parse_many_forms", || {
            let result = ctx.eval_string(black_box(&src))?;
            Ok(black_box(result.fmt_string().len()).to_string())
        })?);
    }

    // Macroexpand a deeply-nested literal — exercises
    // `macroexpand`'s list rebuild.
    {
        let mut src = String::with_capacity(N * 4 + 32);
        src.push_str("(quote (");
        for i in 0..N {
            if i > 0 {
                src.push(' ');
            }
            src.push_str(&i.to_string());
        }
        src.push_str("))");
        let mut ctx = TulispContext::new();
        results.push(common::run("macroexpand_long", || {
            let result = ctx.eval_string(black_box(&src))?;
            Ok(black_box(result.fmt_string().len()).to_string())
        })?);
    }

    // `deep-copy` a long list. Tulisp doesn't expose `deep-copy`
    // directly, but `(append nil long-list)` exercises the same
    // `Cons::append`-via-deep-copy path that ListBuilder now
    // accelerates.
    {
        let mut src = String::with_capacity(N * 4 + 32);
        src.push_str("(append nil '(");
        for i in 0..N {
            if i > 0 {
                src.push(' ');
            }
            src.push_str(&i.to_string());
        }
        src.push_str("))");
        let mut ctx = TulispContext::new();
        results.push(common::run("append_deep_copy", || {
            let result = ctx.eval_string(black_box(&src))?;
            Ok(black_box(result.fmt_string().len()).to_string())
        })?);
    }

    if let Some(path) = std::env::args().nth(1) {
        common::save_results(&path, &results);
    }
    common::print_summary(&results);
    Ok(())
}
