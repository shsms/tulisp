#[path = "common/mod.rs"]
mod common;

use std::hint::black_box;
use tulisp::{Error, TulispContext};

const N: i64 = 200_000;

// Stores the value of `expr` on every iteration. `(if (< a b) ..)`
// compiles to a fused jump and never builds a boolean object; a stored
// result does, once per iteration.
fn bench(
    results: &mut Vec<common::BenchResult>,
    label: &'static str,
    expr: &str,
) -> Result<(), Error> {
    let src = format!(
        "(let ((i 0) (flag nil))
  (while (< i {N})
    (setq flag {expr})
    (setq i (+ i 1)))
  flag)"
    );
    let mut ctx = TulispContext::new();
    results.push(common::run(label, || {
        let result = ctx.eval_string(black_box(&src))?;
        Ok(black_box(result).fmt_string())
    })?);
    Ok(())
}

fn main() -> Result<(), Error> {
    let mut results = Vec::new();
    // t on almost every iteration.
    bench(&mut results, "bool_store_gt", "(> i 5)")?;
    // A Rust `-> bool` builtin called for its value.
    bench(&mut results, "bool_store_predicate", "(numberp i)")?;

    if let Some(path) = std::env::args().nth(1) {
        common::save_results(&path, &results);
    }
    common::print_summary(&results);
    Ok(())
}
