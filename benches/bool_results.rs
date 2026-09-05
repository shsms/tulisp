#[path = "common/mod.rs"]
mod common;

use std::hint::black_box;
use tulisp::{Error, TulispContext};

const N: i64 = 200_000;

// A loop that stores the value of `expr` on every iteration. A stored
// result is a boolean object built once per iteration.
fn stored(expr: &str) -> String {
    format!(
        "(let ((i 0) (flag nil))
  (while (< i {N})
    (setq flag {expr})
    (setq i (+ i 1)))
  flag)"
    )
}

fn bench(
    results: &mut Vec<common::BenchResult>,
    label: &'static str,
    src: &str,
) -> Result<(), Error> {
    let mut ctx = TulispContext::new();
    results.push(common::run(label, || {
        let result = ctx.eval_string(black_box(src))?;
        Ok(black_box(result).fmt_string())
    })?);
    Ok(())
}

fn main() -> Result<(), Error> {
    let mut results = Vec::new();
    // t on almost every iteration.
    bench(&mut results, "bool_store_gt", &stored("(> i 5)"))?;
    // A Rust `-> bool` builtin called for its value.
    bench(&mut results, "bool_store_predicate", &stored("(numberp i)"))?;

    // Conditions that still build a boolean object the jump only
    // reads and drops. A comparison in this position is meant to
    // compile to a fused jump; trace markers block that today.
    bench(
        &mut results,
        "bool_cond_not",
        &format!("(let ((i 0)) (while (not (>= i {N})) (setq i (+ i 1))) i)"),
    )?;
    bench(
        &mut results,
        "bool_cond_equal",
        &format!(
            "(let ((i 0) (hits 0))
  (while (< i {N})
    (if (equal i -1) (setq hits (+ hits 1)))
    (setq i (+ i 1)))
  hits)"
        ),
    )?;
    bench(
        &mut results,
        "bool_cond_chain",
        &format!("(let ((i 0)) (while (<= 0 i (- {N} 1)) (setq i (+ i 1))) i)"),
    )?;

    if let Some(path) = std::env::args().nth(1) {
        common::save_results(&path, &results);
    }
    common::print_summary(&results);
    Ok(())
}
