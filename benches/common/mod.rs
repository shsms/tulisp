use std::time::{Duration, Instant};
use tulisp::Error;

pub const WARMUP_ITERS: u32 = 5;
pub const MIN_DURATION: Duration = Duration::from_secs(3);

pub struct BenchResult {
    pub label: &'static str,
    pub result: String,
    pub iters: u64,
    pub ns_per_iter: u128,
}

pub fn run(
    label: &'static str,
    mut f: impl FnMut() -> Result<String, Error>,
) -> Result<BenchResult, Error> {
    for _ in 0..WARMUP_ITERS {
        f()?;
    }

    let mut iters: u64 = 0;
    let mut last_result = String::new();
    let start = Instant::now();
    while start.elapsed() < MIN_DURATION {
        last_result = f()?;
        iters += 1;
    }
    let elapsed = start.elapsed();
    Ok(BenchResult {
        label,
        result: last_result,
        iters,
        ns_per_iter: elapsed.as_nanos() / iters as u128,
    })
}

/// Append machine-readable results to `path`. Format: one `label ns_per_iter` line per result.
/// Append so that multiple bench binaries invoked by `cargo bench -- <path>` all contribute.
pub fn save_results(path: &str, results: &[BenchResult]) {
    use std::io::Write;
    let mut file = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(path)
        .unwrap_or_else(|e| panic!("bench: failed to open {path}: {e}"));
    for r in results {
        writeln!(file, "{} {}", r.label, r.ns_per_iter)
            .unwrap_or_else(|e| eprintln!("bench: failed to write {path}: {e}"));
    }
}

const COL_NAME: usize = 20;
const COL_RESULT: usize = 12;
const COL_ITERS: usize = 12;
const COL_MS: usize = 12;
const SEPARATOR_WIDTH: usize = COL_NAME + COL_RESULT + COL_ITERS + COL_MS + 3;

fn print_header() {
    println!(
        "\n{:<COL_NAME$} {:>COL_RESULT$} {:>COL_ITERS$} {:>COL_MS$}",
        "benchmark", "result", "iters", "ms/iter"
    );
    println!("{}", "-".repeat(SEPARATOR_WIDTH));
}

fn print_row(r: &BenchResult) {
    println!(
        "{:<COL_NAME$} {:>COL_RESULT$} {:>COL_ITERS$} {:>COL_MS$.3}",
        r.label,
        r.result,
        r.iters,
        r.ns_per_iter as f64 / 1_000_000.0,
    );
}

pub fn print_summary(results: &[BenchResult]) {
    print_header();
    for r in results {
        print_row(r);
    }
}
