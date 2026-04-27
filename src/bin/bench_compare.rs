use std::{collections::HashMap, fs, path::Path, process};

const DEFAULT_THRESHOLD_PCT: f64 = 10.0;
const COL_NAME: usize = 24;
const COL_MS: usize = 12;
const COL_CHANGE: usize = 10;
const SEPARATOR_WIDTH: usize = COL_NAME + COL_MS + COL_MS + COL_CHANGE + 3; // 3 spaces between cols

fn print_header() {
    println!(
        "\n{:<COL_NAME$} {:>COL_MS$} {:>COL_MS$} {:>COL_CHANGE$}",
        "benchmark", "baseline ms", "current ms", "change"
    );
    println!("{}", "-".repeat(SEPARATOR_WIDTH));
}

fn print_row(name: &str, baseline_ms: Option<f64>, current_ms: Option<f64>, change: &str) {
    let base = baseline_ms.map_or_else(
        || format!("{:>COL_MS$}", "N/A"),
        |v| format!("{:>COL_MS$.3}", v),
    );
    let cur = current_ms.map_or_else(
        || format!("{:>COL_MS$}", "N/A"),
        |v| format!("{:>COL_MS$.3}", v),
    );
    println!(
        "{:<COL_NAME$} {} {} {:>COL_CHANGE$}",
        name, base, cur, change
    );
}

fn parse_results(path: &Path) -> Result<Vec<(String, u128)>, String> {
    let content =
        fs::read_to_string(path).map_err(|e| format!("failed to read {}: {e}", path.display()))?;
    let mut results = Vec::new();
    for line in content.lines() {
        let mut parts = line.split_whitespace();
        let (Some(label), Some(ns_str), None) = (parts.next(), parts.next(), parts.next()) else {
            return Err(format!("unexpected line in {}: {line:?}", path.display()));
        };
        let ns: u128 = ns_str
            .parse()
            .map_err(|e| format!("bad ns value {ns_str:?} in {}: {e}", path.display()))?;
        results.push((label.to_string(), ns));
    }
    Ok(results)
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 3 {
        eprintln!("usage: bench_compare <baseline> <current> [threshold_pct]");
        process::exit(2);
    }

    let baseline_path = Path::new(&args[1]);
    let current_path = Path::new(&args[2]);
    let threshold: f64 = args
        .get(3)
        .and_then(|s| s.parse().ok())
        .unwrap_or(DEFAULT_THRESHOLD_PCT);

    if !baseline_path.exists() {
        println!("Baseline file not found — skipping comparison");
        return;
    }

    let baseline_vec = parse_results(baseline_path).unwrap_or_else(|e| {
        eprintln!("{e}");
        process::exit(2);
    });
    let baseline: HashMap<String, u128> = baseline_vec.into_iter().collect();
    let current = parse_results(current_path).unwrap_or_else(|e| {
        eprintln!("{e}");
        process::exit(2);
    });
    let current_names: HashMap<&str, u128> =
        current.iter().map(|(k, v)| (k.as_str(), *v)).collect();

    print_header();

    let mut regressions = 0usize;
    for (name, cur_ns) in &current {
        let cur_ms = *cur_ns as f64 / 1_000_000.0;
        if let Some(&base_ns) = baseline.get(name) {
            let base_ms = base_ns as f64 / 1_000_000.0;
            let pct = (cur_ms - base_ms) / base_ms * 100.0;
            let change = if pct > threshold {
                regressions += 1;
                format!("{:>+9.1}% !! REGRESSION", pct)
            } else {
                format!("{:>+9.1}%", pct)
            };
            print_row(name, Some(base_ms), Some(cur_ms), &change);
        } else {
            print_row(name, None, Some(cur_ms), "(new)");
        }
    }
    for (name, base_ns) in &baseline {
        if !current_names.contains_key(name.as_str()) {
            let base_ms = *base_ns as f64 / 1_000_000.0;
            print_row(name, Some(base_ms), None, "(removed)");
        }
    }

    println!();
    if regressions > 0 {
        println!("{regressions} regression(s) exceed the {threshold}% threshold — FAIL");
        process::exit(1);
    } else {
        println!("No regressions detected");
    }
}
