//! Tiny unified-diff renderer for `tulisp-fmt --diff`.
//!
//! Two-input, line-level only. The algorithm is the textbook
//! longest-common-subsequence DP (O(n·m) time and space) — fine for
//! source files where n and m are at most a few hundred lines.
//!
//! The output is `diff -U3`-compatible: a `--- a/<path>` /
//! `+++ b/<path>` header followed by `@@` hunks. Anything more
//! exotic (word-level highlighting, color, --git extensions) is left
//! to the user's pager of choice.

const CONTEXT: usize = 3;

/// Render a unified diff between `old` and `new`. Returns an empty
/// string when the inputs are identical so callers can use it
/// directly as a "did anything change?" signal.
pub fn unified_diff(label: &str, old: &str, new: &str) -> String {
    if old == new {
        return String::new();
    }
    let old_lines = split_lines(old);
    let new_lines = split_lines(new);
    let ops = diff_ops(&old_lines, &new_lines);

    let mut out = String::new();
    out.push_str(&format!("--- a/{label}\n"));
    out.push_str(&format!("+++ b/{label}\n"));
    write_hunks(&mut out, &ops);
    out
}

fn split_lines(s: &str) -> Vec<&str> {
    if s.is_empty() {
        return Vec::new();
    }
    let mut v: Vec<&str> = s.split_inclusive('\n').collect();
    // Mark a missing trailing newline so we render `\ No newline at
    // end of file` exactly like `diff -u`.
    if !s.ends_with('\n') {
        // The final element already lacks `\n`; nothing to do — the
        // hunk emitter reads the line and detects the absence.
    }
    // Strip empty tail produced when string ends with "\n\n" — split_inclusive
    // does NOT produce a trailing empty segment, so this is always safe.
    if v.last().is_some_and(|s| s.is_empty()) {
        v.pop();
    }
    v
}

#[derive(Clone, Copy)]
enum Op<'a> {
    Same(&'a str),
    Del(&'a str),
    Ins(&'a str),
}

fn diff_ops<'a>(a: &[&'a str], b: &[&'a str]) -> Vec<Op<'a>> {
    let n = a.len();
    let m = b.len();
    let mut t = vec![0usize; (n + 1) * (m + 1)];
    let idx = |i: usize, j: usize| i * (m + 1) + j;
    for i in 0..n {
        for j in 0..m {
            t[idx(i + 1, j + 1)] = if a[i] == b[j] {
                t[idx(i, j)] + 1
            } else {
                t[idx(i + 1, j)].max(t[idx(i, j + 1)])
            };
        }
    }
    let mut ops = Vec::with_capacity(n + m);
    let (mut i, mut j) = (n, m);
    while i > 0 || j > 0 {
        if i > 0 && j > 0 && a[i - 1] == b[j - 1] {
            ops.push(Op::Same(a[i - 1]));
            i -= 1;
            j -= 1;
        } else if j > 0 && (i == 0 || t[idx(i, j - 1)] >= t[idx(i - 1, j)]) {
            ops.push(Op::Ins(b[j - 1]));
            j -= 1;
        } else {
            ops.push(Op::Del(a[i - 1]));
            i -= 1;
        }
    }
    ops.reverse();
    ops
}

fn write_hunks(out: &mut String, ops: &[Op]) {
    // Track input cursors as we walk the op list. `oi`/`ni` are
    // 1-based "next line" counters for the unified-diff header.
    let (mut oi, mut ni) = (1usize, 1usize);
    let mut k = 0usize;

    while k < ops.len() {
        // Find the next change op.
        let next_change = match ops[k..].iter().position(|o| !matches!(o, Op::Same(_))) {
            Some(p) => k + p,
            None => break,
        };
        // Skip over leading Sames, but rewind by CONTEXT lines.
        let leading_skip = next_change - k;
        if leading_skip > CONTEXT {
            let to_skip = leading_skip - CONTEXT;
            for _ in 0..to_skip {
                if let Op::Same(_) = ops[k] {
                    oi += 1;
                    ni += 1;
                }
                k += 1;
            }
        }

        // Hunk start positions.
        let hunk_old_start = oi;
        let hunk_new_start = ni;
        let mut buf: Vec<&Op> = Vec::new();

        // Greedily extend through changes; merge runs separated by
        // ≤ 2*CONTEXT same lines.
        let mut same_run = 0usize;
        while k < ops.len() {
            match ops[k] {
                Op::Same(_) => same_run += 1,
                _ => same_run = 0,
            }
            buf.push(&ops[k]);
            // Advance counters.
            match ops[k] {
                Op::Same(_) => {
                    oi += 1;
                    ni += 1;
                }
                Op::Del(_) => oi += 1,
                Op::Ins(_) => ni += 1,
            }
            k += 1;

            // If we just hit > 2*CONTEXT same lines, the hunk ends
            // CONTEXT lines back; the last CONTEXT same lines start
            // the next hunk's leading context.
            if same_run > 2 * CONTEXT {
                let to_keep = same_run - CONTEXT;
                for _ in 0..to_keep {
                    let last = buf.pop();
                    match last {
                        Some(Op::Same(_)) => {
                            oi -= 1;
                            ni -= 1;
                            k -= 1;
                        }
                        _ => unreachable!(),
                    }
                }
                break;
            }
        }

        // Trim trailing Sames to at most CONTEXT.
        let trailing_sames = buf
            .iter()
            .rev()
            .take_while(|o| matches!(o, Op::Same(_)))
            .count();
        if trailing_sames > CONTEXT {
            let to_drop = trailing_sames - CONTEXT;
            for _ in 0..to_drop {
                let last = buf.pop();
                match last {
                    Some(Op::Same(_)) => {
                        oi -= 1;
                        ni -= 1;
                        k -= 1;
                    }
                    _ => unreachable!(),
                }
            }
        }

        let (mut old_len, mut new_len) = (0usize, 0usize);
        for op in &buf {
            match **op {
                Op::Same(_) => {
                    old_len += 1;
                    new_len += 1;
                }
                Op::Del(_) => old_len += 1,
                Op::Ins(_) => new_len += 1,
            }
        }

        out.push_str(&format!(
            "@@ -{},{} +{},{} @@\n",
            hunk_old_start, old_len, hunk_new_start, new_len,
        ));
        for op in &buf {
            match **op {
                Op::Same(s) => emit_line(out, ' ', s),
                Op::Del(s) => emit_line(out, '-', s),
                Op::Ins(s) => emit_line(out, '+', s),
            }
        }
    }
}

fn emit_line(out: &mut String, sigil: char, line: &str) {
    out.push(sigil);
    out.push_str(line);
    if !line.ends_with('\n') {
        out.push('\n');
        out.push_str("\\ No newline at end of file\n");
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn identical_inputs_produce_empty_output() {
        assert_eq!(unified_diff("a", "x\ny\n", "x\ny\n"), "");
    }

    #[test]
    fn single_line_change() {
        let d = unified_diff("f.lisp", "a\nb\nc\n", "a\nB\nc\n");
        let expected = "\
--- a/f.lisp
+++ b/f.lisp
@@ -1,3 +1,3 @@
 a
-b
+B
 c
";
        assert_eq!(d, expected);
    }

    #[test]
    fn missing_trailing_newline_is_marked() {
        let d = unified_diff("f.lisp", "a", "b");
        assert!(
            d.contains("\\ No newline at end of file"),
            "got:\n{d}"
        );
    }

    #[test]
    fn distant_changes_split_into_two_hunks() {
        let old = (0..10)
            .map(|i| format!("line{i}\n"))
            .collect::<String>();
        let new = old.replace("line0\n", "L0\n").replace("line9\n", "L9\n");
        let d = unified_diff("f", &old, &new);
        let hunk_count = d.matches("@@ ").count();
        assert_eq!(hunk_count, 2, "two distant changes should be two hunks:\n{d}");
    }
}
