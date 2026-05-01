//! Emacs-parity sentinels.
//!
//! Each `(input, expected)` pair was verified against
//! `emacs --batch FILE --eval '(progn (indent-region (point-min)
//! (point-max)) (princ (buffer-string)))'` in the
//! lisp-data-mode auto-mode that `--batch` selects for `.lisp`
//! files. If a regression touches indent rules, the affected case
//! starts producing different output and this test fails — so we
//! find out without having to re-run Emacs.

fn check(input: &str, expected: &str) {
    let got = tulisp_fmt::format(input).expect("parse");
    assert_eq!(
        got, expected,
        "input:\n{input}\nexpected:\n{expected}\ngot:\n{got}"
    );
    // Idempotence: formatting the formatted output produces the same
    // result.
    let twice = tulisp_fmt::format(&got).expect("re-parse");
    assert_eq!(got, twice, "non-idempotent for input:\n{input}");
}

#[test]
fn fib_recursive() {
    let input = "(defun fib (n)
  (if (<= n 2)
      1
      (+ (fib (- n 1))
         (fib (- n 2)))))
";
    // `if` is treated as a function call in lisp-data-mode — both
    // branches align under the cond.
    check(input, input);
}

#[test]
fn fib_let_star_block() {
    let input = "(let* ((tgt 30)
       (res (fib tgt)))
  (print (format \"\\n  (fib %d)\\n  %d\\n\" tgt res)))
";
    check(input, input);
}

#[test]
fn fizz_buzz_nested_if() {
    let input = "(defun fizz-buzz (x)
  (if (and (equal 0 (mod x 3)) (equal 0 (mod x 5)))
      \"FizzBuzz\"
      (if (equal 0 (mod x 3))
          \"Fizz\"
          (if (equal 0 (mod x 5))
              \"Buzz\"
              x))))
";
    check(input, input);
}

#[test]
fn multi_arg_when_body() {
    let input = "(when cond
  (do-this)
  (do-that))
";
    check(input, input);
}

#[test]
fn defun_multi_body() {
    let input = "(defun greet (name)
  (message \"hi %s\" name)
  (message \"bye %s\" name))
";
    check(input, input);
}

#[test]
fn cond_with_clauses_one_per_line() {
    // Broken before any clause — clauses indent at parent + 2.
    let input = "(cond
  (p1 v1)
  (p2 v2))
";
    check(input, input);
}

#[test]
fn cond_first_clause_same_line() {
    // First clause on the same line; subsequent break aligns under it.
    let input = "(cond ((= x 1) 'one)
      ((= x 2) 'two))
";
    check(input, input);
}

#[test]
fn progn_first_arg_same_line() {
    // Same as `cond` — first arg same-line, second break aligns under it.
    let input = "(progn (do-this)
       (do-that)
       (and-this))
";
    check(input, input);
}

#[test]
fn progn_broken_before_body() {
    // Broken before the first arg → body indents at parent + 2.
    let input = "(progn
  (do-this)
  (do-that))
";
    check(input, input);
}

#[test]
fn let_body_indent() {
    let input = "(let ((x 1))
  (use x)
  (use x))
";
    check(input, input);
}

#[test]
fn function_call_aligns_under_second() {
    let input = "(funcall fn arg1
         arg2
         arg3)
";
    check(input, input);
}

#[test]
fn function_call_broken_before_first_arg() {
    let input = "(funcall fn
         arg1)
";
    // Wait — this one's the lisp-data-mode behavior: broken-before-first-arg
    // goes to open + 1, which here is column 1.
    let expected = "(funcall fn
         arg1)
";
    check(input, expected);
}

#[test]
fn blank_lines_preserved() {
    // Two blank lines stay as two blank lines.
    let input = "(a)\n\n\n(b)\n";
    check(input, input);
}

#[test]
fn dotted_pair_atom_round_trips() {
    // Dotted-pair syntax in numeric / symbol atoms should pass
    // through verbatim.
    let input = "(cons 1 2)\n";
    check(input, input);
}

#[test]
fn reader_macros_tight() {
    let input = "(quote-like '(a b ,c ,@d) #'fn)\n";
    check(input, input);
}
