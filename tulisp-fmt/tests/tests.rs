//! Big-input formatting test.
//!
//! A single-line source covering every parser feature
//! (numbers in all bases, scientific notation, character literals,
//! strings with escapes, keywords, dotted pairs, all reader macros,
//! and the special-form / function-call indent shapes) is fed
//! through the formatter and compared against the canonical
//! pretty-printed output.
//!
//! Idempotence is asserted alongside the byte-for-byte match —
//! `format(format(input)) == format(input)`.

const BIG_INPUT: &str = r##"(defun fib (n &optional verbose) (if (<= n 2) 1 (+ (fib (- n 1)) (fib (- n 2))))) (defmacro inc (x) `(setq ,x (1+ ,x))) (defvar tally 0) (defconst pi 3.14) (setq tally (let* ((kw :tag) (xs '(1 2 3 . 4)) (h (make-hash-table)) (c ?\n) (q ?\\) (hex #x1A) (oct #o17) (bin #b101) (sci 1.5e3) (s "esc\n\"end") (sym 'symbol) (vs `(,@xs ,c))) (puthash kw t h) (when (< tally 5) (inc tally) (princ (format "%d %s%c%S\n" tally s c xs))) (cond ((null xs) nil) ((numberp tally) hex) (t (or sci oct bin))) (dolist (x '(1 2 3)) (princ x)) (dotimes (i 3) (princ i)) (while nil nil) (condition-case err (error "boom") (error (princ err))) (catch 'tag (throw 'tag 42)) (apply #'+ '(1 2 3)) (funcall (lambda (n) (* n n)) 7) (-> 5 (+ 10)) (->> '(1 2 3) (mapcar (lambda (n) (1+ n)))) (and t nil) (or t nil) (xor t nil) (if-let ((v (gethash kw h))) v 'none) (when-let ((v (gethash 'absent h))) (princ v))))"##;

const BIG_EXPECTED: &str = r##"(defun fib (n &optional verbose)
  (if (<= n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))
(defmacro inc (x) `(setq ,x (1+ ,x)))
(defvar tally 0)
(defconst pi 3.14)
(setq tally
      (let* ((kw :tag) (xs '(1 2 3 . 4))
                       (h (make-hash-table))
                       (c ?\n)
                       (q ?\\)
                       (hex #x1A)
                       (oct #o17)
                       (bin #b101)
                       (sci 1.5e3)
                       (s "esc\n\"end")
                       (sym 'symbol)
                       (vs `(,@xs ,c)))
        (puthash kw t h)
        (when (< tally 5)
          (inc tally)
          (princ (format "%d %s%c%S\n" tally s c xs)))
        (cond ((null xs) nil) ((numberp tally) hex) (t (or sci oct bin)))
        (dolist (x '(1 2 3)) (princ x))
        (dotimes (i 3) (princ i))
        (while nil nil)
        (condition-case err (error "boom") (error (princ err)))
        (catch 'tag (throw 'tag 42))
        (apply #'+ '(1 2 3))
        (funcall (lambda (n) (* n n)) 7)
        (-> 5 (+ 10))
        (->> '(1 2 3) (mapcar (lambda (n) (1+ n))))
        (and t nil)
        (or t nil)
        (xor t nil)
        (if-let ((v (gethash kw h))) v 'none)
        (when-let ((v (gethash 'absent h))) (princ v))))
"##;

#[test]
fn big_one_liner_pretty_prints() {
    let formatted = tulisp_fmt::format(BIG_INPUT).expect("parse");
    assert_eq!(formatted, BIG_EXPECTED);
}

#[test]
fn big_one_liner_is_idempotent() {
    let once = tulisp_fmt::format(BIG_INPUT).expect("parse");
    let twice = tulisp_fmt::format(&once).expect("re-parse");
    assert_eq!(once, twice);
}
