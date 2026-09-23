/*!
Built-in functions and macros registered on every [`TulispContext`].

Names match the [Emacs Lisp manual] where applicable; semantic
differences from Emacs are called out inline.

[`TulispContext`]: crate::TulispContext
[Emacs Lisp manual]: https://www.gnu.org/software/emacs/manual/html_node/elisp/

# Numbers

- **Arithmetic**: `+`, `-`, `*`, `/`, `mod`, `1+`, `1-`.
- **Comparison**: `=`, `<`, `>`, `<=`, `>=`, `eql`, `max`, `min`, `abs`.
- **Math**: `expt`, `sqrt`, `isnan`.
- **Numerical conversion**: `floor`, `ceiling`, `truncate`, `round`,
  `ffloor`, `fceiling`, `ftruncate`, `fround`. The integer-returning
  forms take an optional divisor; `round` uses banker's rounding.

# Strings

- **Construction**: `concat`, `format`, `make-string` (takes `(N CHAR)`
  where `CHAR` is an integer).
- **Comparison**: `string<` / `string-lessp`, `string>` / `string-greaterp`,
  `string=` / `string-equal`.
- **Output**: `princ`, `print` (behaves like `princ`, not Emacs's `print`),
  `prin1-to-string`.
- **Mutation**: `aset` (replaces a single char at an index).

# Lists

- **Construction**: `cons`, `list`, `append`.
- **Access**: `car`, `cdr`, every `c[ad]+r` form up to four `a`/`d`s,
  `nth`, `nthcdr`, `last`.
- **Modification**: `setcar`, `setcdr`.
- **Length / membership**: `length` (also for strings),
  `memq`, `memql`, `member`.
- **Sequence operations**: `reverse`, `sort`, `mapcar`, `mapconcat`,
  `string-join`, `seq-map`, `seq-filter`, `seq-reduce`, `seq-find`,
  `seq-take`, `seq-drop`.
- **Alists**: `assoc`, `alist-get`.
- **Plists**: `plist-get`.

A list function that must walk a whole list signals a `Circular list`
error when the list's cdrs loop back to an earlier cell. Unlike Emacs,
`last`, `plist-get` and `seq-drop` also do so.

Tulisp has no vector type — sequence functions are list-only.

# Symbols and variables

- **Bindings**: `let`, `let*`, `setq`, `set`, `symbol-value`.
- **Symbols**: `intern` (always uses the default obarray), `make-symbol`,
  `gensym`.
- **Declaration**: `defvar` (sets only when unbound — preserves value
  across reloads).
- **Constants**: `nil`, `t`.

# Functions and macros

- **Definitions**: `defun`, `defmacro`, `lambda`, `declare`.
- **Invocation**: `eval`, `funcall`, `apply`, `macroexpand`.
- **Quoting**: `quote` (also written `'expr`), backquote / unquote /
  splice (`` ` ``, `,`, `,@`).
- **Threading**: `->` / `thread-first`, `->>` / `thread-last`.

Tail-call optimisation is applied to recursive functions automatically.

# Control flow

- **Branches and loops**: `if`, `cond`, `when`, `unless`, `progn`,
  `while`, `dolist`, `dotimes`.
- **Logic**: `and`, `or`, `not`, `xor`.
- **Pattern-matching binds**: `if-let`, `if-let*`, `when-let`,
  `while-let`.

# Predicates

- **Types**: `atom`, `consp`, `listp`, `floatp`, `integerp`, `numberp`,
  `stringp`, `symbolp`, `keywordp`, `functionp`, `boundp`, `null`.
- **Equality**: `eq`, `equal`, `eql`.

# Hash tables

`make-hash-table` (`:test` selects `eq` / `eql` / `equal` key
comparison, default `eql`; `:size` is accepted as a hint and
ignored), `puthash`, `gethash` (optional 3rd `default` argument).

# Time

`current-time` returns a `(ticks . hz)` pair (typically `hz =
1_000_000_000` for nanosecond resolution).

`time-add`, `time-subtract`, `time-less-p`, `time-equal-p` each take
two times — either integer Unix-epoch seconds or `(ticks . hz)`
pairs. `format-seconds` formats a duration.

# Errors

`error`, `throw`, `catch`, `condition-case`.
*/

pub(crate) mod functions;
pub(crate) mod macros;

use crate::{Error, TulispObject, TulispValue};

/// Returns the "Can't set constant symbol" error when `name` is `nil`
/// or `t`. Keywords are not checked here.
pub(crate) fn check_not_nil_or_t(name: &TulispObject) -> Result<(), Error> {
    if matches!(name.inner_ref().0, TulispValue::Nil | TulispValue::T) {
        return Err(Error::setting_constant(name).with_trace(name.clone()));
    }
    Ok(())
}

/// Validate that `target` is a writable variable cell. Used by both
/// the VM compiler (~setq~) and the TW ~setq~ defspecial so the two
/// dispatch paths reject the same inputs with the same error shape,
/// at compile time, before any value expression evaluates.
pub(crate) fn check_settable_target(target: &TulispObject) -> Result<(), Error> {
    check_not_nil_or_t(target)?;
    if !target.is_symbol_variant() {
        return Err(
            Error::type_mismatch(format!("Expected Symbol: Can't assign to {}", target))
                .with_trace(target.clone()),
        );
    }
    if target.keywordp() {
        return Err(Error::setting_constant(target).with_trace(target.clone()));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::TulispContext;
    use crate::test_utils::{eval_assert_equal, eval_assert_error, eval_assert_error_line};

    #[test]
    fn mapcar_maps_a_list() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(mapcar '1+ '(10 20 30))", "'(11 21 31)");

        eval_assert_equal(
            ctx,
            r#"(mapcar (lambda (vv) (plist-get vv :age)) '((:name "person" :age 20) (:name "person2" :age 30)))"#,
            "'(20 30)",
        );
    }

    #[test]
    fn seq_functions_walk_a_list() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(seq-map #'1+ '(2 4 6))", "'(3 5 7)");
        eval_assert_equal(
            ctx,
            r#"(seq-filter #'numberp '(2 4 6 "hello" 8))"#,
            "'(2 4 6 8)",
        );
        eval_assert_equal(
            ctx,
            r#"(seq-filter (lambda (x) (> x 5)) '(2 4 6 8))"#,
            "'(6 8)",
        );
        eval_assert_equal(
            ctx,
            r##"
        (let
            ((items '(2 4 6 "hello" 8)))
         (list (seq-find #'numberp items) (seq-find #'stringp items)))
        "##,
            r#"'(2 "hello")"#,
        );
        eval_assert_equal(
            ctx,
            r##"
        (seq-reduce #'+ '(2 4 6 8) 0)
        "##,
            "20",
        );
        eval_assert_equal(
            ctx,
            r##"
        (seq-reduce (lambda (x y) (+ x y)) '(2 4 6 8) 5)
        "##,
            "25",
        );
    }

    #[test]
    fn prelude_list_functions_reject_a_circular_list() {
        let ctx = &mut TulispContext::new();
        for call in [
            "(mapcar '1+ l)",
            "(seq-map '1+ l)",
            "(seq-filter 'numberp l)",
            "(seq-reduce '+ l 0)",
            "(seq-find 'stringp l)",
            "(mapconcat 'prin1-to-string l)",
            "(sort l '<)",
        ] {
            eval_assert_error_line(
                ctx,
                &format!("(let ((l (list 1 2 3))) (setcdr (cddr l) l) {call})"),
                "ERR OutOfRange: Circular list",
            );
        }
    }

    #[test]
    fn sort_orders_by_the_predicate() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(sort '(20 10 30 15 45) '<)", "'(10 15 20 30 45)");
        eval_assert_equal(ctx, "(sort '(20 10 30 15 45) '>)", "'(45 30 20 15 10)");
        // With a `>`-typed predicate applied to strings, the inner
        // `funcall` hits a number-only operator and errors. The exact
        // string that trips it depends on the sort walk order; `hello`
        // happens to be first under the current Lisp implementation.
        //
        // The trace frames inside the prelude carry the crate-absolute
        // path to `prelude.lisp` (see `eval_prelude` in `context.rs`),
        // so we inject that at compile time via `CARGO_MANIFEST_DIR`.
        let prelude = concat!(env!("CARGO_MANIFEST_DIR"), "/src/builtin/prelude.lisp");
        eval_assert_error(
            ctx,
            r#"(sort '("sort" "hello" "a" "world") '>)"#,
            &format!(
                r#"ERR TypeMismatch: Expected number, got: "hello"
{0}:87.35-87.55:  at (funcall pred item x)
{0}:87.15-87.56:  at (and (not inserted) (funcall pred item x))
{0}:87.11-91.36:  at (if (and (not inserted) (funcall pred item x)) (progn (setq new (cons item new))...
{0}:86.9-91.37:  at (let ((tail out)) (while tail (let ((x (car tail))) (if (and (not inserted) (fun...
{0}:85.7-94.33:  at (let ((inserted nil) (new nil)) (let ((tail out)) (while tail (let ((x (car tail...
{0}:84.5-94.34:  at (let ((tail seq)) (while tail (let ((item (car tail))) (let ((inserted nil) (new...
{0}:83.3-95.8:  at (let ((out nil)) (let ((tail seq)) (while tail (let ((item (car tail))) (let ((i...
<eval_string>:1.1-1.39:  at (sort '("sort" "hello" "a" "world") '>)
"#,
                prelude
            ),
        );
        eval_assert_equal(
            ctx,
            r#"(sort '("sort" "hello" "a" "world") 'string<)"#,
            r#"'("a" "hello" "sort" "world")"#,
        );
        eval_assert_equal(
            ctx,
            r#"(sort '("sort" "hello" "a" "world") 'string>)"#,
            r#"'("world" "sort" "hello" "a")"#,
        );
        // `sort` is written in Lisp, so an unknown predicate fails at
        // the inner `funcall`, and the trace has the sort body's
        // frames.
        eval_assert_error(
            ctx,
            "(sort '(20 10 30 15 45) '<<)",
            &format!(
                r#"ERR Uninitialized: Variable definition is void: <<
{0}:87.35-87.55:  at (funcall pred item x)
{0}:87.15-87.56:  at (and (not inserted) (funcall pred item x))
{0}:87.11-91.36:  at (if (and (not inserted) (funcall pred item x)) (progn (setq new (cons item new))...
{0}:86.9-91.37:  at (let ((tail out)) (while tail (let ((x (car tail))) (if (and (not inserted) (fun...
{0}:85.7-94.33:  at (let ((inserted nil) (new nil)) (let ((tail out)) (while tail (let ((x (car tail...
{0}:84.5-94.34:  at (let ((tail seq)) (while tail (let ((item (car tail))) (let ((inserted nil) (new...
{0}:83.3-95.8:  at (let ((out nil)) (let ((tail seq)) (while tail (let ((item (car tail))) (let ((i...
<eval_string>:1.1-1.28:  at (sort '(20 10 30 15 45) '<<)
"#,
                prelude
            ),
        );
        eval_assert_error(
            ctx,
            "(sort '(20 10 30 15 45))",
            r#"ERR ArityMismatch: Too few arguments
<eval_string>:1.1-1.24:  at (sort '(20 10 30 15 45))
"#,
        );
        eval_assert_equal(
            ctx,
            "(defun << (v1 v2) (> v1 v2)) (sort '(20 10 30 15 45) '<<)",
            "'(45 30 20 15 10)",
        );

        eval_assert_equal(
            ctx,
            "(sort '(20 10 30 15 45) '(lambda (v1 v2) (> v1 v2)))",
            "'(45 30 20 15 10)",
        );
    }
}
