/*!
Built-in functions and macros registered on every [`TulispContext`].

Names match the [Emacs Lisp manual] where applicable; semantic
differences from Emacs are called out inline.

[`TulispContext`]: crate::TulispContext
[Emacs Lisp manual]: https://www.gnu.org/software/emacs/manual/html_node/elisp/

# Numbers

- **Arithmetic**: `+`, `-`, `*`, `/`, `%` (remainder, with the sign of the
  dividend), `mod`, `1+`, `1-`.
- **Comparison**: `=`, `<`, `>`, `<=`, `>=`, `eql`, `max`, `min`, `abs`.
- **Math**: `expt`, `sqrt`, `isnan`.
- **Numerical conversion**: `floor`, `ceiling`, `truncate`, `round`,
  `ffloor`, `fceiling`, `ftruncate`, `fround`. The integer-returning
  forms take an optional divisor, and divide two integers exactly. `round`
  and `fround` break a tie to the even number.

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
- **Modification**: `setcar`, `setcdr`, `push` (PLACE must be a
  variable: tulisp has no generalized variables).
- **Length / membership**: `length` (also for strings),
  `memq`, `memql`, `member`.
- **Sequence operations**: `reverse`, `sort`, `mapcar`, `mapconcat`,
  `string-join`, `seq-map`, `seq-filter`, `seq-reduce`, `seq-find`,
  `seq-take`, `seq-drop`.
- **Ordering**: `value<`, the order `sort` uses when given no predicate:
  numbers by value, strings and symbols by name, lists element by element.
- **Alists**: `assoc`, `alist-get`.
- **Plists**: `plist-get`.

A list function that must walk a whole list signals a `Circular list`
error when the list's cdrs loop back to an earlier cell. Unlike Emacs,
`last` and `plist-get` without a PREDICATE also do so.

Tulisp has no vector type — sequence functions are list-only.

# Symbols and variables

- **Bindings**: `let`, `let*`, `setq`, `set`, `symbol-value`.
- **Symbols**: `intern` (always uses the default obarray), `make-symbol`,
  `gensym`.
- **Declaration**: `defvar` (sets only when the name has no top-level
  value — preserves value across reloads; with no value, only marks the
  name special).
- **Constants**: `nil`, `t`.

# Functions and macros

- **Definitions**: `defun`, `defmacro`, `lambda`, `declare`.
- **Invocation**: `eval`, `funcall`, `apply`, `macroexpand`, `macroexpand-1`,
  `macroexpand-all`.
- **Loading**: `load`, which evaluates a file, found under the load path when
  [`set_load_path`](crate::TulispContext::set_load_path) set one, and
  returns `t`; with NOERROR, a missing file gives nil.
- **Quoting**: `quote` (also written `'expr`), `function` (also written
  `#'expr`; makes a closure of a `lambda`), backquote / unquote / splice
  (`` ` ``, `,`, `,@`).
- **Threading**: `->` / `thread-first`, `->>` / `thread-last`.

Tail-call optimisation is applied to recursive functions automatically.

# Control flow

- **Branches and loops**: `if`, `cond`, `when`, `unless`, `progn`, `prog1`,
  `prog2`, `while`, `dolist`, `dotimes`.
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

`error`, `signal`, `define-error`, `error-message-string`, `user-error`,
`throw`, `catch`, `condition-case`, `unwind-protect`.
*/

pub(crate) mod functions;
pub(crate) mod macros;

use crate::{Error, TulispContext, TulispObject, TulispValue};

/// Returns the "Can't set constant symbol" error when `name` is `nil`
/// or `t`. Keywords are not checked here.
pub(crate) fn check_not_nil_or_t(name: &TulispObject) -> Result<(), Error> {
    if matches!(name.inner_ref().0, TulispValue::Nil | TulispValue::T) {
        return Err(Error::setting_constant(name).with_trace(name.clone()));
    }
    Ok(())
}

/// Refuses a `defvar` name that is `nil`, `t` or not a symbol.
pub(crate) fn check_defvar_name(name: &TulispObject) -> Result<(), Error> {
    check_not_nil_or_t(name)?;
    if !name.is_symbol_variant() {
        return Err(Error::type_mismatch(
            "defvar: first argument must be a symbol".to_string(),
        ));
    }
    Ok(())
}

/// BODY of a `defun` or `defmacro` without the `(declare ...)` form that
/// follows its leading string, as Emacs drops it before it tells a
/// docstring from a string that is the value.
pub(crate) fn drop_declare_after_docstring(
    ctx: &TulispContext,
    body: TulispObject,
) -> Result<TulispObject, Error> {
    let first = body.car()?;
    let rest = body.cdr()?;
    let next = rest.car()?;
    if first.stringp() && next.consp() && next.car()?.eq(&ctx.keywords.declare) {
        return Ok(TulispObject::cons(first, rest.cdr()?));
    }
    Ok(body)
}

/// Checks the parameter list of a `defun`, `defmacro` or `lambda`: a
/// list of symbols, with at most one symbol after `&rest`. `&optional`
/// and `&rest` are the interned symbols, as the compilers read them.
pub(crate) fn check_param_list(ctx: &TulispContext, params: &TulispObject) -> Result<(), Error> {
    if !params.listp() {
        return Err(Error::syntax_error(
            "Parameter list needs to be a list".to_string(),
        ));
    }
    let mut params_iter = params.base_iter();
    let mut is_rest = false;
    while let Some(param) = params_iter.next() {
        check_not_nil_or_t(&param)?;
        param.as_symbol()?;
        if param.eq(&ctx.keywords.amp_optional) {
            continue;
        } else if param.eq(&ctx.keywords.amp_rest) {
            is_rest = true;
            continue;
        }
        if is_rest {
            if params_iter.next().is_some() {
                return Err(Error::type_mismatch(
                    "Too many &rest parameters".to_string(),
                ));
            }
            break;
        }
    }
    params_iter.take_error()
}

/// Validate that `target` is a writable variable cell. The compiler of
/// `setq` calls it, so a bad target is refused at compile time, before
/// any value expression evaluates. The compiler of `condition-case`
/// calls it for VAR, and each handler raises the error when it runs.
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
    use crate::test_utils::{eval_assert_equal, eval_assert_error_line};

    // An uninterned symbol named `&rest` is an ordinary parameter, as
    // in Emacs.
    #[test]
    fn an_uninterned_rest_is_an_ordinary_parameter() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            r#"(funcall (eval (list 'lambda (list (make-symbol "&rest") 'a 'b) 'b)) 1 2 3)"#,
            "3",
        );
        eval_assert_equal(
            ctx,
            r#"(eval (list 'defun 'ur-f (list (make-symbol "&rest") 'a 'b) 'b))"#,
            "'ur-f",
        );
        eval_assert_equal(ctx, "(ur-f 1 2 3)", "3");
        eval_assert_equal(
            ctx,
            r#"(eval (list 'defmacro 'ur-m (list (make-symbol "&rest") 'a 'b) 'b))"#,
            "'ur-m",
        );
        eval_assert_equal(ctx, "(ur-m 1 2 3)", "3");
    }

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

    // `push` conses onto a variable, evaluating NEWELT first, and
    // gives the new list.
    #[test]
    fn push_conses_onto_a_variable() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(let ((l '(2))) (push 1 l) l)", "'(1 2)");
        eval_assert_equal(ctx, "(let ((l nil)) (push 1 l))", "'(1)");
        eval_assert_equal(
            ctx,
            "(let ((l nil) (i 0)) (push (setq i (1+ i)) l) (push i l))",
            "'(1 1)",
        );
        eval_assert_equal(
            ctx,
            "(let ((l '(2))) (push (progn (setq l nil) 1) l))",
            "'(1)",
        );
        eval_assert_equal(ctx, "(defun f (l) (push 0 l)) (f '(1))", "'(0 1)");
        // The `cons` it expands to is the function, not a variable.
        eval_assert_equal(ctx, "(defun g (cons) (push 1 cons) cons) (g nil)", "'(1)");
        // Only a variable is a place: tulisp has no generalized
        // variables.
        eval_assert_error_line(
            ctx,
            "(let ((l (list 1))) (push 0 (car l)))",
            "ERR LispError: push: PLACE must be a variable, got: (car l)",
        );
    }

    #[test]
    fn prog1_and_prog2_are_prelude_macros() {
        let ctx = &mut TulispContext::new();
        // prog1 returns FIRST, evaluates BODY for side effects.
        eval_assert_equal(
            ctx,
            "(let ((trace nil))
           (prog1 (progn (setq trace (cons 1 trace)) 'first)
                  (setq trace (cons 2 trace))
                  (setq trace (cons 3 trace))))",
            "'first",
        );
        // Order of evaluation is FIRST, then BODY left-to-right.
        eval_assert_equal(
            ctx,
            "(let ((trace nil))
           (prog1 (progn (setq trace (cons 1 trace)) 'first)
                  (setq trace (cons 2 trace))
                  (setq trace (cons 3 trace)))
           (reverse trace))",
            "'(1 2 3)",
        );
        // prog1 with no body still returns FIRST.
        eval_assert_equal(ctx, "(prog1 42)", "42");
        // prog2 returns SECOND.
        eval_assert_equal(ctx, "(prog2 1 2 3 4)", "2");
        // prog2 evaluates FIRST, then SECOND, then BODY.
        eval_assert_equal(
            ctx,
            "(let ((trace nil))
           (prog2 (setq trace (cons 1 trace))
                  (setq trace (cons 2 trace))
                  (setq trace (cons 3 trace)))
           (reverse trace))",
            "'(1 2 3)",
        );
        // Hygiene: a user variable named `prog1-result` doesn't collide
        // with the macro's internal gensym.
        eval_assert_equal(
            ctx,
            "(let ((prog1-result 'outer))
           (prog1 'returned
                  (setq prog1-result 'mutated))
           prog1-result)",
            "'mutated",
        );
    }
}
