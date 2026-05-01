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
  `nth`, `nthcdr`, `last` (errors on circular lists).
- **Modification**: `setcar`, `setcdr`.
- **Length / membership**: `length` (also for strings; cycle-safe),
  `memq`, `memql`, `member`.
- **Sequence operations**: `reverse`, `sort`, `mapcar`, `mapconcat`,
  `string-join`, `seq-map`, `seq-filter`, `seq-reduce`, `seq-find`,
  `seq-take`, `seq-drop`.
- **Alists**: `assoc`, `alist-get`.
- **Plists**: `plist-get`.

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
- **Invocation**: `eval`, `funcall`, `macroexpand`.
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

- **Types**: `consp`, `listp`, `floatp`, `integerp`, `numberp`,
  `stringp`, `symbolp`, `keywordp`, `boundp`, `null`.
- **Equality**: `eq`, `equal`, `eql`.

# Hash tables

`make-hash-table` (no arguments; uses `eql` as the test function),
`puthash`, `gethash` (optional 3rd `default` argument).

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

use crate::{Error, TulispObject};

/// Validate that `target` is a writable variable cell. Used by both
/// the VM compiler (~setq~) and the TW ~setq~ defspecial so the two
/// dispatch paths reject the same inputs with the same error shape,
/// at compile time, before any value expression evaluates.
pub(crate) fn check_settable_target(target: &TulispObject) -> Result<(), Error> {
    if !target.symbolp() {
        return Err(
            Error::type_mismatch(format!("Expected Symbol: Can't assign to {}", target))
                .with_trace(target.clone()),
        );
    }
    if target.keywordp() {
        return Err(
            Error::type_mismatch(format!("Can't set constant symbol: {}", target))
                .with_trace(target.clone()),
        );
    }
    Ok(())
}
