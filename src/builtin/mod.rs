/*!
A list of currently implemented functions and macros:


## Arithmetic Operations

Click [here](https://www.gnu.org/software/emacs/manual/html_node/elisp/Arithmetic-Operations.html) for the Emacs lisp manual page for arithmetic operations.

| Name  | Status | Details |
|-------|--------|---------|
| `1+`  | ☑️      |         |
| `1-`  | ☑️      |         |
| `+`   | ☑️      |         |
| `-`   | ☑️      |         |
| `*`   | ☑️      |         |
| `/`   | ☑️      |         |
| `%`   | 🔳     |         |
| `mod` | ☑️      |         |

## Conditionals

Click  [here](https://www.gnu.org/software/emacs/manual/html_node/elisp/Conditionals.html) for the Emacs lisp manual page for conditionals.

| Name        | Status | Details                                          |
|-------------|--------|--------------------------------------------------|
| `if`        | ☑️      |                                                  |
| `when`      | ☑️      |                                                  |
| `unless`    | ☑️      |                                                  |
| `cond`      | ☑️      |                                                  |
| `not`      | ☑️      |                                                  |
| `and`      | ☑️      |                                                  |
| `or`      | ☑️      |                                                  |
| `xor`      | ☑️      |                                                  |
| `if-let`    | ☑️
| `when-let`  | ☑️     |                                                  |
| `while-let` | ☑️     |                                                  |

## Comparison of Numbers

Click [here](https://www.gnu.org/software/emacs/manual/html_node/elisp/Comparison-of-Numbers.html) for the Emacs lisp manual page for comparison of numbers.

| Name  | Status | Details |
|-------|--------|---------|
| `>`   | ☑️      |         |
| `<`   | ☑️      |         |
| `>=`  | ☑️      |         |
| `<=`  | ☑️      |         |
| `=`   | ☑️      |         |
| `eql` | ☑️     |         |
| `/=`  | 🔳     |         |
| `max` | ☑️      |         |
| `min` | ☑️      |         |
| `abs` | ☑️      |         |


## Comparison of Strings

Click [here](https://www.gnu.org/software/emacs/manual/html_node/elisp/Text-Comparison.html) for the Emacs lisp manual page for comparison of strings. Not all methods are implemented.

| Name              | Status | Details |
|-------------------|--------|---------|
| `string>`         | ☑️      |         |
| `string<`         | ☑️      |         |
| `string=`         | ☑️      |         |
| `string-lessp`    | ☑️      |         |
| `string-greaterp` | ☑️      |         |
| `string-equal`    | ☑️      |         |


## Equality Predicates

Click [here](https://www.gnu.org/software/emacs/manual/html_node/elisp/Equality-Predicates.html) for the Emacs lisp manual page for equality predicates.

| Name    | Status | Details |
|---------|--------|---------|
| `eq`    | ☑️      |         |
| `equal` | ☑️      |         |


## Accessing Elements of Lists

Click [here](https://www.gnu.org/software/emacs/manual/html_node/elisp/List-Elements.html) for the Emacs lisp manual page for accessing elements of lists.

| Name          | Status | Details                                                 |
|---------------|--------|---------------------------------------------------------|
| `car`         | ☑️      |                                                         |
| `cdr`         | ☑️      |                                                         |
| `car-safe`    | 🔳     |                                                         |
| `cdr-safe`    | 🔳     |                                                         |
| `pop`         | 🔳     |                                                         |
| `nth`         | ☑️      |                                                         |
| `nthcdr`      | ☑️      |                                                         |
| `take`        | 🔳     |                                                         |
| `ntake`       | 🔳     |                                                         |
| `last`        | ☑️      | not cycle-safe                                          |
| `safe-lentgh` | 🔳     | not until we have a cycle-detection algorithm for lists |
| `caar`        | ☑️      |                                                         |
| `cadr`        | ☑️      |                                                         |
| `cdar`        | ☑️      |                                                         |
| `cddr`        | ☑️      |                                                         |
| `cxxxr` * 8   | ☑️      |                                                         |
| `cxxxxr` * 16 | ☑️      |                                                         |
| `butlast`     | 🔳     |                                                         |
| `nbutlast`    | 🔳     |                                                         |


## Sequence Functions

Click [here](https://www.gnu.org/software/emacs/manual/html_node/elisp/Sequence-Functions.html) for the Emacs lisp manual page for sequences.

`Tulisp` doesn't have vectors.  Instead, we use lists for everything.  This means that the sequence functions are implemented just for lists.  They are not available for strings either, but they might be in the future.

Because there are a huge number of sequence functions that are not yet implemented, the following table only lists the ones that are implemented, and a few that might be implemented in the near future.

| Name         | Status | Details |
|--------------|--------|---------|
| `length`     | ☑️      | works on lists and strings |
| `make-string`| ☑️      | takes (N CHAR), where CHAR is an integer |
| `reverse`    | ☑️      |         |
| `sort`       | ☑️      |         |
| `mapconcat`  | ☑️      |         |
| `string-join`| ☑️      |         |
| `seq-map`    | ☑️      |         |
| `seq-filter` | ☑️      |         |
| `seq-reduce` | ☑️      |         |
| `seq-find`   | ☑️      |         |
| `seq-drop`   | ☑️      |         |
| `seq-take`   | ☑️      |         |
| `memq`       | ☑️      |         |
| `memql`      | ☑️      |         |
| `member`     | ☑️      |         |
| `seq-uniq`   | 🔳     |         |
| `seq-some`  | 🔳     |         |
| `seq-every-p` | 🔳     |         |
| `seq-contains-p` | 🔳     |         |
| `seq-position` | 🔳     |         |
| `seq-positions` | 🔳     |         |
| `seq-subseq` | 🔳     |         |
| `seq-let` | 🔳     |         |

## Hash Tables

Click [here](https://www.gnu.org/software/emacs/manual/html_node/elisp/Hash-Tables.html) for the Emacs lisp manual page for hash tables.

| Name               | Status | Details |
|--------------------|--------|---------|
| `make-hash-table`  | ☑️     | takes no arguments, uses `eql` as the test function. |
| `puthash`          | ☑️     |         |
| `gethash`          | ☑️     |         |

## Time Calculations

These functions are described in the [time of
 day](https://www.gnu.org/software/emacs/manual/html_node/elisp/Time-of-Day.html)
 and [time
 calculations](https://www.gnu.org/software/emacs/manual/html_node/elisp/Time-Calculations.html)
 Emacs lisp manual pages.

`time-less-p`, `time-equal-p`, `test-subtract`, `test-add` all take two arguments.  The
arguments can be integers representing a number of seconds since the Unix epoch,
or they can be `(ticks . hz)` values, representing `ticks/hz` values seconds since
the unix epoch.

| Name             | Status | Details                                                                                                           |
|------------------|--------|-------------------------------------------------------------------------------------------------------------------|
| `current-time`   | ☑️      | Returns a `(ticks . hz)` value, usually with a hz value of 1000000000, corresponding to a nano-second resolution. |
| `time-less-p`    | ☑️      |                                                                                                                   |
| `time-equal-p`   | ☑️      |                                                                                                                   |
| `test-subtract`  | ☑️      |                                                                                                                   |
| `test-add`       | ☑️      |                                                                                                                   |
| `format-seconds` | ☑️      |                                                                                                                   |

## Math Functions

| Name        | Status | Details |
|-------------|--------|---------|
| `expt`      | ☑️      |        |
| `sqrt`      | ☑️      |        |
| `abs`       | ☑️      |        |

## Error handling

| Name                                                                                                   | Status | Details |
|--------------------------------------------------------------------------------------------------------|--------|---------|
| [`error`](https://www.gnu.org/software/emacs/manual/html_node/elisp/Signaling-Errors.html#index-error) | ☑️      |         |
| [`throw`](https://www.gnu.org/software/emacs/manual/html_node/elisp/Catch-and-Throw.html#index-throw)  | ☑️      |         |
| [`catch`](https://www.gnu.org/software/emacs/manual/html_node/elisp/Catch-and-Throw.html#index-catch)  | ☑️      |         |

## Others

These functions need to be organized into categories.  They are grouped here for now.

| Name                                                                                                    | Status | Details                                           |
|---------------------------------------------------------------------------------------------------------|--------|---------------------------------------------------|
| [`while`](https://www.gnu.org/software/emacs/manual/html_node/eintr/while.html)                         | ☑️      |                                                   |
| [`format`](https://www.gnu.org/software/emacs/manual/html_node/elisp/Formatting-Strings.html)           | ☑️      |                                                   |
| [`let`](https://www.gnu.org/software/emacs/manual/html_node/eintr/let.html)                             | ☑️      |                                                   |
| [`let*`](https://www.gnu.org/software/emacs/manual/html_node/elisp/Local-Variables.html#index-let_002a) | ☑️      |                                                   |
| [`progn`](https://www.gnu.org/software/emacs/manual/html_node/eintr/progn.html)                         | ☑️      |                                                   |
| `defun`                                                                                                 | ☑️      |                                                   |
| `defmacro`                                                                                              | ☑️      |                                                   |
| `defvar`                                                                                                | ☑️      | sets only when unbound; preserves value on reload |
| `lambda`                                                                                                | ☑️      |                                                   |
| `quote`                                                                                                 | ☑️      |                                                   |
| `null`                                                                                                  | ☑️      |                                                   |
| `eval`                                                                                                  | ☑️      |                                                   |
| `funcall`                                                                                               | ☑️      |                                                   |
| `macroexpand`                                                                                           | ☑️      |                                                   |
| `cons`                                                                                                  | ☑️      |                                                   |
| `append`                                                                                                | ☑️      |                                                   |
| `dolist`                                                                                                | ☑️      |                                                   |
| `dotimes`                                                                                               | ☑️      |                                                   |
| `list`                                                                                                  | ☑️      |                                                   |
| `mapcar`                                                                                                | ☑️      |                                                   |
| `assoc`                                                                                                 | ☑️      |                                                   |
| `alist-get`                                                                                             | ☑️      |                                                   |
| `plist-get`                                                                                             | ☑️      |                                                   |
| `print`                                                                                                 | ☑️      | behaves like `princ`, not like emacs lisp `print` |
| `princ`                                                                                                 | ☑️      |                                                   |
| `prin1-to-string`                                                                                       | ☑️      |                                                   |
| `set`                                                                                                   | ☑️      |                                                   |
| `setq`                                                                                                  | ☑️      |                                                   |
| `concat`                                                                                                | ☑️      | for strings                                       |
| `load`                                                                                                  | ☑️      |                                                   |
| `intern`                                                                                                | ☑️      | no optional obarray param, always uses default.   |
| `make-symbol`                                                                                           | ☑️      |                                                   |
| `gensym`                                                                                                | ☑️      |                                                   |
| `floor`                                                                                                 | ☑️      | takes optional divisor                            |
| `ceiling`                                                                                               | ☑️      | takes optional divisor                            |
| `truncate`                                                                                              | ☑️      | takes optional divisor                            |
| `round`                                                                                                 | ☑️      | banker's rounding, takes optional divisor         |
| `ffloor`                                                                                                | ☑️      |                                                   |
| `fceiling`                                                                                              | ☑️      |                                                   |
| `fround`                                                                                                | ☑️      |                                                   |
| `ftruncate`                                                                                             | ☑️      |                                                   |

## Other predicates

| Name       | Status | Details |
|------------|--------|---------|
| `symbolp`  | ☑️      |         |
| `numberp`  | ☑️      |         |
| `stringp`  | ☑️      |         |
| `listp`    | ☑️      |         |
| `consp`    | ☑️      |         |
| `floatp`   | ☑️      |         |
| `integerp` | ☑️      |         |
| `boundp`   | ☑️      |         |
| `keywordp` | ☑️      |         |
*/

pub(crate) mod functions;
pub(crate) mod macros;
