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

| Name  | Status | Details                                             |
|-------|--------|-----------------------------------------------------|
| `>`   | ☑️      | currently broken for more than 2 args               |
| `<`   | ☑️      | 〃 [#26](https://github.com/shsms/tulisp/issues/26) |
| `>=`  | ☑️      | 〃                                                  |
| `<=`  | ☑️      | 〃                                                  |
| `=`   | 🔳     |                                                     |
| `eql` | 🔳     |                                                     |
| `/=`  | 🔳     |                                                     |
| `max` | ☑️      |                                                     |
| `min` | ☑️      |                                                     |


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
| `length`     | ☑️      |         |
| `sort`       | ☑️      |         |
| `seq-map`    | ☑️      |         |
| `seq-filter` | ☑️      |         |
| `seq-reduce` | ☑️      |         |
| `seq-find`   | ☑️      |         |
| `seq-drop`  | 🔳     |         |
| `seq-take`  | 🔳     |         |
| `seq-uniq`  | 🔳     |         |
| `seq-some`  | 🔳     |         |
| `seq-every-p` | 🔳     |         |
| `seq-contains-p` | 🔳     |         |
| `seq-position` | 🔳     |         |
| `seq-positions` | 🔳     |         |
| `seq-subseq` | 🔳     |         |
| `seq-let` | 🔳     |         |

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
| `setq`                                                                                                  | ☑️      |                                                   |
| `concat`                                                                                                | ☑️      | for strings                                       |
| `expt`                                                                                                  | ☑️      |                                                   |


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

*/

pub(crate) mod functions;
pub(crate) mod macros;
