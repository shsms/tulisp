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
| `if-let`    | 🔳     | [#11](https://github.com/shsms/tulisp/issues/11) |
| `when-let`  | 🔳     |                                                  |
| `while-let` | 🔳     |                                                  |


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


*/

pub(crate) mod functions;
pub(crate) mod macros;
