# Tail Call Optimization

Tulisp has two evaluator backends: the tree-walker (`src/eval.rs`) and the
bytecode VM (`src/bytecode/interpreter.rs`). Both run a marked call in
tail position without growing the Rust stack, for self-recursion and for
calls between functions. Which calls get marked differs; see below.

## Shared design: Bounce marks

Both backends use the same rewrite of a function body:

1. `mark_tail_calls` walks a function body and finds calls in tail position
   (including inside `if`, `cond`, `progn`, `let`, `let*`).
2. Each qualifying tail call `(fn arg1 arg2 ...)` is rewritten to
   `(list Bounce fn arg1 arg2 ...)` — a fresh cons list whose `car` is the
   `TulispValue::Bounce` marker.

The tree-walker runs that form: the caller returns the bounced value, and
its trampoline dispatches again without growing the host stack. The VM
compiles the form into a jump or a `TailCall`, so no bounced value exists
when it runs.

`TulispValue::Bounce` is fieldless; it lives at the head of the returned list.
`is_bounced()` is `matches!(self, TulispValue::List { cons, .. })` where
`cons.car()` is `TulispValue::Bounce`.

## Which tail calls are marked

`mark_tail_calls` lives in `src/parse.rs`. It marks a tail call when:

- it calls the function being defined (self-recursion), or
- the VM has registered the callee in `defun_args`, or
- the callee's symbol holds a tree-walker `Lambda`.

A call to anything else is left as an ordinary call. That includes a
function held in a variable, and a function defined later in the program
when neither of the last two rules applies.

## Tree-walker

`eval::defun_lambda` calls `mark_tail_calls` when a `defun` runs. The
trampoline is `eval_lambda` in `src/eval.rs`. After running the body once,
it loops while the result is bounced: it takes the function (`cadr`) and
the arguments (`cddr`) and runs that function without recursing. Since
the tree-walker defines a function only when its `defun` runs, a tail call
to a function defined further down is not marked, unless the VM has
registered its name.

## VM

`compile_defun` in `src/bytecode/compiler/forms/other_functions.rs` calls
the same `mark_tail_calls`. `compile_fn_defun_bounce_call` compiles a
marked call:

- A self call stores the arguments in the function's own parameters and
  jumps to its start (`Jump(Pos::Abs(0))`).
- A call to another function becomes a `TailCall`. When the callee is in
  `defun_args`, its arity is checked at compile time. `run_tail_calls` in
  `src/bytecode/interpreter.rs` loops on it without a new Rust frame.

Before a program compiles, `pre_register_defun_arities` puts every `defun`
at the program's top level, or in a `progn` there, into `defun_args`. The
walk over the top-level forms does the same for each `progn` a macro
expands to. So such functions can tail-call each other whatever their
order. A `defun` that a macro produces on its own is registered only when
the walk reaches it: a tail call to it from an earlier function is an
ordinary call.

`TailCall` finds its target only in the machine's function table, so a
tail call to a function the tree-walker defined, and the VM never
compiled, fails with "undefined function".

## Related code

- `src/parse.rs`: `mark_tail_calls`.
- `src/eval.rs`: `defun_lambda`, `eval_lambda`.
- `src/bytecode/compiler/compiler.rs`: `pre_register_defun_arities`.
- `src/bytecode/compiler/forms/other_functions.rs`: `compile_defun`,
  `compile_fn_list` (tail-call detection), `compile_fn_defun_bounce_call`.
- `src/bytecode/interpreter.rs`: the `TailCall` instruction and
  `run_tail_calls`.
- `src/value.rs`: `TulispValue::Bounce`; `src/object.rs`: `is_bounced`.

## Test coverage

- `src/bytecode/compiler/forms/other_functions.rs`:
  - `test_tco`: self-recursion through `if`, `cond`, `let` and `progn` in
    both evaluators, and mutual recursion in the VM.
  - `test_mutual_tail_recursion_is_tco`, and the arity checks
    `test_mutual_tail_call_arity_checked_at_compile_time` and
    `test_self_tail_recursion_arity_checked_at_compile_time`.
- `src/bytecode/compiler/compiler.rs`:
  `defuns_in_a_top_level_progn_tail_call_each_other`,
  `defuns_a_macro_produces_tail_call_each_other` and
  `a_defun_tail_calls_one_in_a_later_progn`.
