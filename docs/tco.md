# Tail Call Optimization

The bytecode VM (`src/bytecode/interpreter.rs`) runs a marked call in
tail position without growing the Rust stack, for self-recursion and for
calls between functions.

## Bounce marks

The compiler rewrites a function body before it compiles it:

1. `mark_tail_calls` walks a function body and finds calls in tail position
   (including inside `if`, `cond`, `progn`, `let`, `let*`).
2. Each qualifying tail call `(fn arg1 arg2 ...)` is rewritten to
   `(Bounce fn arg1 arg2 ...)`, a cons whose `car` is the fieldless
   `TulispValue::Bounce` marker and whose `cdr` is the call.

`compile_form` compiles a form headed by the marker into a jump, a
`TailCall` or an ordinary call, so the marker is never called as a
function. The marker is not a symbol, so no function or variable of the
program can shadow it. A marked call prints as the call, so an error
trace shows it as written, in its own entry and in the forms around it.

## Which tail calls are marked

`mark_tail_calls` lives in `src/parse.rs`. It marks a tail call when:

- it calls the function being defined (self-recursion), or
- the compiler has registered the callee in `defun_args`.

A call to anything else is left as an ordinary call. That includes a
function held in a variable, and a function defined later in the program
that is not registered yet.

## Compiling a marked call

`compile_defun` in `src/bytecode/compiler/forms/other_functions.rs` calls
`mark_tail_calls`. `compile_fn_defun_bounce_call` compiles a marked call:

- Inside a `let` or `let*` of the same function that binds a special
  (`defvar`) variable, any marked call is an ordinary call: a tail call
  would leave that binding before the callee runs. Its arity is checked
  when it runs, and recursion through it counts toward the eval depth
  limit (`TulispContext::set_max_eval_depth`), as in Emacs.
- Otherwise, a self call stores the arguments in the function's own
  parameters and jumps to its start (`Jump(Pos::Abs(0))`).
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
tail call to a name whose `defun` is not there fails with "undefined
function".

## Related code

- `src/parse.rs`: `mark_tail_calls`.
- `src/bytecode/compiler/compiler.rs`: `pre_register_defun_arities`.
- `src/bytecode/compiler/forms/mod.rs`: `compile_form` (tail-call
  detection).
- `src/bytecode/compiler/forms/other_functions.rs`: `compile_defun`,
  `compile_fn_defun_bounce_call`.
- `src/bytecode/interpreter.rs`: the `TailCall` instruction and
  `run_tail_calls`.
- `src/value.rs`: `TulispValue::Bounce`.

## Test coverage

- `src/bytecode/compiler/forms/other_functions.rs`:
  - `test_tco`: self-recursion through `if`, `cond`, `let` and `progn`,
    and mutual recursion.
  - `test_mutual_tail_recursion_is_tco`, and the arity checks
    `test_mutual_tail_call_arity_checked_at_compile_time` and
    `test_self_tail_recursion_arity_checked_at_compile_time`.
  - `a_tail_call_is_not_taken_over_by_list`: a variable named `list`,
    or a redefined `list`, leaves the marker alone.
  - `a_tail_call_keeps_a_special_binding`: a call in tail position
    inside a `let` that binds a special variable keeps that binding.
  - `a_tail_call_arity_error_traces_the_call_once`: the trace of a
    tail call shows the call once, without the marker.
  - `a_trace_shows_no_tail_call_marker`: the forms around a tail
    call also show it without the marker.
- `src/bytecode/compiler/compiler.rs`:
  `defuns_in_a_top_level_progn_tail_call_each_other`,
  `defuns_a_macro_produces_tail_call_each_other` and
  `a_defun_tail_calls_one_in_a_later_progn`.
