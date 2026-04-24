# Tail Call Optimization

Tulisp has two evaluator backends — the tree-walker (`src/eval.rs`) and the
bytecode VM (`src/bytecode/interpreter.rs`) — and they implement TCO
differently. As of the `vm` branch merge of `lexical-binding` (2026-04-24),
the tree-walker supports general TCO (self + mutual); the VM only supports
self-recursive TCO.

## Shared design: Bounce trampolines

Both backends use the same compile-time rewrite:

1. `mark_tail_calls` walks a function body and finds calls in tail position
   (including inside `if`, `cond`, `progn`, `let`, `let*`).
2. Each qualifying tail call `(fn arg1 arg2 ...)` is rewritten to
   `(list Bounce fn arg1 arg2 ...)` — a fresh cons list whose `car` is the
   `TulispValue::Bounce` marker.
3. At runtime, the caller returns this bounced value instead of recursing.
4. The *caller's* machinery checks `is_bounced()` and, if so, unpacks the
   function + args and dispatches again — without growing the host stack.

`TulispValue::Bounce` is fieldless; it lives at the head of the returned list.
`is_bounced()` is `matches!(self, TulispValue::List { cons, .. })` where
`cons.car()` is `TulispValue::Bounce`.

## Tree-walker: general TCO

**`mark_tail_calls` lives in `src/builtin/functions/core.rs`.** Signature
takes `self_name: Option<&TulispObject>`. A tail call is rewritten if:

- it's a self-recursive call (`tail_ident == self_name`), **or**
- `ctx.eval(&tail_ident)` resolves to a `TulispValue::Lambda { .. }`
  (i.e. any known user-defined function — this is what enables mutual
  recursion).

**Trampoline: `eval_lambda` in `src/eval.rs`.** After calling `eval_function`
once, it loops while `result.is_bounced()`, extracting `cadr` (the lambda)
and `cddr` (the args) and re-entering `eval_function` without recursing.
Because the next function's body may itself return a bounced value for
*another* function, this works for arbitrary mutual recursion.

The lambda check at rewrite time is important: for unknown or not-yet-bound
calls, `mark_tail_calls` leaves the call alone. That means a function that
tail-calls itself before the target exists still TCOs (the self-name check
catches it), but ordinary forward references to not-yet-defined functions
fall back to non-TCO — which is fine in practice because Elisp programs
define mutually recursive helpers in order.

## VM: self-TCO only

**`mark_tail_calls` lives in `src/parse.rs`.** Different signature — takes
the defun name as a plain `TulispObject` — and only rewrites `tail_ident ==
name` (self-recursion). No `ctx.eval` lambda check; mutual recursion is not
detected.

**Tail-call site: `compile_fn_list` in
`src/bytecode/compiler/forms/other_functions.rs`.** When compiling a call
and `args.is_bounced()` is true, it dispatches to
`compile_fn_defun_bounce_call`, which:

1. Compiles each argument expression.
2. Stores the results into the *current* function's parameter slots via
   `StorePop`.
3. Emits `Instruction::Jump(Pos::Abs(0))` — jump back to the start of the
   same compiled function.

This reuses the current `run_function` Rust stack frame, so self-recursion is
O(1) stack regardless of depth.

**Why mutual recursion overflows:** `Instruction::Call` in
`src/bytecode/interpreter.rs` always does
`self.run_function(ctx, &instructions, recursion_depth + 1)?` — a native
Rust recursion per logical call. 30000-deep mutual recursion ≈ 30000 Rust
stack frames ≈ stack overflow (cargo test threads default to ~2 MiB).

Native binaries (the CLI, examples) have an 8 MiB main-thread stack and
handle 30000 fine; the overflow is specific to test threads.

## What general VM TCO would need

The VM's `Call` would need a tail-call variant that, instead of calling
`run_function` recursively, unwinds the current frame and dispatches to the
callee's bytecode within the same Rust stack frame. Options:

- **Separate opcode** (`TailCall`): the compiler emits it when the call is
  in tail position. Execution swaps the current `instructions`/`pc` pointers
  for the callee's and continues the loop.
- **Inner dispatch loop** at `Call`: detect tail position at compile time
  (keep a "next instruction is Ret" hint) and convert to a local loop. Same
  shape, different plumbing.

Either approach also needs to unify argument binding: self-TCO today uses
`StorePop` directly into param slots because it knows the params are already
bound. A general tail call to a *different* function needs to bind the
target function's params — which means the compiler needs `VMDefunParams`
for the callee at the tail-call site. `defun_args` is keyed by
`addr_as_usize()` so it's already keyed across functions; the tail-call site
just needs to look up the callee's params, not only the enclosing function's.

The trampoline approach used by the tree-walker (return a bounced value,
loop at the entry point) is awkward to port: the VM's `Call` instruction
currently pushes the call's result onto `self.stack` and moves on. A bounced
return value would need to be unpacked by the caller's `Ret` or by a
dedicated post-call step, and care needs to be taken so that non-tail calls
are unaffected.

## Related code

- `src/eval.rs` — `eval_lambda` trampoline, `eval_function`.
- `src/builtin/functions/core.rs` — tree-walker `mark_tail_calls`, defun site.
- `src/parse.rs` — VM `mark_tail_calls` (self-only).
- `src/bytecode/compiler/forms/other_functions.rs` —
  `compile_fn_list` tail-call detection, `compile_fn_defun_bounce_call`
  self-TCO codegen.
- `src/bytecode/interpreter.rs` — `Instruction::Call` implementation,
  `run_function` recursion.
- `src/value.rs` — `TulispValue::Bounce`, `is_bounce`, `is_bounced`.

## Test coverage

`test_tco` in `tests/tests.rs` has four sub-tests:

1. `if-tail` — self-recursive via `if`. **TW + VM pass.**
2. `cond-tail` — self-recursive via `cond`. **TW + VM pass.**
3. `progn-tail` — self-recursive via `let` + `progn`. **TW + VM pass.**
4. `my-even` / `my-odd` — mutual recursion at depth 30000. **TW passes,
   VM overflows** (added in `d37b33d` on the lexical-binding branch).
