use super::{
    Block, Handler, Instruction, LambdaTemplate, bytecode::Bytecode, bytecode::CompiledDefun,
    bytecode::TraceRange, compiler::VMDefunParams,
};
use crate::{
    Error, Number, TulispContext, TulispObject, TulispValue, bytecode::Pos,
    object::wrappers::generic::SharedMut, plist,
};
use std::collections::HashMap;

/// A compiled function to run on arguments already on the stack,
/// with how many of them fill optional and rest parameters.
struct TailCallInfo {
    function: CompiledDefun,
    optional_count: usize,
    rest_count: usize,
}

/// Coerce both operands to `Number` and apply a fallible `op` (one
/// that surfaces overflow as `Err`). `as_number` already attaches
/// the operand to the trace on type-mismatch.
#[inline(always)]
fn binary_op_checked(
    a: &TulispObject,
    b: &TulispObject,
    op: impl FnOnce(Number, Number) -> Result<Number, Error>,
) -> Result<TulispObject, Error> {
    let a = a.as_number()?;
    let b = b.as_number()?;
    op(a, b).map(Into::into)
}

/// Coerce both operands to `Number` and apply `cmp`.
#[inline(always)]
fn compare_op(
    a: &TulispObject,
    b: &TulispObject,
    cmp: impl FnOnce(Number, Number) -> bool,
) -> Result<bool, Error> {
    let a = a.as_number()?;
    let b = b.as_number()?;
    Ok(cmp(a, b))
}

struct SetParams(Vec<TulispObject>);

impl SetParams {
    fn new() -> Self {
        Self(Vec::new())
    }

    fn push(&mut self, obj: TulispObject) {
        self.0.push(obj);
    }
}

impl Drop for SetParams {
    fn drop(&mut self) {
        // Drop runs on every call return, including the error-unwind
        // path. `unset` errors are unreachable in practice (every entry
        // came from a successful `set_scope` in `init_defun_args`), but
        // a panic here while another error is propagating would
        // double-fault and abort the process — silently swallow the
        // error like `LexScopeGuard::drop` in `eval.rs`.
        for obj in self.0.iter() {
            let _ = obj.unset();
        }
    }
}

/// Per-frame Drop guard for `BeginScope` bindings — `let` / `let*` / inline
/// `lambda` body bindings. Mirrors `SetParams` (function params) and
/// `LexScopeGuard` (TW path).
///
/// On clean execution every `BeginScope` is matched by an `EndScope`,
/// which removes the entry from the guard, so `Drop` finds the Vec
/// empty. On error escape, `?` propagates out of `run_impl_inner`
/// before the trailing `EndScope`s run; `Drop` then unsets whatever
/// is still pending. Without this guard, `(let ((y 5)) (error …))`
/// inside a defun leaked one `LEX_STACKS` entry per call (and the
/// defvar variant leaked onto `SymbolBindings::items`); a 1000-call
/// loop in a persistent `TulispContext` was visibly bleeding memory.
struct ActiveScopes(Vec<TulispObject>);

impl ActiveScopes {
    fn new() -> Self {
        Self(Vec::new())
    }

    fn enter(&mut self, obj: TulispObject) {
        self.0.push(obj);
    }

    fn exit(&mut self, obj: &TulispObject) {
        // EndScopes can fire in non-LIFO order — `compile_fn_let_star`
        // emits them in declaration order, not reverse — so scan by
        // identity rather than blindly popping the tail.
        if let Some(pos) = self.0.iter().rposition(|x| x.eq_ptr(obj)) {
            self.0.remove(pos);
        }
    }
}

impl Drop for ActiveScopes {
    fn drop(&mut self) {
        for obj in self.0.iter().rev() {
            let _ = obj.unset();
        }
    }
}

pub struct Machine {
    stack: Vec<TulispObject>,
    functions: HashMap<usize, CompiledDefun>, // key: fn_name.addr_as_usize()
}

/// Pops two operands and gives whether `$cmp` holds for them. `$b` is
/// the top of the stack and `$a` the one below it, so `$a` is the
/// first argument, evaluated first. The operands are dropped before an
/// error from `$cmp` propagates.
macro_rules! pop_compare {
    ($ctx:ident, |$a:ident, $b:ident| $cmp:expr) => {{
        let minus2 = $ctx.vm.stack.len() - 2;
        let [ref $a, ref $b] = $ctx.vm.stack[minus2..] else {
            unreachable!()
        };
        let cmp: Result<bool, Error> = $cmp;
        $ctx.vm.stack.truncate(minus2);
        cmp?
    }};
}

/// Pops two operands and pushes whether `$cmp` holds for them.
macro_rules! compare_binary {
    ($ctx:ident, |$a:ident, $b:ident| $cmp:expr) => {{
        let holds = pop_compare!($ctx, |$a, $b| $cmp);
        $ctx.vm.stack.push(holds.into());
    }};
}

/// Pops two operands and jumps when `$cmp` holds for them.
macro_rules! jump_if_binary {
    ($ctx:ident, $pc:ident, $pos:ident, |$a:ident, $b:ident| $cmp:expr) => {{
        if pop_compare!($ctx, |$a, $b| $cmp) {
            jump_to_pos!($ctx, $pc, $pos);
            continue;
        }
    }};
}

macro_rules! jump_to_pos {
    ($ctx: ident, $pc:ident, $pos:ident) => {
        $pc = {
            match $pos {
                Pos::Abs(p) => *p,
                Pos::Rel(p) => {
                    let abs_pos = ($pc as isize + *p + 1) as usize;
                    *$pos = Pos::Abs(abs_pos);
                    abs_pos
                }
                Pos::Label(_) => {
                    return Err(Error::lisp_error(
                        "internal: label jump reached the interpreter; \
                         assemble should have resolved it",
                    ));
                }
            }
        }
    };
}

impl Machine {
    pub(crate) fn new() -> Self {
        Machine {
            stack: Vec::new(),
            functions: HashMap::new(),
        }
    }

    /// The compiled copy of the `defun` that `name` holds as `function`,
    /// if this machine has one compiled from that same definition.
    pub(crate) fn compiled_copy(
        &self,
        name: &TulispObject,
        function: &TulispObject,
    ) -> Option<CompiledDefun> {
        let compiled = self.functions.get(&name.addr_as_usize())?;
        let source = compiled.source.as_ref()?;
        source.eq_ptr(function).then(|| compiled.clone())
    }
}

/// Restores the caller's VM stack height on drop, on any path
/// including a panic, so only this run's values ever sit above it.
struct RunGuard<'a> {
    ctx: &'a mut TulispContext,
    stack_base: usize,
}

impl<'a> RunGuard<'a> {
    fn new(ctx: &'a mut TulispContext) -> Self {
        let stack_base = ctx.vm.stack.len();
        RunGuard { ctx, stack_base }
    }

    /// This run's value, or nil when it left none above the caller's
    /// stack.
    fn take_value(&mut self) -> TulispObject {
        // A run leaves at most its own value; anything more is a value
        // some instruction failed to pop.
        debug_assert!(
            self.ctx.vm.stack.len() <= self.stack_base + 1,
            "a run left {} values",
            self.ctx.vm.stack.len() - self.stack_base
        );
        if self.ctx.vm.stack.len() > self.stack_base {
            self.ctx.vm.stack.pop().unwrap_or_else(TulispObject::nil)
        } else {
            TulispObject::nil()
        }
    }
}

impl Drop for RunGuard<'_> {
    fn drop(&mut self) {
        self.ctx.vm.stack.truncate(self.stack_base);
    }
}

pub fn run(ctx: &mut TulispContext, bytecode: Bytecode) -> Result<TulispObject, Error> {
    // Each symbol holds the definition of the compiled copy the machine
    // loads for it.
    for function in bytecode.functions.values() {
        if let Some(source) = &function.source {
            function.name.set_global(source.clone())?;
        }
    }
    ctx.vm.functions.extend(bytecode.functions);
    // A re-entrant run (a Rust callable evaluating a program
    // mid-run) shares the machine with its caller. The function
    // table is a namespace an inner run extends for good; the guard
    // gives back only the stack.
    let mut guard = RunGuard::new(ctx);
    let tail = run_impl(
        guard.ctx,
        &bytecode.global,
        bytecode.global_trace_ranges.as_slice(),
        false,
    )?;
    if tail.is_some() {
        return Err(Error::lisp_error(
            "internal: tail call outside a function body",
        ));
    }
    // When the top-level form has no value (e.g., a program of only
    // `defun`s), the compiler emits no trailing Push; the result is
    // nil then.
    Ok(guard.take_value())
}

/// Invoke a VM-compiled lambda with already-evaluated args. Used by
/// `eval::funcall` when it encounters a `TulispValue::CompiledDefun`,
/// by the bounce trampoline in `eval::eval_lambda`, and by the VM's
/// own `funcall` dispatch.
pub(crate) fn run_lambda(
    ctx: &mut TulispContext,
    compiled: CompiledDefun,
    args: Vec<TulispObject>,
) -> Result<TulispObject, Error> {
    let (optional_count, rest_count) = compiled.params.arity().split(args.len())?;

    // Push args in order; `init_defun_args` pops them in reverse
    // to match `params.required` + `params.optional` + `rest` layout.
    let mut guard = RunGuard::new(ctx);
    guard.ctx.vm.stack.extend(args);

    let call = TailCallInfo {
        function: compiled,
        optional_count,
        rest_count,
    };
    run_tail_calls(guard.ctx, call)?;
    // The body ended in `Ret` with exactly its value on the stack.
    debug_assert_eq!(guard.ctx.vm.stack.len(), guard.stack_base + 1);

    Ok(guard.take_value())
}

/// Runs BLOCK and returns its value. ARG, if given, is pushed first for
/// the block's first instruction to take. On an error the stack is cut
/// back to where it was, and the block's scope guard undoes its
/// bindings.
pub(crate) fn run_block(
    ctx: &mut TulispContext,
    block: &Block,
    arg: Option<TulispObject>,
) -> Result<TulispObject, Error> {
    run_block_impl(ctx, block, arg, false)
}

/// Like `run_block`, for a cleanup or handler: it, and the calls it
/// makes, may use the depth reserve (`enter_frame_with_reserve`), so it
/// still runs when its body stopped at the limit.
pub(crate) fn run_block_with_reserve(
    ctx: &mut TulispContext,
    block: &Block,
    arg: Option<TulispObject>,
) -> Result<TulispObject, Error> {
    run_block_impl(ctx, block, arg, true)
}

fn run_block_impl(
    ctx: &mut TulispContext,
    block: &Block,
    arg: Option<TulispObject>,
    reserve: bool,
) -> Result<TulispObject, Error> {
    debug_assert_eq!(block.takes_arg, arg.is_some(), "a block's argument");
    let mut guard = RunGuard::new(ctx);
    guard.ctx.vm.stack.extend(arg);
    let tail = run_impl(
        guard.ctx,
        &block.instructions,
        block.trace_ranges.as_slice(),
        reserve,
    )?;
    if tail.is_some() {
        return Err(Error::lisp_error(
            "internal: tail call inside a protected body",
        ));
    }
    Ok(guard.take_value())
}

/// Runs the first of HANDLERS whose condition matches ERR, with the
/// error data pushed for it when BINDS, or gives back ERR when none
/// matches.
fn run_handler(
    ctx: &mut TulispContext,
    binds: bool,
    handlers: &[Handler],
    err: Error,
) -> Result<TulispObject, Error> {
    use crate::builtin::functions::errors::{condition_matches, error_data, error_symbol};
    let Some(kind_sym) = error_symbol(&err) else {
        return Err(err);
    };
    for handler in handlers {
        if condition_matches(&handler.condition, kind_sym)? {
            let data = binds.then(|| error_data(ctx, kind_sym, &err));
            return run_block_with_reserve(ctx, &handler.body, data);
        }
    }
    Err(err)
}

/// Wrapper around `run_impl_inner` that applies form-trace
/// information from the bytecode's side-table on the error
/// path only. The happy path runs the inner loop with no extra
/// bookkeeping. When the inner loop returns `Err`, every range
/// in `trace_ranges` whose `[start_pc, end_pc)` contains the
/// failing PC contributes a `with_trace(form)` call (innermost
/// first), reproducing TW's recursive `eval_basic` shape.
///
/// `Error::with_trace` already de-duplicates same-form entries,
/// so an inner call instruction whose handler attached its own
/// `with_trace(form)` is collapsed against the matching range.
fn run_impl(
    ctx: &mut TulispContext,
    program: &SharedMut<Vec<Instruction>>,
    trace_ranges: &[TraceRange],
    reserve: bool,
) -> Result<Option<TailCallInfo>, Error> {
    let mut pc: usize = 0;
    // Each nested (non-tail) call re-enters `run_impl`, while the
    // tail-call loops re-enter at a constant depth, so this counts
    // real stack growth.
    let result = {
        let mut guard = if reserve {
            ctx.enter_frame_with_reserve()?
        } else {
            ctx.enter_frame()?
        };
        run_impl_inner(&mut guard, program, &mut pc)
    };
    match result {
        Ok(v) => Ok(v),
        Err(mut e) => {
            // `assemble` pushes ranges as it encounters each
            // closing `PopTrace`, so the vector is sorted
            // innermost-first. Walking forward applies the
            // innermost form first, matching TW's recursive
            // `eval_basic` shape (the innermost wrapper runs
            // closest to the failure). Inner-first also lets
            // `Error::with_trace`'s last-entry dedup collapse
            // duplicates with whatever the inner call
            // instruction's own `with_trace(form)` already
            // attached.
            for range in trace_ranges.iter() {
                if range.start_pc <= pc && pc < range.end_pc {
                    e = e.with_trace(range.form.clone());
                }
            }
            Err(e)
        }
    }
}

fn run_impl_inner(
    ctx: &mut TulispContext,
    program: &SharedMut<Vec<Instruction>>,
    pc_out: &mut usize,
) -> Result<Option<TailCallInfo>, Error> {
    let mut pc: usize = 0;
    let program_size = program.borrow().len();
    let mut instr_ref = program.borrow_mut();
    let mut active = ActiveScopes::new();
    while pc < program_size {
        // Mirror the loop's `pc` into the caller's pc_out so
        // that `run_impl` knows which instruction was active
        // when an error propagates out via `?`. One usize write
        // per dispatch — the trace ranges themselves cost
        // nothing on the happy path.
        *pc_out = pc;

        let instr = &mut instr_ref[pc];
        match instr {
            Instruction::Push(obj) => ctx.vm.stack.push(obj.clone()),
            Instruction::Pop => {
                ctx.vm.stack.pop();
            }
            Instruction::BinaryOp(op) => {
                let [ref b, ref a] = ctx.vm.stack[(ctx.vm.stack.len() - 2)..] else {
                    unreachable!()
                };

                use crate::bytecode::instruction::BinaryOp;
                let vv = match op {
                    BinaryOp::Add => binary_op_checked(a, b, Number::checked_add)?,
                    BinaryOp::Sub => binary_op_checked(a, b, Number::checked_sub)?,
                    BinaryOp::Mul => binary_op_checked(a, b, Number::checked_mul)?,
                    // Errors on an integer zero divisor and on
                    // `i64::MIN / -1` overflow; a float operand yields
                    // ±inf for a zero divisor, matching Emacs'
                    // `1.0e+INF` shape.
                    BinaryOp::Div => binary_op_checked(a, b, Number::checked_div)?,
                };
                ctx.vm.stack.truncate(ctx.vm.stack.len() - 2);
                ctx.vm.stack.push(vv);
            }
            Instruction::LoadFile => {
                let filename = ctx.vm.stack.pop().unwrap();
                let filename = filename
                    .as_string()
                    .map_err(|err| err.with_trace(filename))?;
                let full_path = if let Some(ref load_path) = ctx.load_path {
                    load_path.join(&filename)
                } else {
                    std::path::PathBuf::from(&filename)
                };
                let full_path = full_path.to_str().ok_or_else(|| {
                    Error::invalid_argument(format!(
                        "load: Invalid path: {}",
                        full_path.to_string_lossy()
                    ))
                })?;
                // `(load …)` from VM-compiled code compiles the
                // loaded file through the VM as well — so defuns
                // in the loaded file register in `ctx.vm.functions`
                // and subsequent calls dispatch directly via the
                // `Call` instruction (same as if they had been
                // written in the outer file).
                drop(instr_ref);
                let result = ctx.eval_file(full_path)?;
                instr_ref = program.borrow_mut();
                ctx.vm.stack.push(result);
            }
            Instruction::PrintPop => {
                let a = ctx.vm.stack.pop().unwrap();
                crate::builtin::functions::print_to_stdout(&a.fmt_string(), true)?;
            }
            Instruction::Print => {
                let a = ctx.vm.stack.last().unwrap();
                crate::builtin::functions::print_to_stdout(&a.fmt_string(), true)?;
            }
            Instruction::JumpIfNil(pos) => {
                let a = ctx.vm.stack.last().unwrap();
                let cmp = a.null();
                ctx.vm.stack.truncate(ctx.vm.stack.len() - 1);
                if cmp {
                    jump_to_pos!(ctx, pc, pos);
                    continue;
                }
            }
            Instruction::JumpIfNotNil(pos) => {
                let a = ctx.vm.stack.last().unwrap();
                let cmp = !a.null();
                ctx.vm.stack.truncate(ctx.vm.stack.len() - 1);
                if cmp {
                    jump_to_pos!(ctx, pc, pos);
                    continue;
                }
            }
            Instruction::JumpIfNilElsePop(pos) => {
                let a = ctx.vm.stack.last().unwrap();
                if a.null() {
                    jump_to_pos!(ctx, pc, pos);
                    continue;
                } else {
                    ctx.vm.stack.truncate(ctx.vm.stack.len() - 1);
                }
            }
            Instruction::JumpIfNotNilElsePop(pos) => {
                let a = ctx.vm.stack.last().unwrap();
                if !a.null() {
                    jump_to_pos!(ctx, pc, pos);
                    continue;
                } else {
                    ctx.vm.stack.truncate(ctx.vm.stack.len() - 1);
                }
            }
            Instruction::JumpIfNeq(pos) => jump_if_binary!(ctx, pc, pos, |a, b| Ok(!a.eq(b))),
            Instruction::JumpIfEq(pos) => jump_if_binary!(ctx, pc, pos, |a, b| Ok(a.eq(b))),
            Instruction::JumpIfEqual(pos) => jump_if_binary!(ctx, pc, pos, |a, b| Ok(a.equal(b))),
            Instruction::JumpIfNotEqual(pos) => {
                jump_if_binary!(ctx, pc, pos, |a, b| Ok(!a.equal(b)))
            }
            Instruction::JumpIfLt(pos) => {
                jump_if_binary!(ctx, pc, pos, |a, b| compare_op(a, b, |a, b| a < b))
            }
            Instruction::JumpIfLtEq(pos) => {
                jump_if_binary!(ctx, pc, pos, |a, b| compare_op(a, b, |a, b| a <= b))
            }
            Instruction::JumpIfGt(pos) => {
                jump_if_binary!(ctx, pc, pos, |a, b| compare_op(a, b, |a, b| a > b))
            }
            Instruction::JumpIfGtEq(pos) => {
                jump_if_binary!(ctx, pc, pos, |a, b| compare_op(a, b, |a, b| a >= b))
            }
            Instruction::JumpIfNotLt(pos) => {
                jump_if_binary!(ctx, pc, pos, |a, b| compare_op(a, b, |a, b| a < b)
                    .map(|holds| !holds))
            }
            Instruction::JumpIfNotLtEq(pos) => {
                jump_if_binary!(ctx, pc, pos, |a, b| compare_op(a, b, |a, b| a <= b)
                    .map(|holds| !holds))
            }
            Instruction::JumpIfNotGt(pos) => {
                jump_if_binary!(ctx, pc, pos, |a, b| compare_op(a, b, |a, b| a > b)
                    .map(|holds| !holds))
            }
            Instruction::JumpIfNotGtEq(pos) => {
                jump_if_binary!(ctx, pc, pos, |a, b| compare_op(a, b, |a, b| a >= b)
                    .map(|holds| !holds))
            }
            Instruction::CompareChain { comparison, count } => {
                let comparison = *comparison;
                let base = ctx.vm.stack.len() - *count;
                let mut holds = Ok(true);
                for pair in ctx.vm.stack[base..].windows(2) {
                    holds = compare_op(&pair[0], &pair[1], |a, b| comparison.holds(a, b));
                    if !matches!(holds, Ok(true)) {
                        break;
                    }
                }
                // Pop the arguments before `?` so an error leaves the
                // stack balanced.
                ctx.vm.stack.truncate(base);
                ctx.vm.stack.push(holds?.into());
            }
            Instruction::Jump(pos) => {
                jump_to_pos!(ctx, pc, pos);
                continue;
            }
            Instruction::Equal => compare_binary!(ctx, |a, b| Ok(a.equal(b))),
            Instruction::Eq => compare_binary!(ctx, |a, b| Ok(a.eq(b))),
            Instruction::Lt => compare_binary!(ctx, |a, b| compare_op(a, b, |a, b| a < b)),
            Instruction::LtEq => compare_binary!(ctx, |a, b| compare_op(a, b, |a, b| a <= b)),
            Instruction::Gt => compare_binary!(ctx, |a, b| compare_op(a, b, |a, b| a > b)),
            Instruction::GtEq => compare_binary!(ctx, |a, b| compare_op(a, b, |a, b| a >= b)),
            Instruction::Set => {
                let minus2 = ctx.vm.stack.len() - 2;
                let [ref value, ref variable] = ctx.vm.stack[minus2..] else {
                    unreachable!()
                };
                variable.set(value.clone())?;
                // remove just the variable from the stack, keep the value
                ctx.vm.stack.truncate(ctx.vm.stack.len() - 1);
            }
            Instruction::SetPop => {
                let minus2 = ctx.vm.stack.len() - 2;
                let [ref value, ref variable] = ctx.vm.stack[minus2..] else {
                    unreachable!()
                };
                variable.set(value.clone())?;
                // remove both variable and value from stack.
                ctx.vm.stack.truncate(minus2);
            }
            Instruction::StorePop(obj) => {
                let a = ctx.vm.stack.pop().unwrap();
                obj.set(a)?;
            }
            Instruction::Store(obj) => {
                let a = ctx.vm.stack.last().unwrap();
                obj.set(a.clone())?;
            }
            Instruction::Load(obj) => {
                let a = obj.get().map_err(|e| e.with_trace(obj.clone()))?;
                ctx.vm.stack.push(a);
            }
            Instruction::BeginScope(obj) => {
                let a = ctx.vm.stack.last().unwrap();
                obj.set_scope(a.clone())?;
                active.enter(obj.clone());
                ctx.vm.stack.truncate(ctx.vm.stack.len() - 1);
            }
            Instruction::EndScope(obj) => {
                obj.unset()?;
                active.exit(obj);
            }
            Instruction::Call {
                name,
                form,
                function,
                args_count,
                optional_count,
                rest_count,
            } => {
                if function.is_none() {
                    let addr = name.addr_as_usize();
                    if let Some(func) = ctx.vm.functions.get(&addr) {
                        let func = func.clone();
                        (*optional_count, *rest_count) = func
                            .params
                            .arity()
                            .split(*args_count)
                            .map_err(|e| e.with_trace(form.clone()))?;
                        *function = Some(func);
                    } else {
                        // Target isn't a VM-compiled defun. It might
                        // be a TW `Lambda` (e.g., defined by a file
                        // loaded via `(load …)` → TW `eval_file`),
                        // a `Func` defspecial, or a variable holding
                        // a compiled closure. Fall back to the same
                        // dispatch the inline `Funcall` uses.
                        let args_count = *args_count;
                        let split_at = ctx.vm.stack.len() - args_count;
                        let args: Vec<TulispObject> = ctx.vm.stack.drain(split_at..).collect();
                        let name = name.clone();
                        let form = form.clone();
                        drop(instr_ref);
                        let result =
                            funcall_inline(ctx, &name, args).map_err(|e| e.with_trace(form))?;
                        ctx.vm.stack.push(result);
                        instr_ref = program.borrow_mut();
                        pc += 1;
                        continue;
                    }
                }

                let call = TailCallInfo {
                    function: function.as_ref().unwrap().clone(),
                    optional_count: *optional_count,
                    rest_count: *rest_count,
                };
                let form = form.clone();

                drop(instr_ref);
                run_tail_calls(ctx, call).map_err(|e| e.with_trace(form))?;
                instr_ref = program.borrow_mut();
            }
            Instruction::TailCall {
                name,
                form,
                function,
                args_count,
                optional_count,
                rest_count,
            } => {
                if function.is_none() {
                    let addr = name.addr_as_usize();
                    let Some(func) = ctx.vm.functions.get(&addr) else {
                        return Err(Error::new(
                            crate::ErrorKind::Undefined,
                            format!("undefined function: {}", name),
                        )
                        .with_trace(form.clone()));
                    };
                    let func = func.clone();
                    (*optional_count, *rest_count) = func
                        .params
                        .arity()
                        .split(*args_count)
                        .map_err(|e| e.with_trace(form.clone()))?;
                    *function = Some(func);
                }

                let info = TailCallInfo {
                    function: function.as_ref().unwrap().clone(),
                    optional_count: *optional_count,
                    rest_count: *rest_count,
                };
                return Ok(Some(info));
            }
            Instruction::Ret => return Ok(None),
            Instruction::MakeLambda(template) => {
                let closure = make_lambda_from_template(ctx, template)?;
                ctx.vm.stack.push(closure);
            }
            Instruction::Catch { body } => {
                let tag = ctx.vm.stack.pop().unwrap();
                // Release the program: a recursive function re-enters it.
                let body = body.clone();
                drop(instr_ref);
                let result = run_block(ctx, &body, None)
                    .or_else(|err| crate::builtin::functions::errors::catch_throw(err, &tag));
                instr_ref = program.borrow_mut();
                ctx.vm.stack.push(result?);
            }
            Instruction::UnwindProtect { body, cleanup } => {
                let (body, cleanup) = (body.clone(), cleanup.clone());
                drop(instr_ref);
                let result = run_block(ctx, &body, None);
                let cleaned = run_block_with_reserve(ctx, &cleanup, None);
                instr_ref = program.borrow_mut();
                ctx.vm.stack.push(cleaned.and(result)?);
            }
            Instruction::Raise(err) => return Err((**err).clone()),
            Instruction::ConditionCase {
                binds,
                body,
                handlers,
            } => {
                let (binds, body, handlers) = (*binds, body.clone(), handlers.clone());
                drop(instr_ref);
                let result = match run_block(ctx, &body, None) {
                    Err(err) => run_handler(ctx, binds, &handlers, err),
                    value => value,
                };
                instr_ref = program.borrow_mut();
                ctx.vm.stack.push(result?);
            }
            Instruction::Funcall { args_count } => {
                let args_count = *args_count;
                let split_at = ctx.vm.stack.len() - args_count;
                let args: Vec<TulispObject> = ctx.vm.stack.drain(split_at..).collect();
                let func = ctx.vm.stack.pop().unwrap();
                drop(instr_ref);
                let result = funcall_inline(ctx, &func, args)?;
                ctx.vm.stack.push(result);
                instr_ref = program.borrow_mut();
            }
            Instruction::Apply { args_count } => {
                let args_count = *args_count;
                // Stack layout: [..., FN, intermediate_0..N-1, FINAL_LIST]
                let final_list = ctx.vm.stack.pop().unwrap();
                if !final_list.listp() {
                    return Err(Error::type_mismatch(format!(
                        "apply: last argument must be a list, got: {final_list}"
                    )));
                }
                let split_at = ctx.vm.stack.len() - args_count;
                let mut args: Vec<TulispObject> = ctx.vm.stack.drain(split_at..).collect();
                let func = ctx.vm.stack.pop().unwrap();
                let mut items = final_list.base_iter();
                args.extend(items.by_ref());
                let tail = items.tail()?;
                if !tail.null() {
                    return Err(Error::type_mismatch(format!(
                        "apply: last argument must be a proper list, got non-nil tail: {tail}"
                    )));
                }
                drop(instr_ref);
                let result = funcall_inline(ctx, &func, args)?;
                ctx.vm.stack.push(result);
                instr_ref = program.borrow_mut();
            }
            Instruction::RustCall {
                form,
                func,
                keep_result,
                ..
            } => {
                let args = ctx.vm.stack.pop().unwrap();
                // Clone what the call needs and release the program
                // borrow: the host callable may re-enter the
                // interpreter (e.g. a defspecial that calls
                // `ctx.eval_string`), which re-borrows this
                // function's instruction list.
                let form = form.clone();
                let func = func.clone();
                let keep_result = *keep_result;
                drop(instr_ref);
                let result = func(ctx, &args).map_err(|e| e.with_trace(form))?;
                instr_ref = program.borrow_mut();
                if keep_result {
                    ctx.vm.stack.push(result);
                }
            }
            Instruction::RustCallTyped {
                form,
                call,
                args_count,
                keep_result,
                ..
            } => {
                let args_count = *args_count;
                let split_at = ctx.vm.stack.len() - args_count;
                let args: Vec<TulispObject> = ctx.vm.stack.drain(split_at..).collect();
                // Same re-entry discipline as `RustCall` above: the
                // typed closure also receives `ctx` and may evaluate.
                let form = form.clone();
                let call = call.clone();
                let keep_result = *keep_result;
                drop(instr_ref);
                let result = call(ctx, &args).map_err(|e| e.with_trace(form))?;
                instr_ref = program.borrow_mut();
                if keep_result {
                    ctx.vm.stack.push(result);
                }
            }
            // Trace markers and labels never reach the interpreter:
            // `assemble` removes them at compile time, lifting the
            // form spans into a `TraceRange` side-table consulted by
            // `run_impl` on the error path. One that gets here is a
            // compiler bug; report it instead of silently skipping
            // it.
            Instruction::PushTrace(_) | Instruction::PopTrace | Instruction::Label(_) => {
                return Err(Error::lisp_error(
                    "internal: compile-time marker reached the interpreter; \
                     assemble should have removed it",
                ));
            }
            Instruction::Cons => {
                let b = ctx.vm.stack.pop().unwrap();
                let a = ctx.vm.stack.pop().unwrap();
                ctx.vm.stack.push(TulispObject::cons(a, b));
            }
            Instruction::List(len) => {
                let mut list = TulispObject::nil();
                for _ in 0..*len {
                    let a = ctx.vm.stack.pop().unwrap();
                    list = TulispObject::cons(a, list);
                }
                ctx.vm.stack.push(list);
            }
            Instruction::Append(len) => {
                let start = ctx.vm.stack.len() - *len;
                let result = crate::lists::append(ctx.vm.stack.drain(start..))?;
                ctx.vm.stack.push(result);
            }
            Instruction::Cxr(cxr) => {
                let a: TulispObject = ctx.vm.stack.pop().unwrap();

                use crate::bytecode::instruction::Cxr;
                let result = match cxr {
                    Cxr::Car => a.car()?,
                    Cxr::Cdr => a.cdr()?,
                    Cxr::Caar => a.caar()?,
                    Cxr::Cadr => a.cadr()?,
                    Cxr::Cdar => a.cdar()?,
                    Cxr::Cddr => a.cddr()?,
                    Cxr::Caaar => a.caaar()?,
                    Cxr::Caadr => a.caadr()?,
                    Cxr::Cadar => a.cadar()?,
                    Cxr::Caddr => a.caddr()?,
                    Cxr::Cdaar => a.cdaar()?,
                    Cxr::Cdadr => a.cdadr()?,
                    Cxr::Cddar => a.cddar()?,
                    Cxr::Cdddr => a.cdddr()?,
                    Cxr::Caaaar => a.caaaar()?,
                    Cxr::Caaadr => a.caaadr()?,
                    Cxr::Caadar => a.caadar()?,
                    Cxr::Caaddr => a.caaddr()?,
                    Cxr::Cadaar => a.cadaar()?,
                    Cxr::Cadadr => a.cadadr()?,
                    Cxr::Caddar => a.caddar()?,
                    Cxr::Cadddr => a.cadddr()?,
                    Cxr::Cdaaar => a.cdaaar()?,
                    Cxr::Cdaadr => a.cdaadr()?,
                    Cxr::Cdadar => a.cdadar()?,
                    Cxr::Cdaddr => a.cdaddr()?,
                    Cxr::Cddaar => a.cddaar()?,
                    Cxr::Cddadr => a.cddadr()?,
                    Cxr::Cdddar => a.cdddar()?,
                    Cxr::Cddddr => a.cddddr()?,
                };
                ctx.vm.stack.push(result);
            }
            Instruction::PlistGet => {
                let [ref key, ref plist] = ctx.vm.stack[(ctx.vm.stack.len() - 2)..] else {
                    unreachable!()
                };
                let value = plist::plist_get(plist, key)?;
                ctx.vm.stack.truncate(ctx.vm.stack.len() - 2);
                ctx.vm.stack.push(value);
            }
            // predicates
            Instruction::Null => {
                let a = ctx.vm.stack.last().unwrap().null();
                *ctx.vm.stack.last_mut().unwrap() = a.into();
            }
            Instruction::Quote => {
                let a = ctx.vm.stack.pop().unwrap();
                ctx.vm
                    .stack
                    .push(TulispValue::Quote { value: a }.into_ref(None));
            }
            Instruction::WrapBackquote => {
                let a = ctx.vm.stack.pop().unwrap();
                ctx.vm
                    .stack
                    .push(TulispValue::Backquote { value: a }.into_ref(None));
            }
            Instruction::WrapUnquote => {
                let a = ctx.vm.stack.pop().unwrap();
                ctx.vm
                    .stack
                    .push(TulispValue::Unquote { value: a }.into_ref(None));
            }
            Instruction::WrapSplice => {
                let a = ctx.vm.stack.pop().unwrap();
                ctx.vm
                    .stack
                    .push(TulispValue::Splice { value: a }.into_ref(None));
            }
            Instruction::SoleElement { empty_is_nil } => {
                let a = ctx.vm.stack.pop().unwrap();
                ctx.vm
                    .stack
                    .push(crate::lists::sole_element(&a, *empty_is_nil)?);
            }
        }
        pc += 1;
    }
    Ok(None)
}

fn init_defun_args(ctx: &mut TulispContext, call: &TailCallInfo) -> Result<SetParams, Error> {
    let params = &call.function.params;
    let mut set_params = SetParams::new();
    if let Some(rest) = &params.rest {
        let mut rest_value = TulispObject::nil();
        for _ in 0..call.rest_count {
            rest_value = TulispObject::cons(ctx.vm.stack.pop().unwrap(), rest_value);
        }
        rest.set_scope(rest_value)?;
        set_params.push(rest.clone());
    }
    for (ii, arg) in params.optional.iter().enumerate().rev() {
        // Every `set_scope` must pair with a `set_params.push` so
        // `SetParams::drop` unsets it on return — including the
        // missing-optional case, where the previous `continue` skipped
        // the push and leaked the nil binding onto `LEX_STACKS` once
        // per call.
        let val = if ii >= call.optional_count {
            TulispObject::nil()
        } else {
            ctx.vm.stack.pop().unwrap()
        };
        arg.set_scope(val)?;
        set_params.push(arg.clone());
    }
    for arg in params.required.iter().rev() {
        arg.set_scope(ctx.vm.stack.pop().unwrap())?;
        set_params.push(arg.clone());
    }
    Ok(set_params)
}

/// Runs `call`'s function on arguments already on the stack and
/// follows its tail calls until one returns a value, so a chain of
/// tail calls costs no native stack.
fn run_tail_calls(ctx: &mut TulispContext, mut call: TailCallInfo) -> Result<(), Error> {
    loop {
        let params = init_defun_args(ctx, &call)?;
        let tail = run_impl(
            ctx,
            &call.function.instructions,
            call.function.trace_ranges.as_slice(),
            false,
        )?;
        drop(params);
        match tail {
            Some(next) => call = next,
            None => return Ok(()),
        }
    }
}

/// In-VM `funcall` dispatch used by `Instruction::Funcall`. Args are
/// already fully evaluated, so going through `eval::funcall` would
/// only bounce out of the dispatch loop and re-enter the interpreter
/// for a form we can dispatch right here. Instead we dispatch each
/// callable variant on the machine we already have.
fn funcall_inline(
    ctx: &mut TulispContext,
    func: &TulispObject,
    args: Vec<TulispObject>,
) -> Result<TulispObject, Error> {
    // `(funcall 'funcall fn …)` — unwrap the redundant outer
    // `funcall`. If we didn't, the symbol would eval to the
    // `funcall` defspecial `Func` and we'd fall through to the
    // Lambda/Func arm below, which hands control back to
    // `eval::funcall` — a needless bounce out of the VM for a
    // call we can dispatch right here. Peel one layer: the first
    // arg is the new func, the rest are its args.
    if func.eq(&ctx.keywords.funcall) && !args.is_empty() {
        let mut args = args;
        let inner_func = args.remove(0);
        return funcall_inline(ctx, &inner_func, args);
    }
    let resolved = crate::eval::resolve_function(ctx, func)?;
    let inner = resolved.inner_ref();
    match &inner.0 {
        TulispValue::CompiledDefun { value } => {
            let cd = value.clone();
            drop(inner);
            run_lambda(ctx, cd, args)
        }
        TulispValue::Defun { call, arity } => {
            // Args are already evaluated values from the VM stack
            // — hand them straight to the typed-args closure.
            // No interpreter re-entry: we're using the closure's
            // `&[TulispObject]` shape directly.
            let call = call.clone();
            let arity = arity.clone();
            drop(inner);
            arity.check(args.len())?;
            call(ctx, &args)
        }
        TulispValue::Lambda { .. } | TulispValue::Func(_) => {
            drop(inner);
            // Rebuild an arg list TulispObject (quoted so the TW
            // side doesn't re-evaluate already-resolved values).
            let mut list = crate::cons::ListBuilder::new();
            for a in args {
                list.push(TulispValue::Quote { value: a }.into_ref(None));
            }
            crate::eval::funcall::<crate::eval::Eval>(ctx, &resolved, &list.build())
        }
        _ => Err(Error::undefined(format!("function is void: {}", resolved))),
    }
}

/// Extracts the original symbol from a placeholder LexicalBinding; if
/// `obj` isn't a LexicalBinding (shouldn't happen for our placeholders)
/// it's returned as-is.
fn placeholder_symbol(obj: &TulispObject) -> TulispObject {
    let inner = obj.inner_ref();
    if let TulispValue::LexicalBinding { binding } = &inner.0 {
        let s = binding.symbol().clone();
        drop(inner);
        s
    } else {
        drop(inner);
        obj.clone()
    }
}

/// Phase 2 of the two-phase lambda compile: given a `LambdaTemplate`
/// and the current evaluation context, (a) capture each free var's
/// slot, (b) mint fresh LexicalBindings for the params, (c) clone the
/// template's instruction vector and rewrite placeholder references to
/// those new bindings, then (d) wrap the result in a
/// `TulispValue::CompiledDefun` so `funcall` can dispatch it to the VM.
fn make_lambda_from_template(
    ctx: &mut TulispContext,
    template: &LambdaTemplate,
) -> Result<TulispObject, Error> {
    let allocator = ctx.lex_allocator.clone();
    let mut mapping: HashMap<usize, TulispObject> =
        HashMap::with_capacity(template.param_placeholders.len() + template.free_vars.len());

    // Free vars: share the enclosing scope's slot if there is one,
    // otherwise fall back to the original symbol (global/dynamic
    // reference — no capture).
    for (orig, placeholder) in &template.free_vars {
        let slot = {
            let inner = orig.inner_ref();
            match &inner.0 {
                TulispValue::LexicalBinding { binding } => binding.current_slot(),
                _ => None,
            }
        };
        let replacement = if let Some(slot) = slot {
            TulispObject::lexical_binding_captured(allocator.clone(), orig.clone(), slot)
        } else {
            orig.clone()
        };
        mapping.insert(placeholder.addr_as_usize(), replacement);
    }

    // Params: each gets a fresh LexicalBinding; at call time, arg
    // values are pushed onto the binding's thread-local stack.
    for placeholder in &template.param_placeholders {
        let sym = placeholder_symbol(placeholder);
        let fresh = TulispObject::lexical_binding(allocator.clone(), sym);
        mapping.insert(placeholder.addr_as_usize(), fresh);
    }

    let rewrite = |obj: &mut TulispObject| {
        if let Some(replacement) = mapping.get(&obj.addr_as_usize()) {
            *obj = replacement.clone();
        }
    };

    let instructions = rewrite_instructions(&template.instructions, &mapping, &rewrite)?;

    let rewrite_obj = |obj: &TulispObject| -> TulispObject {
        mapping
            .get(&obj.addr_as_usize())
            .cloned()
            .unwrap_or_else(|| obj.clone())
    };
    let params = VMDefunParams {
        required: template.params.required.iter().map(&rewrite_obj).collect(),
        optional: template.params.optional.iter().map(&rewrite_obj).collect(),
        rest: template.params.rest.as_ref().map(&rewrite_obj),
    };

    let cd = CompiledDefun {
        name: TulispObject::nil(),
        source: None,
        instructions: SharedMut::new(instructions),
        // PCs are unchanged by `rewrite_instruction` (it only
        // swaps placeholder objects, never adds or removes
        // instructions), so the template's trace ranges remain
        // valid for the materialized closure. Wrap in a `Shared`
        // since `LambdaTemplate::trace_ranges` is owned.
        trace_ranges: crate::object::wrappers::generic::Shared::new(template.trace_ranges.clone()),
        params: crate::object::wrappers::generic::Shared::new(params),
    };
    Ok(TulispValue::CompiledDefun { value: cd }.into_ref(None))
}

/// Apply the outer closure's placeholder→binding map to a single
/// instruction. For nested `MakeLambda`, descend and produce a rebuilt
/// `LambdaTemplate` so the inner template's free-var keys become the
/// outer's fresh bindings (so when the inner later runs its own phase
/// 2 it finds the right slots).
fn rewrite_instruction(
    insn: &mut Instruction,
    mapping: &HashMap<usize, TulispObject>,
    rewrite: &impl Fn(&mut TulispObject),
) -> Result<(), Error> {
    match insn {
        Instruction::Load(o)
        | Instruction::Store(o)
        | Instruction::StorePop(o)
        | Instruction::BeginScope(o)
        | Instruction::EndScope(o) => rewrite(o),
        Instruction::Push(o)
            // `Push` can carry an AST subtree — notably the args list
            // of a `RustCall`-dispatched defun call — that may contain
            // placeholder LexicalBinding references inside cons cells.
            // Walk the subtree and materialize a fresh copy with
            // placeholders substituted; the original AST is shared
            // across all closures from this template, so we must not
            // mutate in place.
            if ast_contains_placeholder(o, mapping) => {
                *o = rewrite_ast(o, mapping)?;
            }
        Instruction::MakeLambda(template) => {
            let rebuilt = rewrite_template(template, mapping)?;
            *template = crate::object::wrappers::generic::Shared::new(rebuilt);
        }
        Instruction::Catch { body } => *body = rewrite_block(body, mapping, rewrite)?,
        Instruction::UnwindProtect { body, cleanup } => {
            *body = rewrite_block(body, mapping, rewrite)?;
            *cleanup = rewrite_block(cleanup, mapping, rewrite)?;
        }
        Instruction::ConditionCase { body, handlers, .. } => {
            *body = rewrite_block(body, mapping, rewrite)?;
            let mut rewritten = Vec::with_capacity(handlers.len());
            for handler in handlers.iter() {
                rewritten.push(Handler {
                    condition: handler.condition.clone(),
                    body: rewrite_block(&handler.body, mapping, rewrite)?,
                });
            }
            *handlers = crate::object::wrappers::generic::Shared::new(rewritten);
        }
        _ => debug_assert!(!insn.holds_blocks(), "a block {insn} holds is not rewritten"),
    }
    Ok(())
}

/// A copy of BLOCK with the closure's bindings swapped in. The block's
/// handles are shared with the template, so it is copied, never edited
/// in place: an in-place edit would give every later closure the first
/// closure's bindings.
fn rewrite_block(
    block: &Block,
    mapping: &HashMap<usize, TulispObject>,
    rewrite: &impl Fn(&mut TulispObject),
) -> Result<Block, Error> {
    Ok(Block {
        instructions: SharedMut::new(rewrite_instructions(
            &block.instructions.borrow(),
            mapping,
            rewrite,
        )?),
        trace_ranges: block.trace_ranges.clone(),
        takes_arg: block.takes_arg,
    })
}

/// A copy of INSTRUCTIONS with the closure's bindings swapped in.
fn rewrite_instructions(
    instructions: &[Instruction],
    mapping: &HashMap<usize, TulispObject>,
    rewrite: &impl Fn(&mut TulispObject),
) -> Result<Vec<Instruction>, Error> {
    let mut rewritten = instructions.to_vec();
    for insn in rewritten.iter_mut() {
        rewrite_instruction(insn, mapping, rewrite)?;
    }
    Ok(rewritten)
}

/// Quick check: does `obj` or any cons-cell descendant reference a
/// placeholder? Avoids the expensive deep-copy when Push holds plain
/// literals (numbers, strings, etc.).
fn ast_contains_placeholder(obj: &TulispObject, mapping: &HashMap<usize, TulispObject>) -> bool {
    if mapping.contains_key(&obj.addr_as_usize()) {
        return true;
    }
    if obj.consp() {
        let mut items = obj.base_iter();
        for car in items.by_ref() {
            if ast_contains_placeholder(&car, mapping) {
                return true;
            }
        }
        // A list that loops back has had all of its cells walked by
        // the time the iterator notices, so its error means `false`.
        return items
            .tail()
            .is_ok_and(|tail| mapping.contains_key(&tail.addr_as_usize()));
    }
    false
}

/// Deep-clone `obj`, substituting any placeholder reference with the
/// mapped binding. Only descends through cons lists; quoted literals
/// and other value kinds pass through unchanged. A list that loops
/// back has no finite copy, so it is an error.
fn rewrite_ast(
    obj: &TulispObject,
    mapping: &HashMap<usize, TulispObject>,
) -> Result<TulispObject, Error> {
    if let Some(replacement) = mapping.get(&obj.addr_as_usize()) {
        return Ok(replacement.clone());
    }
    if obj.consp() {
        let span = obj.span();
        let mut builder = crate::cons::ListBuilder::new();
        let mut items = obj.base_iter();
        for car in items.by_ref() {
            builder.push(rewrite_ast(&car, mapping)?);
        }
        let tail = items.tail()?;
        if !tail.null() {
            builder.append(mapping.get(&tail.addr_as_usize()).cloned().unwrap_or(tail))?;
        }
        return Ok(builder.build().with_span(span));
    }
    Ok(obj.clone())
}

/// Clone `template` but with every `(orig, placeholder)` in `free_vars`
/// whose `orig` appears in `mapping` rewritten to the mapped binding —
/// and with nested `MakeLambda` instructions in the body recursively
/// rebuilt the same way. The inner placeholder keys themselves stay
/// intact so the inner's phase-2 rewrite still finds them.
fn rewrite_template(
    template: &LambdaTemplate,
    mapping: &HashMap<usize, TulispObject>,
) -> Result<LambdaTemplate, Error> {
    let rewrite = |obj: &mut TulispObject| {
        if let Some(replacement) = mapping.get(&obj.addr_as_usize()) {
            *obj = replacement.clone();
        }
    };

    let new_free_vars: Vec<(TulispObject, TulispObject)> = template
        .free_vars
        .iter()
        .map(|(orig, ph)| {
            let new_orig = mapping
                .get(&orig.addr_as_usize())
                .cloned()
                .unwrap_or_else(|| orig.clone());
            (new_orig, ph.clone())
        })
        .collect();

    let new_instructions = rewrite_instructions(&template.instructions, mapping, &rewrite)?;

    Ok(LambdaTemplate {
        instructions: new_instructions,
        // `rewrite_instruction` swaps in-place; PCs are stable so
        // the inner template's trace ranges still apply.
        trace_ranges: template.trace_ranges.clone(),
        param_placeholders: template.param_placeholders.clone(),
        params: template.params.clone(),
        free_vars: new_free_vars,
    })
}

#[cfg(test)]
mod tests {
    use super::{ast_contains_placeholder, rewrite_ast, run};
    use crate::TulispContext;
    use crate::TulispObject;
    use crate::bytecode::{Bytecode, Instruction, Pos};
    use crate::test_utils::eval_assert_equal;
    use std::collections::HashMap;

    #[test]
    fn a_host_call_runs_the_compiled_copy_of_a_named_defun() {
        let mut ctx = TulispContext::new();
        ctx.eval_string("(defun f (&rest _) nil) (defun g (&rest _) t)")
            .unwrap();
        let f = ctx.intern("f");
        let g = ctx.intern("g");
        let f_value = f.get().unwrap();
        assert!(ctx.vm.compiled_copy(&f, &f_value).is_some());
        // File g's compiled body under f, as compiled from f's current
        // definition: only a call that runs the compiled copy sees t.
        let mut swapped = ctx.vm.functions[&g.addr_as_usize()].clone();
        swapped.source = Some(f_value);
        ctx.vm.functions.insert(f.addr_as_usize(), swapped);
        let zero = ctx.eval_string("'(0)").unwrap();
        assert_eq!(ctx.funcall(&f, (0i64,)).unwrap().to_string(), "t");
        assert_eq!(ctx.apply(&f, vec![0i64]).unwrap().to_string(), "t");
        assert_eq!(ctx.map(&f, &zero).unwrap().to_string(), "(t)");
        assert_eq!(ctx.filter(&f, &zero).unwrap().to_string(), "(0)");
        let nil = TulispObject::nil();
        assert_eq!(ctx.reduce(&f, &zero, &nil).unwrap().to_string(), "t");
    }

    // Arguments from Rust reach a function unevaluated on every path: a
    // tree-walker lambda, and a special form that reads its argument
    // forms.
    #[test]
    fn a_host_call_passes_arguments_unevaluated() {
        let mut ctx = TulispContext::new();
        ctx.tw_eval_string("(defun same (x) x)").unwrap();
        ctx.defspecial("raw", |_, args| Ok(args.clone()));
        let same = ctx.intern("same");
        let raw = ctx.intern("raw");
        let sym = ctx.intern("unbound-sym");
        let form = ctx.eval_string("'(+ 1 2)").unwrap();
        assert_eq!(
            ctx.funcall(&same, (sym.clone(),)).unwrap().to_string(),
            "unbound-sym"
        );
        assert_eq!(
            ctx.funcall(&raw, (sym, form.clone())).unwrap().to_string(),
            "(unbound-sym (+ 1 2))"
        );
        assert_eq!(ctx.apply(&raw, &form).unwrap().to_string(), "(+ 1 2)");
    }

    #[test]
    fn a_host_call_skips_a_compiled_copy_the_symbol_no_longer_holds() {
        let mut ctx = TulispContext::new();
        ctx.eval_string("(defun f (x) (* x 10))").unwrap();
        let f = ctx.intern("f");
        assert_eq!(ctx.funcall(&f, (2i64,)).unwrap().to_string(), "20");
        ctx.tw_eval_string("(defun f (x) (+ x 1))").unwrap();
        assert!(ctx.vm.compiled_copy(&f, &f.get().unwrap()).is_none());
        assert_eq!(ctx.funcall(&f, (2i64,)).unwrap().to_string(), "3");
        ctx.defun("f", |x: i64| x - 1);
        assert_eq!(ctx.funcall(&f, (2i64,)).unwrap().to_string(), "1");
        ctx.eval_string("(defun f (x) (* x 100))").unwrap();
        assert!(ctx.vm.compiled_copy(&f, &f.get().unwrap()).is_some());
        assert_eq!(ctx.apply(&f, vec![2i64]).unwrap().to_string(), "200");
    }

    // The parse evaluates a quoted `defun` too, so after this second
    // program the symbol holds its lambda, which has no compiled copy:
    // a host call runs it, as `(funcall 'f)` does.
    #[test]
    fn a_host_call_skips_a_compiled_copy_of_a_different_defun_form() {
        let mut ctx = TulispContext::new();
        ctx.eval_string("(defun f (x) 1)").unwrap();
        ctx.eval_string("(setq data '(defun f (x) 2))").unwrap();
        let f = ctx.intern("f");
        assert_eq!(ctx.eval_string("(funcall 'f 0)").unwrap().to_string(), "2");
        assert_eq!(ctx.funcall(&f, (0i64,)).unwrap().to_string(), "2");
    }

    // `assemble` removes every trace marker and label, and resolves
    // every label jump, before a program runs. One that slips
    // through is a compiler bug and must surface as an error, not
    // as a silent no-op.
    #[test]
    fn an_unassembled_instruction_reaching_the_interpreter_is_an_error() {
        let mut ctx = TulispContext::new();
        for (leftover, message) in [
            (
                Instruction::PushTrace(TulispObject::nil()),
                "compile-time marker",
            ),
            (Instruction::PopTrace, "compile-time marker"),
            (
                Instruction::Label(TulispObject::nil()),
                "compile-time marker",
            ),
            (
                Instruction::Jump(Pos::Label(TulispObject::nil())),
                "label jump",
            ),
        ] {
            let bytecode = Bytecode::default();
            bytecode.global.borrow_mut().push(leftover);
            let err = run(&mut ctx, bytecode).unwrap_err();
            assert!(err.to_string().contains(message), "{err}");
        }
    }

    // A Rust callable may re-enter `eval_string` mid-run. An inner
    // program that yields no value must not pop the outer run's
    // pending stack value in its place.
    #[test]
    fn reentrant_eval_string_preserves_vm_stack() {
        let mut ctx = TulispContext::new();
        ctx.defspecial("inner-eval", |ctx, args| {
            let program = args.car()?.as_string()?;
            ctx.eval_string(&program)
        });
        eval_assert_equal(&mut ctx, r#"(list 1 2 (inner-eval ""))"#, "'(1 2 nil)");
    }

    // An inner `eval_string` that errors after pushing partial
    // values must not leave them on the outer run's stack when the
    // host swallows the error.
    #[test]
    fn reentrant_eval_error_leaves_outer_stack_clean() {
        let mut ctx = TulispContext::new();
        ctx.defspecial("inner-eval-swallow", |ctx, args| {
            let program = args.car()?.as_string()?;
            let result = match ctx.eval_string(&program) {
                Ok(_) => ctx.intern("ok"),
                Err(_) => ctx.intern("caught"),
            };
            Ok(result)
        });
        eval_assert_equal(
            &mut ctx,
            r#"(list 1 2 (inner-eval-swallow "(list 7 8 (car 5))"))"#,
            "'(1 2 caught)",
        );
    }

    // A host callable that catches a panic from an inner program run
    // and carries on must find the outer run's stack as it left it.
    // The second program routes the panic through the VM's own
    // `funcall` of a compiled lambda.
    #[test]
    fn caught_panic_in_reentrant_run_leaves_outer_stack_intact() {
        let mut ctx = TulispContext::new();
        ctx.defun("panicky", || -> i64 { panic!("host panic") });
        ctx.defspecial("inner-catch", |ctx, args| {
            let program = args.car()?.as_string()?;
            let caught = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                ctx.eval_string(&program)
            }));
            Ok(ctx.intern(if caught.is_err() { "caught" } else { "ok" }))
        });
        eval_assert_equal(
            &mut ctx,
            r#"(list 1 2 (inner-catch "(list 7 8 (panicky))") 3)"#,
            "'(1 2 caught 3)",
        );
        eval_assert_equal(
            &mut ctx,
            r#"(list 1 2 (inner-catch "(let ((f (lambda () (list 7 8 (panicky))))) (funcall f))") 3)"#,
            "'(1 2 caught 3)",
        );
    }

    // The same through the host's `funcall` of a compiled lambda,
    // which runs on the shared stack without a program run.
    #[test]
    fn caught_panic_in_reentrant_funcall_leaves_outer_stack_intact() {
        let mut ctx = TulispContext::new();
        ctx.defun("panicky", || -> i64 { panic!("host panic") });
        ctx.defspecial("inner-catch-call", |ctx, _args| {
            let lambda = ctx.eval_string("(lambda () (list 7 8 (panicky)))")?;
            let caught =
                std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| ctx.funcall(&lambda, ())));
            Ok(ctx.intern(if caught.is_err() { "caught" } else { "ok" }))
        });
        eval_assert_equal(
            &mut ctx,
            "(list 1 2 (inner-catch-call) 3)",
            "'(1 2 caught 3)",
        );
    }

    // A compiled lambda called through `funcall` is the one call
    // whose arity the compiler cannot check, so `run_lambda` checks
    // it like a `Call` does.
    #[test]
    fn a_compiled_lambda_called_through_funcall_checks_its_arity() {
        let mut ctx = TulispContext::new();
        for (program, message) in [
            ("(funcall (lambda (a b) (+ a b)) 1)", "Too few arguments"),
            ("(funcall (lambda (a) a) 1 2)", "Too many arguments"),
            (
                "(funcall (lambda (a &optional b) b) 1 2 3)",
                "Too many arguments",
            ),
        ] {
            let err = ctx.eval_string(program).unwrap_err();
            assert!(err.to_string().contains(message), "{program}: {err}");
        }
        eval_assert_equal(
            &mut ctx,
            "(funcall (lambda (a &optional b &rest r) (list a b r)) 1 2 3 4)",
            "'(1 2 (3 4))",
        );
    }

    // A self tail call rebinds the rest parameter in place, to nil
    // when nothing is left for it.
    #[test]
    fn a_self_tail_call_rebinds_an_empty_rest() {
        let mut ctx = TulispContext::new();
        eval_assert_equal(
            &mut ctx,
            "(defun cnt (n &rest r) (if (= n 0) r (cnt (- n 1)))) (cnt 2 1 2 3)",
            "nil",
        );
    }

    #[test]
    fn the_placeholder_walkers_stop_at_a_circular_list() {
        let mut ctx = TulispContext::new();
        let placeholder = ctx.intern("p");
        let mapping = HashMap::from([(placeholder.addr_as_usize(), TulispObject::from(9))]);
        let without = ctx
            .eval_string("(let ((l (list 1 2 3))) (setcdr (cddr l) l) l)")
            .unwrap();
        assert!(!ast_contains_placeholder(&without, &mapping));
        // The placeholder sits past the point where the loop starts.
        let with = ctx
            .eval_string("(let ((l (list 1 2 3 4 5 6 7 8 9 'p))) (setcdr (last l) (cdr l)) l)")
            .unwrap();
        assert!(ast_contains_placeholder(&with, &mapping));
        let err = rewrite_ast(&with, &mapping).unwrap_err();
        assert_eq!(err.to_string(), "ERR OutOfRange: Circular list");
    }
}
