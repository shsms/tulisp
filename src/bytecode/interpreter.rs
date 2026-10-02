use super::{
    Block, CaptureSource, Captured, Captures, FrameState, Handler, Instruction, LambdaTemplate,
    Slot, bytecode::Bytecode, bytecode::CompiledDefun, bytecode::TraceRange,
};
use crate::{
    Error, ErrorKind, Number, TulispContext, TulispObject, TulispValue, bytecode::Pos,
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
        // error.
        for obj in self.0.iter() {
            let _ = obj.unset();
        }
    }
}

/// Per-frame Drop guard for `BeginScope` bindings: a `let` / `let*` of
/// a special variable, or a `condition-case` handler binding one.
///
/// On clean execution every `BeginScope` is matched by an `EndScope`,
/// which removes the entry from the guard, so `Drop` finds the Vec
/// empty. On error escape, `?` propagates out of `run_impl_inner`
/// before the trailing `EndScope`s run; `Drop` then unsets whatever
/// is still pending. Without this guard, a `let` of a `defvar`
/// variable that errors inside a defun leaked onto
/// `SymbolBindings::items` once per call.
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
    /// Counts the functions replaced, so a call can tell that the
    /// target it keeps may be out of date. No entry is ever removed and
    /// a call keeps only one it found, so adding a name leaves every
    /// kept target valid.
    generation: u64,
    /// The lexical variables of every running call, one stretch per
    /// call; the running call's stretch starts at `base`.
    pub(crate) locals: Vec<Slot>,
    pub(crate) base: usize,
    /// The cells of the closure the running call runs.
    pub(crate) captures: Captures,
    /// Tells this machine from any other, for `Form`.
    pub(crate) id: u64,
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
        let target = match $pos {
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
        };
        // A backward jump is a loop turn.
        if target <= $pc {
            $ctx.interrupt_checkpoint()?;
        }
        $pc = target;
    };
}

impl Machine {
    pub(crate) fn new() -> Self {
        Machine {
            stack: Vec::new(),
            functions: HashMap::new(),
            generation: 0,
            locals: Vec::new(),
            base: 0,
            captures: Captures::default(),
            id: next_machine_id(),
        }
    }

    /// Starts a frame of SLOT_COUNT slots above the running one, with
    /// CAPTURES, and gives back what to restore on leaving it.
    pub(crate) fn enter_frame_state(&mut self, slot_count: u16, captures: Captures) -> FrameState {
        let saved = FrameState {
            base: self.base,
            captures: std::mem::replace(&mut self.captures, captures),
        };
        self.base = self.locals.len();
        self.locals
            .resize_with(self.base + usize::from(slot_count), Slot::default);
        saved
    }

    /// Ends the running frame and goes back to SAVED.
    pub(crate) fn leave_frame_state(&mut self, saved: FrameState) {
        self.locals.truncate(self.base);
        self.base = saved.base;
        self.captures = saved.captures;
    }

    /// Makes the running frame one of SLOT_COUNT slots with CAPTURES,
    /// for a tail call that replaces it.
    fn replace_frame(&mut self, slot_count: u16, captures: Captures) {
        self.locals.truncate(self.base);
        self.locals
            .resize_with(self.base + usize::from(slot_count), Slot::default);
        self.captures = captures;
    }

    /// Makes FUNCTION what a compiled call to the name at ADDR runs.
    pub(crate) fn set_function(&mut self, addr: usize, function: CompiledDefun) {
        if self.functions.insert(addr, function).is_some() {
            self.generation += 1;
        }
    }
}

fn next_machine_id() -> u64 {
    static NEXT: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
    NEXT.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
}

/// Leaves a frame on drop, on any path out, a panic included.
struct FrameScope<'a> {
    ctx: &'a mut TulispContext,
    saved: Option<FrameState>,
}

impl<'a> FrameScope<'a> {
    fn new(ctx: &'a mut TulispContext, slot_count: u16, captures: Captures) -> Self {
        let saved = ctx.vm.enter_frame_state(slot_count, captures);
        FrameScope {
            ctx,
            saved: Some(saved),
        }
    }
}

impl Drop for FrameScope<'_> {
    fn drop(&mut self) {
        if let Some(saved) = self.saved.take() {
            self.ctx.vm.leave_frame_state(saved);
        }
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
    // A re-entrant run (a Rust callable evaluating a program
    // mid-run) shares the machine with its caller; the guard gives
    // back only the stack.
    let mut guard = RunGuard::new(ctx);
    let tail = {
        let scope = FrameScope::new(guard.ctx, bytecode.global_slot_count, Captures::default());
        run_impl(
            scope.ctx,
            &bytecode.global,
            bytecode.global_trace_ranges.as_slice(),
            false,
        )?
    };
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

/// Invoke a VM-compiled lambda with ARGS, which are values. Used by
/// `call_function`.
fn run_lambda(
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

/// Runs BLOCK in FRAME, the frame of the code that holds it, and then
/// goes back to the running frame. Frames above FRAME's `base` may be
/// live, so `locals` is left as it is.
pub(crate) fn run_block_in_frame(
    ctx: &mut TulispContext,
    block: &Block,
    frame: &FrameState,
) -> Result<TulispObject, Error> {
    struct Restore<'a> {
        ctx: &'a mut TulispContext,
        saved: Option<FrameState>,
    }
    impl Drop for Restore<'_> {
        fn drop(&mut self) {
            if let Some(saved) = self.saved.take() {
                self.ctx.vm.base = saved.base;
                self.ctx.vm.captures = saved.captures;
            }
        }
    }
    let saved = FrameState {
        base: std::mem::replace(&mut ctx.vm.base, frame.base),
        captures: std::mem::replace(&mut ctx.vm.captures, frame.captures.clone()),
    };
    let restore = Restore {
        ctx,
        saved: Some(saved),
    };
    run_block(restore.ctx, block, None)
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
    use crate::builtin::functions::errors::{condition_matches, error_value};
    let Some(kind_sym) = err.symbol_name() else {
        return Err(err);
    };
    for handler in handlers {
        if condition_matches(ctx, &handler.condition, &kind_sym)? {
            let value = binds.then(|| error_value(ctx, &kind_sym, &err));
            return run_block_with_reserve(ctx, &handler.body, value);
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
/// first).
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
            // innermost form first, so the trace reads from the
            // failure outwards. Inner-first also lets
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
    ctx.interrupt_checkpoint()?;
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
                // `(load …)` compiles the loaded file — so defuns
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
            Instruction::JumpIfEqual(pos) => jump_if_binary!(ctx, pc, pos, |a, b| a.try_equal(b)),
            Instruction::JumpIfNotEqual(pos) => {
                jump_if_binary!(ctx, pc, pos, |a, b| a.try_equal(b).map(|equal| !equal))
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
            Instruction::Equal => compare_binary!(ctx, |a, b| a.try_equal(b)),
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
            Instruction::BindLocal(n) => {
                let value = ctx.vm.stack.pop().ok_or_else(empty_stack)?;
                *slot_mut(&mut ctx.vm, *n)? = Slot::Value(value);
            }
            Instruction::BindCell(n) => {
                let value = ctx.vm.stack.pop().ok_or_else(empty_stack)?;
                *slot_mut(&mut ctx.vm, *n)? = Slot::Cell(SharedMut::new(Some(value)));
            }
            Instruction::LoadLocal(n) => {
                let Slot::Value(value) = slot_mut(&mut ctx.vm, *n)? else {
                    return Err(Error::lisp_error(
                        "internal: a cell where a value was expected",
                    ));
                };
                let value = value.clone();
                ctx.vm.stack.push(value);
            }
            Instruction::StoreLocal(n) => {
                let value = ctx.vm.stack.last().cloned().ok_or_else(empty_stack)?;
                *slot_mut(&mut ctx.vm, *n)? = Slot::Value(value);
            }
            Instruction::StorePopLocal(n) => {
                let value = ctx.vm.stack.pop().ok_or_else(empty_stack)?;
                *slot_mut(&mut ctx.vm, *n)? = Slot::Value(value);
            }
            Instruction::LoadCell(n) => {
                // `BindCell` always stores a value, so an empty cell
                // here is a compiler bug.
                let value = slot_cell(&mut ctx.vm, *n)?
                    .borrow()
                    .clone()
                    .ok_or_else(|| Error::lisp_error("internal: an empty cell in a slot"))?;
                ctx.vm.stack.push(value);
            }
            Instruction::StoreCell(n) => {
                let value = ctx.vm.stack.last().cloned().ok_or_else(empty_stack)?;
                *slot_cell(&mut ctx.vm, *n)?.borrow_mut() = Some(value);
            }
            Instruction::StorePopCell(n) => {
                let value = ctx.vm.stack.pop().ok_or_else(empty_stack)?;
                *slot_cell(&mut ctx.vm, *n)?.borrow_mut() = Some(value);
            }
            Instruction::ClearLocals { from, to } => {
                for n in *from..*to {
                    *slot_mut(&mut ctx.vm, n)? = Slot::default();
                }
            }
            Instruction::LoadCapture(index) => {
                let captured = capture(&ctx.vm.captures, *index)?;
                let value = captured.cell.borrow().clone();
                let value = value.ok_or_else(|| {
                    Error::uninitialized(format!("Variable definition is void: {}", captured.name))
                        .with_trace(captured.name.clone())
                })?;
                ctx.vm.stack.push(value);
            }
            Instruction::StoreCapture(index) => {
                let value = ctx.vm.stack.last().cloned().ok_or_else(empty_stack)?;
                *capture(&ctx.vm.captures, *index)?.cell.borrow_mut() = Some(value);
            }
            Instruction::StorePopCapture(index) => {
                let value = ctx.vm.stack.pop().ok_or_else(empty_stack)?;
                *capture(&ctx.vm.captures, *index)?.cell.borrow_mut() = Some(value);
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
                let generation = ctx.vm.generation;
                if function
                    .as_ref()
                    .is_none_or(|(seen, _)| *seen != generation)
                {
                    let addr = name.addr_as_usize();
                    if let Some(func) = ctx.vm.functions.get(&addr) {
                        let func = func.clone();
                        (*optional_count, *rest_count) = func
                            .params
                            .arity()
                            .split(*args_count)
                            .map_err(|e| e.with_trace(form.clone()))?;
                        *function = Some((generation, func));
                    } else {
                        // Target isn't a VM-compiled defun. It might
                        // be a Rust function, a variable holding a
                        // compiled closure, or a special form, which
                        // is refused. Fall back to the same dispatch
                        // the inline `Funcall` uses.
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
                    function: function.as_ref().unwrap().1.clone(),
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
                let generation = ctx.vm.generation;
                if function
                    .as_ref()
                    .is_none_or(|(seen, _)| *seen != generation)
                {
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
                    *function = Some((generation, func));
                }

                let info = TailCallInfo {
                    function: function.as_ref().unwrap().1.clone(),
                    optional_count: *optional_count,
                    rest_count: *rest_count,
                };
                return Ok(Some(info));
            }
            Instruction::Ret => return Ok(None),
            Instruction::MakeLambda(template) => {
                let closure = make_lambda(ctx, template)?;
                ctx.vm.stack.push(closure);
            }
            Instruction::DefineFunction(name) => {
                let closure = ctx.vm.stack.pop().unwrap_or_default();
                let TulispValue::CompiledDefun { value } = &closure.inner_ref().0 else {
                    return Err(Error::lisp_error(
                        "internal: define_function needs a compiled function",
                    ));
                };
                // Install the new function only if the name still holds
                // a function of this defun form: the one the form compiled
                // to, or one an earlier run of the form made. A later
                // defun of the name, which took effect as the program
                // compiled, stays.
                let addr = name.addr_as_usize();
                let holds_this_form = ctx
                    .vm
                    .functions
                    .get(&addr)
                    .is_some_and(|current| current.trace_ranges.ptr_eq(&value.trace_ranges));
                if holds_this_form {
                    let function = CompiledDefun {
                        name: name.clone(),
                        ..value.clone()
                    };
                    name.set_global(
                        TulispValue::CompiledDefun {
                            value: function.clone(),
                        }
                        .into_ref(None),
                    )?;
                    ctx.vm.set_function(addr, function);
                }
            }
            Instruction::Catch { body } => {
                let tag = ctx.vm.stack.pop().unwrap();
                // Release the program: a recursive function re-enters it.
                let body = body.clone();
                drop(instr_ref);
                ctx.catch_tags.push(tag.clone());
                let result = run_block(ctx, &body, None);
                ctx.catch_tags.pop();
                let result =
                    result.or_else(|err| crate::builtin::functions::errors::catch_throw(err, &tag));
                instr_ref = program.borrow_mut();
                ctx.vm.stack.push(result?);
            }
            Instruction::UnwindProtect { body, cleanup } => {
                let (body, cleanup) = (body.clone(), cleanup.clone());
                drop(instr_ref);
                let result = run_block(ctx, &body, None);
                let cleaned = run_block_with_reserve(ctx, &cleanup, None);
                instr_ref = program.borrow_mut();
                let result = match result {
                    Err(e) if matches!(e.kind_ref(), ErrorKind::Interrupted) => Err(e),
                    result => cleaned.and(result),
                };
                ctx.vm.stack.push(result?);
            }
            Instruction::Raise(err) => return Err((**err).clone()),
            Instruction::DefVar(sym) => {
                let bound = sym.boundp();
                ctx.vm.stack.push(bound.into());
            }
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
                // Stack layout: [..., FN, intermediate_0..N-1, FINAL_LIST]
                let split_at = ctx.vm.stack.len() - *args_count - 1;
                let args: Vec<TulispObject> = ctx.vm.stack.drain(split_at..).collect();
                let func = ctx.vm.stack.pop().unwrap();
                let args = crate::eval::spread_apply_args(args)?;
                drop(instr_ref);
                let result = funcall_inline(ctx, &func, args)?;
                ctx.vm.stack.push(result);
                instr_ref = program.borrow_mut();
            }
            Instruction::RustCall {
                form,
                call,
                args_count,
                keep_result,
                ..
            } => {
                let args_count = *args_count;
                let split_at = ctx.vm.stack.len() - args_count;
                let args: Vec<TulispObject> = ctx.vm.stack.drain(split_at..).collect();
                // Clone what the call needs and release the program
                // borrow: the closure receives `ctx` and may re-enter
                // the interpreter, which re-borrows this instruction
                // list.
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
            Instruction::SpecialCall {
                form,
                call,
                eager_count,
                blocks,
                keep_result,
                ..
            } => {
                let split_at = ctx.vm.stack.len() - *eager_count;
                let values: Vec<TulispObject> = ctx.vm.stack.drain(split_at..).collect();
                let call_forms = crate::context::special::CallForms::new(
                    ctx.vm.id,
                    FrameState {
                        base: ctx.vm.base,
                        captures: ctx.vm.captures.clone(),
                    },
                );
                let forms = blocks
                    .iter()
                    .map(|arg| call_forms.form(arg.block.clone(), arg.source.clone()))
                    .collect();
                // The closure re-enters the machine, which re-borrows
                // this instruction list: release it first.
                let form = form.clone();
                let call = call.clone();
                let keep_result = *keep_result;
                drop(instr_ref);
                let result = call(ctx, &values, forms);
                drop(call_forms);
                let result = result.map_err(|e| e.with_trace(form))?;
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
    let scope = FrameScope::new(
        ctx,
        call.function.slot_count,
        call.function.captures.clone(),
    );
    loop {
        let params = init_defun_args(scope.ctx, &call)?;
        let tail = run_impl(
            scope.ctx,
            &call.function.instructions,
            call.function.trace_ranges.as_slice(),
            false,
        )?;
        drop(params);
        match tail {
            Some(next) => {
                call = next;
                // The next function replaces this one's frame.
                scope
                    .ctx
                    .vm
                    .replace_frame(call.function.slot_count, call.function.captures.clone());
            }
            None => return Ok(()),
        }
    }
}

/// In-VM `funcall` dispatch used by `Instruction::Funcall`: resolves
/// FUNC and calls it with ARGS, which are already evaluated, on the
/// machine already running.
fn funcall_inline(
    ctx: &mut TulispContext,
    func: &TulispObject,
    args: Vec<TulispObject>,
) -> Result<TulispObject, Error> {
    let resolved = crate::eval::resolve_function(ctx, func)?;
    call_function(ctx, &resolved, args)
}

/// Calls FUNCTION, a function value `resolve_function` gave, with
/// ARGS, which are passed as they are.
pub(crate) fn call_function(
    ctx: &mut TulispContext,
    function: &TulispObject,
    args: Vec<TulispObject>,
) -> Result<TulispObject, Error> {
    let inner = function.inner_ref();
    match &inner.0 {
        TulispValue::CompiledDefun { value } => {
            let cd = value.clone();
            drop(inner);
            run_lambda(ctx, cd, args)
        }
        TulispValue::Defun { call, arity } => {
            // ARGS are values; the closure takes them as a slice.
            let call = call.clone();
            let arity = arity.clone();
            drop(inner);
            arity.check(args.len())?;
            call(ctx, &args)
        }
        TulispValue::SpecialForm | TulispValue::Special { .. } => Err(Error::invalid_argument(
            format!("invalid function: {function}"),
        )),
        _ => Err(Error::undefined(format!("function is void: {}", function))),
    }
}

/// Makes a closure of TEMPLATE: its shared body, with the cells of the
/// variables it captures from the running frame.
fn make_lambda(ctx: &TulispContext, template: &LambdaTemplate) -> Result<TulispObject, Error> {
    let mut cells = Vec::with_capacity(template.captures.len());
    for (source, name) in &template.captures {
        let cell = match source {
            CaptureSource::Lex(binding) => match &binding.inner_ref().0 {
                // A variable with no value yet has no cell; the closure
                // gets an empty one of its own.
                TulispValue::LexicalBinding { binding } => binding
                    .current_slot()
                    .unwrap_or_else(|| SharedMut::new(None)),
                _ => {
                    return Err(Error::lisp_error(
                        "internal: a capture of something other than a binding",
                    ));
                }
            },
            CaptureSource::Local(n) => match ctx.vm.locals.get(ctx.vm.base + usize::from(*n)) {
                Some(Slot::Cell(cell)) => cell.clone(),
                _ => {
                    return Err(Error::lisp_error(
                        "internal: a capture of a slot that holds no cell",
                    ));
                }
            },
            CaptureSource::Capture(index) => capture(&ctx.vm.captures, *index)?.cell.clone(),
        };
        cells.push(Captured {
            cell,
            name: name.clone(),
        });
    }
    let function = CompiledDefun {
        captures: Captures::new(cells),
        ..template.function.clone()
    };
    Ok(TulispValue::CompiledDefun { value: function }.into_ref(None))
}

/// Slot N of the running frame.
#[inline(always)]
fn slot_mut(vm: &mut Machine, n: u16) -> Result<&mut Slot, Error> {
    let index = vm.base + usize::from(n);
    vm.locals
        .get_mut(index)
        .ok_or_else(|| Error::lisp_error("internal: a slot past the frame"))
}

/// The cell in slot N of the running frame.
#[inline(always)]
fn slot_cell(vm: &mut Machine, n: u16) -> Result<&crate::bytecode::Cell, Error> {
    match slot_mut(vm, n)? {
        Slot::Cell(cell) => Ok(cell),
        Slot::Value(_) => Err(Error::lisp_error(
            "internal: a value where a cell was expected",
        )),
    }
}

fn empty_stack() -> Error {
    Error::lisp_error("internal: empty stack")
}

/// The captured variable at INDEX of CAPTURES.
#[inline(always)]
fn capture(captures: &Captures, index: u16) -> Result<&Captured, Error> {
    captures
        .get(usize::from(index))
        .ok_or_else(|| Error::lisp_error("internal: capture index past the closure's captures"))
}

#[cfg(test)]
mod tests {
    use super::run;
    use crate::bytecode::{Bytecode, Instruction, Pos};
    use crate::test_utils::eval_assert_equal;
    use crate::{Error, Form, Rest, TulispContext, TulispObject, TulispValue};

    // A `defun` leaves its compiled function on its symbol, and a call
    // from Rust runs it.
    #[test]
    fn a_host_call_runs_the_compiled_function_of_a_defun() {
        let mut ctx = TulispContext::new();
        ctx.eval_string("(defun f (&rest xs) (cons 'compiled xs))")
            .unwrap();
        let f = ctx.intern("f");
        assert!(matches!(
            f.get().unwrap().inner_ref().0,
            TulispValue::CompiledDefun { .. }
        ));
        let zero = ctx.eval_string("'(0)").unwrap();
        assert_eq!(
            ctx.funcall(&f, (0i64,)).unwrap().to_string(),
            "(compiled 0)"
        );
        assert_eq!(
            ctx.apply(&f, vec![0i64]).unwrap().to_string(),
            "(compiled 0)"
        );
        assert_eq!(ctx.map(&f, &zero).unwrap().to_string(), "((compiled 0))");
        assert_eq!(ctx.filter(&f, &zero).unwrap().to_string(), "(0)");
        let nil = TulispObject::nil();
        assert_eq!(
            ctx.reduce(&f, &zero, &nil).unwrap().to_string(),
            "(compiled nil 0)"
        );
    }

    // Arguments from Rust reach a function unevaluated. A special form
    // is not a function, so Rust cannot call it.
    #[test]
    fn a_host_call_passes_arguments_unevaluated() {
        let mut ctx = TulispContext::new();
        ctx.eval_string("(defun same (x) x)").unwrap();
        ctx.defspecial("raw", |forms: Rest<Form>| -> TulispObject {
            forms
                .into_iter()
                .map(|form| form.source().clone())
                .collect()
        });
        let same = ctx.intern("same");
        let raw = ctx.intern("raw");
        let sym = ctx.intern("unbound-sym");
        let form = ctx.eval_string("'(+ 1 2)").unwrap();
        assert_eq!(
            ctx.funcall(&same, (sym.clone(),)).unwrap().to_string(),
            "unbound-sym"
        );
        for err in [
            ctx.funcall(&raw, (sym, form.clone())).unwrap_err(),
            ctx.apply(&raw, &form).unwrap_err(),
        ] {
            assert!(err.format(&ctx).contains("invalid function: raw"));
        }
    }

    // After a later program redefines a function, a call compiled
    // earlier reaches the new definition.
    #[test]
    fn a_compiled_call_reaches_a_redefined_function() {
        let ctx = &mut TulispContext::new();
        ctx.eval_string("(defun g () 1) (defun h () (g)) (defun hl () (list (g))) (h) (hl)")
            .unwrap();
        ctx.eval_string("(defun g () 2)").unwrap();
        let got = ctx
            .eval_string("(list (h) (hl) (funcall 'h) (mapcar (lambda (_) (h)) '(1)))")
            .unwrap();
        assert_eq!(got.to_string(), "(2 (2) 2 (2))");
        let h = ctx.intern("h");
        assert_eq!(ctx.funcall(&h, ()).unwrap().to_string(), "2");
        // A redefinition with other parameters checks the arguments
        // again.
        ctx.eval_string(
            "(defun g2 (&optional a) (list 'opt a))
             (defun h2 () (g2 1)) (defun hl2 () (list (g2 1))) (h2) (hl2)",
        )
        .unwrap();
        ctx.eval_string("(defun g2 (a) (list 'req a))").unwrap();
        let got = ctx.eval_string("(list (h2) (hl2))").unwrap();
        assert_eq!(got.to_string(), "((req 1) ((req 1)))");
        ctx.eval_string("(defun g2 (a b) b)").unwrap();
        for program in ["(h2)", "(hl2)"] {
            let err = ctx.eval_string(program).unwrap_err();
            assert!(err.to_string().contains("Too few arguments"), "{err}");
        }
    }

    // A call from Rust runs whatever the symbol holds now.
    #[test]
    fn a_host_call_runs_what_the_symbol_holds() {
        let mut ctx = TulispContext::new();
        ctx.eval_string("(defun f (x) (* x 10))").unwrap();
        let f = ctx.intern("f");
        assert_eq!(ctx.funcall(&f, (2i64,)).unwrap().to_string(), "20");
        ctx.eval_string("(defun f (x) (+ x 1))").unwrap();
        assert_eq!(ctx.funcall(&f, (2i64,)).unwrap().to_string(), "3");
        ctx.defun("f", |x: i64| x - 1);
        assert_eq!(ctx.funcall(&f, (2i64,)).unwrap().to_string(), "1");
        ctx.eval_string("(defun f (x) (* x 100))").unwrap();
        assert_eq!(ctx.apply(&f, vec![2i64]).unwrap().to_string(), "200");
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
        ctx.defspecial("inner-eval", |ctx: &mut TulispContext, program: String| {
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
        ctx.defspecial(
            "inner-eval-swallow",
            |ctx: &mut TulispContext, program: String| match ctx.eval_string(&program) {
                Ok(_) => ctx.intern("ok"),
                Err(_) => ctx.intern("caught"),
            },
        );
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
        ctx.defspecial("inner-catch", |ctx: &mut TulispContext, program: String| {
            let caught = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                ctx.eval_string(&program)
            }));
            ctx.intern(if caught.is_err() { "caught" } else { "ok" })
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
        ctx.defspecial(
            "inner-catch-call",
            |ctx: &mut TulispContext| -> Result<TulispObject, Error> {
                let lambda = ctx.eval_string("(lambda () (list 7 8 (panicky)))")?;
                let caught = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    ctx.funcall(&lambda, ())
                }));
                Ok(ctx.intern(if caught.is_err() { "caught" } else { "ok" }))
            },
        );
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
    fn test_vm_reentry_during_run() -> Result<(), Error> {
        // VM re-entry from inside a VM run. The outer `eval_string`
        // is mid-run on `ctx.vm` when the `eval` defun receives the
        // quoted form and hands it to `ctx.eval`, which compiles and
        // runs it in the same machine: its `funcall` reaches a callable
        // defined on the same context and runs it there. The inner
        // and outer runs share the same machine — the inner sees the
        // outer's function table and pushes/pops on the shared stack.
        //
        // Pre-rewrite (when `ctx.vm` was `Option<Machine>` taken at
        // the run boundary), the inner entry found `ctx.vm = None`
        // and panicked with "ctx.vm taken twice — VM re-entered
        // during a run". Free-function dispatch with a direct
        // `ctx.vm` field makes re-entry transparent.

        // Lambda case: `f` holds a `CompiledDefun` (anonymous lambda
        // materialized by `Instruction::MakeLambda`). The inner run's
        // `funcall` runs it through `run_lambda`.
        let mut ctx = TulispContext::new();
        let result: i64 = ctx
            .eval_string(
                r#"
        (setq f (lambda () 42))
        (eval '(funcall f))
        "#,
            )?
            .try_into()?;
        assert_eq!(result, 42);

        // Defun case: top-level `(defun g …)` leaves a
        // `CompiledDefun` on `g`. The inner run's `funcall` on the
        // symbol runs it through `run_lambda`, in the machine the outer
        // run is using.
        let mut ctx = TulispContext::new();
        let result: i64 = ctx
            .eval_string(
                r#"
        (defun g () 99)
        (eval '(funcall 'g))
        "#,
            )?
            .try_into()?;
        assert_eq!(result, 99);
        Ok(())
    }

    // A call from Rust runs a compiled `defun` that calls a compiled
    // lambda, which passes its argument to a typed Rust function: the
    // arguments from Rust reach the lambda as the values they are.
    #[test]
    fn a_host_call_reaches_a_typed_function_through_a_lambda() -> Result<(), Error> {
        let mut ctx = TulispContext::new();
        ctx.defun("rust-needs-num", |v: f64| -> f64 { v });
        ctx.eval_string("(set 'inner-fn (lambda (x) (rust-needs-num x)))")?;
        ctx.eval_string(
            r#"
        (defun outer (id v)
          (funcall (symbol-value 'inner-fn) v))
        "#,
        )?;
        let outer = ctx.intern("outer");
        let result = ctx.funcall(&outer, (1i64, 42.0_f64))?;
        assert!(
            result.equal(&42.0_f64.into()),
            "expected 42, got {}",
            result
        );
        Ok(())
    }

    // Every way out of a run leaves the machine's slots as it found
    // them.
    #[test]
    fn locals_are_empty_after_every_run() {
        let ctx = &mut TulispContext::new();
        for program in [
            "(defun f (n) (if (= n 0) 0 (f (- n 1)))) (f 100)",
            "(let ((a 1)) (let ((b 2)) (+ a b)))",
            "(condition-case nil (let ((a 1)) (error \"x\")) (error 2))",
            "(catch 'k (let ((a 1)) (throw 'k a)))",
            "(funcall (lambda (x) (let ((y x)) y)) 3)",
            "(let ((a 1)) (error \"escapes\"))",
        ] {
            let _ = ctx.eval_string(program);
            assert_eq!(ctx.debug_locals_len(), 0, "{program}");
        }
    }

    #[test]
    fn locals_are_empty_after_the_depth_limit_error() {
        // On an 8 MiB stack, as the depth tests in `context.rs` run.
        std::thread::Builder::new()
            .stack_size(8 * 1024 * 1024)
            .spawn(|| {
                let ctx = &mut TulispContext::new();
                ctx.set_max_eval_depth(50);
                let err = ctx
                    .eval_string("(defun deep (n) (let ((m n)) (+ 1 (deep m)))) (deep 1)")
                    .unwrap_err();
                assert!(err.to_string().contains("depth"), "{err}");
                assert_eq!(ctx.debug_locals_len(), 0);
            })
            .unwrap()
            .join()
            .unwrap();
    }

    #[test]
    fn locals_are_empty_after_a_caught_panic() {
        let mut ctx = TulispContext::new();
        ctx.defun("boom", || -> i64 { panic!("boom") });
        let caught = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            let _ = ctx.eval_string("(defun g (x) (let ((y x)) (boom))) (g 1)");
        }));
        assert!(caught.is_err());
        assert_eq!(ctx.debug_locals_len(), 0);
        eval_assert_equal(&mut ctx, "(defun h (x) x) (h 1)", "1");
    }

    // A tail-call loop replaces its frame each turn, so the machine's
    // slots stay the same size.
    #[test]
    fn a_tail_call_loop_keeps_locals_constant() {
        use std::sync::{Arc, Mutex};
        let ctx = &mut TulispContext::new();
        let seen: Arc<Mutex<Vec<usize>>> = Arc::default();
        let log = seen.clone();
        ctx.defun("note-locals", move |ctx: &mut TulispContext| {
            log.lock().unwrap().push(ctx.debug_locals_len());
        });
        ctx.eval_string(
            "(defun spin (n) (let ((a n)) (note-locals) (if (= n 0) 0 (spin (- n 1)))))
             (spin 50)",
        )
        .unwrap();
        let seen = seen.lock().unwrap();
        assert_eq!(seen.len(), 51);
        assert!(seen.windows(2).all(|w| w[0] == w[1]), "{seen:?}");
    }

    // A `let` lets go of its values when it ends, not when its function
    // returns.
    #[test]
    fn a_let_lets_go_of_its_values_when_it_ends() {
        use std::sync::Arc;
        use std::sync::atomic::{AtomicUsize, Ordering};
        #[derive(Clone)]
        struct Token(Arc<AtomicUsize>);
        impl Drop for Token {
            fn drop(&mut self) {
                self.0.fetch_add(1, Ordering::SeqCst);
            }
        }
        impl std::fmt::Display for Token {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.write_str("#<token>")
            }
        }
        impl crate::TulispAny for Token {}

        let dropped = Arc::new(AtomicUsize::new(0));
        let ctx = &mut TulispContext::new();
        let made = dropped.clone();
        ctx.defun("make-token", move || Token(made.clone()));
        let seen = dropped.clone();
        ctx.defun("tokens-dropped", move || seen.load(Ordering::SeqCst) as i64);
        eval_assert_equal(
            ctx,
            "(defun f () (let ((x (make-token))) nil) (tokens-dropped)) (f)",
            "1",
        );
    }
}
