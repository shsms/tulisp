use super::{
    Block, CaptureSource, Captured, CapturedValue, Captures, FormBlock, FrameState, Handler,
    Instruction, LambdaTemplate, Slot, bytecode::Bytecode, bytecode::CompiledDefun,
    bytecode::TraceRange,
};
use crate::{
    Error, ErrorKind, Number, ParamKind, TulispContext, TulispObject, TulispValue,
    bytecode::Pos,
    object::wrappers::{
        DefunFn, SpecialFn,
        generic::{Shared, SharedMut},
    },
    plist,
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

pub struct Machine {
    stack: Vec<TulispObject>,
    functions: HashMap<usize, CompiledDefun>, // key: fn_name.addr_as_usize()
    /// Counts the changes to what a name calls: a function, macro or
    /// special form defined, replaced or removed. A call keeps the target
    /// it found with the count it found it at, and finds it again when the
    /// count has moved.
    generation: u64,
    /// The lexical variables of every running call, one stretch per
    /// call; the running call's stretch starts at `base`.
    pub(crate) locals: Vec<Slot>,
    pub(crate) base: usize,
    /// The variables the running call's closure captured.
    pub(crate) captures: Captures,
    /// Tells this machine from any other, for `Form`.
    pub(crate) id: u64,
    /// The special variables bound by a `let` or a `condition-case`
    /// handler that has not ended, in the order bound; each `EndScope`
    /// ends the last one. When a run or a block ends, an error or a
    /// panic included, its `RunGuard` undoes the ones bound since it
    /// began. A `Call` or `TailCall` of a compiled `defun` runs under its
    /// caller's guard; `run_lambda`, which every other call of a compiled
    /// function goes through (`funcall`, `apply`, calls from Rust), has
    /// its own.
    specials: Vec<TulispObject>,
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
            specials: Vec::new(),
        }
    }

    /// The running frame's base and captures.
    pub(crate) fn frame_state(&self) -> FrameState {
        FrameState {
            base: self.base,
            captures: self.captures.clone(),
        }
    }

    /// Makes FRAME the running one and gives back the one it replaces.
    /// `locals` is left as it is.
    fn swap_frame(&mut self, frame: FrameState) -> FrameState {
        FrameState {
            base: std::mem::replace(&mut self.base, frame.base),
            captures: std::mem::replace(&mut self.captures, frame.captures),
        }
    }

    /// Makes the running frame one of SLOT_COUNT slots with CAPTURES,
    /// for a tail call that replaces it.
    fn replace_frame(&mut self, slot_count: u16, captures: Captures) {
        self.locals.truncate(self.base);
        self.reserve_slots(slot_count);
        self.captures = captures;
    }

    /// Adds SLOT_COUNT empty slots for the running frame.
    #[inline(always)]
    fn reserve_slots(&mut self, slot_count: u16) {
        if slot_count > 0 {
            self.locals
                .resize_with(self.base + usize::from(slot_count), Slot::default);
        }
    }

    /// Undoes the special bindings made since there were BASE of them.
    fn unwind_specials(&mut self, base: usize) {
        // Most runs bind none; a drain costs even when empty.
        if self.specials.len() > base {
            for symbol in self.specials.drain(base..).rev() {
                let _ = symbol.unset();
            }
        }
    }

    /// Makes FUNCTION what a compiled call to the name at ADDR runs.
    pub(crate) fn set_function(&mut self, addr: usize, function: CompiledDefun) {
        self.functions.insert(addr, function);
        self.generation += 1;
    }

    /// Drops the compiled function of the name at ADDR, and marks every
    /// call's kept target as possibly out of date.
    pub(crate) fn remove_function(&mut self, addr: usize) {
        self.functions.remove(&addr);
        self.generation += 1;
    }

    /// The count a call keeps its target at; see `generation`.
    pub(crate) fn generation(&self) -> u64 {
        self.generation
    }
}

fn next_machine_id() -> u64 {
    static NEXT: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
    NEXT.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
}

/// Goes back to the frame that was running before on drop, on any path
/// out, a panic included.
struct LocalsGuard<'a> {
    ctx: &'a mut TulispContext,
    saved: FrameState,
    /// What the guard does with the slots when it ends.
    slots: GuardedSlots,
}

/// The slots a `LocalsGuard` gives back.
enum GuardedSlots {
    /// The guard's own frame: its slots go.
    Own,
    /// The slots at RANGE of an existing frame, which a form binds its
    /// variables in: they are cleared, and the values a run of the same
    /// form had there are put back.
    Form {
        range: std::ops::Range<usize>,
        set_aside: Vec<Slot>,
    },
}

impl<'a> LocalsGuard<'a> {
    /// Starts a frame of SLOT_COUNT slots above the running one, with
    /// CAPTURES.
    fn new(ctx: &'a mut TulispContext, slot_count: u16, captures: Captures) -> Self {
        let base = ctx.vm.locals.len();
        let saved = ctx.vm.swap_frame(FrameState { base, captures });
        ctx.vm.reserve_slots(slot_count);
        LocalsGuard {
            ctx,
            saved,
            slots: GuardedSlots::Own,
        }
    }

    /// Enters FRAME, an existing frame, for a run of a form. FRAME's
    /// slots stay when the guard ends, but for SLOTS, the ones the form
    /// binds its variables in: any value in them belongs to a run of the
    /// same form that is still going, and is set aside and put back when
    /// this run ends.
    fn for_form(
        ctx: &'a mut TulispContext,
        frame: FrameState,
        slots: std::ops::Range<u16>,
    ) -> Result<Self, Error> {
        let range = frame.base + usize::from(slots.start)..frame.base + usize::from(slots.end);
        let in_range = ctx
            .vm
            .locals
            .get_mut(range.clone())
            .ok_or_else(slot_past_frame)?;
        let set_aside = if in_range.iter().any(|slot| !matches!(slot, Slot::Empty)) {
            in_range.iter_mut().map(std::mem::take).collect()
        } else {
            Vec::new()
        };
        let saved = ctx.vm.swap_frame(frame);
        Ok(LocalsGuard {
            ctx,
            saved,
            slots: GuardedSlots::Form { range, set_aside },
        })
    }
}

impl Drop for LocalsGuard<'_> {
    fn drop(&mut self) {
        let vm = &mut self.ctx.vm;
        match &mut self.slots {
            GuardedSlots::Own => vm.locals.truncate(vm.base),
            GuardedSlots::Form { range, set_aside } => {
                if let Some(in_range) = vm.locals.get_mut(range.clone()) {
                    if set_aside.len() == in_range.len() {
                        // This run's values go with the guard.
                        in_range.swap_with_slice(set_aside);
                    } else {
                        in_range.fill_with(Slot::default);
                    }
                }
            }
        }
        vm.swap_frame(std::mem::take(&mut self.saved));
    }
}

/// Restores the caller's VM stack height on drop, and undoes the
/// special bindings the run left, on any path including a panic, so
/// only this run's values ever sit above it.
struct RunGuard<'a> {
    ctx: &'a mut TulispContext,
    stack_base: usize,
    /// How many special bindings were in force when the run began.
    specials_base: usize,
}

impl<'a> RunGuard<'a> {
    fn new(ctx: &'a mut TulispContext) -> Self {
        let stack_base = ctx.vm.stack.len();
        let specials_base = ctx.vm.specials.len();
        RunGuard {
            ctx,
            stack_base,
            specials_base,
        }
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
        self.ctx.vm.unwind_specials(self.specials_base);
    }
}

pub fn run(ctx: &mut TulispContext, bytecode: Bytecode) -> Result<TulispObject, Error> {
    // A re-entrant run (a Rust callable evaluating a program
    // mid-run) shares the machine with its caller; the guard gives
    // back only the stack and the special bindings.
    let mut guard = RunGuard::new(ctx);
    let tail = {
        let locals = LocalsGuard::new(guard.ctx, bytecode.global_slot_count, Captures::default());
        run_impl(
            locals.ctx,
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

    // Push args in order; `init_defun_args` moves them into the
    // frame's slots, in the same order.
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
/// back to where it was, and the block's `RunGuard` undoes its
/// bindings.
pub(crate) fn run_block(
    ctx: &mut TulispContext,
    block: &Block,
    arg: Option<TulispObject>,
) -> Result<TulispObject, Error> {
    run_block_impl(ctx, block, arg, false)
}

/// Runs FORM's block in FRAME, the frame of the code that holds it,
/// and then goes back to the running frame. Frames above FRAME's `base`
/// may be live, so `locals` is left as it is, but for the slots the
/// form binds its variables in.
pub(crate) fn run_form_in_frame(
    ctx: &mut TulispContext,
    form: &FormBlock,
    frame: &FrameState,
) -> Result<TulispObject, Error> {
    let guard = LocalsGuard::for_form(ctx, frame.clone(), form.slots.clone())?;
    run_block(guard.ctx, &form.block, None)
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
                // `b` is the top of the stack, and `a` the value below it,
                // the first operand, evaluated first.
                let [ref a, ref b] = ctx.vm.stack[(ctx.vm.stack.len() - 2)..] else {
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
            Instruction::ArithChain { op, count } => {
                let base = ctx.vm.stack.len() - *count;
                let result = match ctx.vm.stack[base..].split_first() {
                    Some((first, rest)) => op.fold(first, rest).map(TulispObject::from),
                    None => Err(missing_arguments()),
                };
                ctx.vm.stack.truncate(base);
                ctx.vm.stack.push(result?);
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
                let [ref variable, ref value] = ctx.vm.stack[minus2..] else {
                    unreachable!()
                };
                variable.set(value.clone())?;
                // remove just the variable from the stack, keep the value
                ctx.vm.stack.swap_remove(minus2);
            }
            Instruction::SetPop => {
                let value = ctx.vm.stack.pop().ok_or_else(empty_stack)?;
                let variable = ctx.vm.stack.pop().ok_or_else(empty_stack)?;
                variable.set(value)?;
            }
            Instruction::StorePop(obj) => {
                let a = ctx.vm.stack.pop().unwrap();
                obj.set(a)?;
            }
            Instruction::StorePopGlobal(obj) => {
                let a = ctx.vm.stack.pop().unwrap();
                obj.set_global(a)?;
            }
            Instruction::Store(obj) => {
                let a = ctx.vm.stack.last().unwrap();
                obj.set(a.clone())?;
            }
            Instruction::Load(obj) => {
                let a = obj.get().map_err(|e| e.with_trace(obj.clone()))?;
                ctx.vm.stack.push(a);
            }
            Instruction::BindLocal(n) | Instruction::StorePopLocal(n) => {
                let value = ctx.vm.stack.pop().ok_or_else(empty_stack)?;
                *slot_mut(&mut ctx.vm, *n)? = Slot::Value(value);
            }
            Instruction::BindCell(n) => {
                let value = ctx.vm.stack.pop().ok_or_else(empty_stack)?;
                *slot_mut(&mut ctx.vm, *n)? = Slot::Cell(SharedMut::new(Some(value)));
            }
            Instruction::LoadLocal(n) => {
                let value = match slot(&ctx.vm, *n)? {
                    Slot::Value(value) => value.clone(),
                    // A missing optional argument.
                    Slot::Empty => TulispObject::nil(),
                    Slot::Cell(_) => {
                        return Err(Error::lisp_error(
                            "internal: a cell where a value was expected",
                        ));
                    }
                };
                ctx.vm.stack.push(value);
            }
            Instruction::StoreLocal(n) => {
                let value = ctx.vm.stack.last().cloned().ok_or_else(empty_stack)?;
                *slot_mut(&mut ctx.vm, *n)? = Slot::Value(value);
            }
            Instruction::LoadCell(n) => {
                // `BindCell` always stores a value, so an empty cell
                // here is a compiler bug.
                let value = slot_cell(&ctx.vm, *n)?
                    .borrow()
                    .clone()
                    .ok_or_else(|| Error::lisp_error("internal: an empty cell in a slot"))?;
                ctx.vm.stack.push(value);
            }
            Instruction::StoreCell(n) => {
                let value = ctx.vm.stack.last().cloned().ok_or_else(empty_stack)?;
                *slot_cell(&ctx.vm, *n)?.borrow_mut() = Some(value);
            }
            Instruction::StorePopCell(n) => {
                let value = ctx.vm.stack.pop().ok_or_else(empty_stack)?;
                *slot_cell(&ctx.vm, *n)?.borrow_mut() = Some(value);
            }
            Instruction::ClearLocals { from, to } => {
                let vm = &mut ctx.vm;
                let range = vm.base + usize::from(*from)..vm.base + usize::from(*to);
                vm.locals
                    .get_mut(range)
                    .ok_or_else(slot_past_frame)?
                    .fill_with(Slot::default);
            }
            Instruction::LoadCapture(index) => {
                let captured = capture(&ctx.vm.captures, *index)?;
                let value = match &captured.value {
                    CapturedValue::Value(value) => value.clone(),
                    CapturedValue::Cell(cell) => cell.borrow().clone().ok_or_else(|| {
                        Error::void_variable(captured.name.clone())
                            .with_trace(captured.name.clone())
                    })?,
                };
                ctx.vm.stack.push(value);
            }
            Instruction::StoreCapture(index) => {
                let value = ctx.vm.stack.last().cloned().ok_or_else(empty_stack)?;
                *capture_cell(&ctx.vm.captures, *index)?.borrow_mut() = Some(value);
            }
            Instruction::StorePopCapture(index) => {
                let value = ctx.vm.stack.pop().ok_or_else(empty_stack)?;
                *capture_cell(&ctx.vm.captures, *index)?.borrow_mut() = Some(value);
            }
            Instruction::BeginScope(obj) => {
                let a = ctx.vm.stack.last().unwrap();
                obj.set_scope(a.clone())?;
                ctx.vm.specials.push(obj.clone());
                ctx.vm.stack.truncate(ctx.vm.stack.len() - 1);
            }
            Instruction::EndScope(obj) => {
                // The binding that ends is the last one made.
                if ctx.vm.specials.pop_if(|last| last.eq_ptr(obj)).is_none() {
                    return Err(Error::lisp_error(format!(
                        "internal: EndScope of {obj}, which is not the last binding"
                    )));
                }
                obj.unset()?;
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
                        let (name, form, args_count) = (name.clone(), form.clone(), *args_count);
                        drop(instr_ref);
                        call_by_name(ctx, &name, form, args_count)?;
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
                        // The name no longer holds a VM-compiled defun:
                        // call it as `Call` does, and return its value.
                        let (name, form, args_count) = (name.clone(), form.clone(), *args_count);
                        drop(instr_ref);
                        call_by_name(ctx, &name, form, args_count)?;
                        return Ok(None);
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
                let function = match &closure.inner_ref().0 {
                    TulispValue::CompiledDefun { value } => value.clone(),
                    _ => {
                        return Err(Error::lisp_error(
                            "internal: define_function needs a compiled function",
                        ));
                    }
                };
                // The form compiled the closure under NAME already.
                debug_assert!(function.name.eq_ptr(name));
                // Install the new function unless the machine's table holds
                // a function of another defun of the name, so that a later
                // defun, which took effect as the program compiled, stays.
                // One of this form, the one it compiled to or one an
                // earlier run made, is replaced, and so is no function, or
                // one made under another name or none, as a lambda is.
                let addr = name.addr_as_usize();
                let holds_another_defun = ctx.vm.functions.get(&addr).is_some_and(|current| {
                    current.name.eq_ptr(name) && !current.same_code(&function)
                });
                if !holds_another_defun {
                    name.set_global(closure)?;
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
                    Err(e) if matches!(e.kind(), ErrorKind::Interrupted) => Err(e),
                    result => cleaned.and(result),
                };
                ctx.vm.stack.push(result?);
            }
            Instruction::Raise(err) => return Err((**err).clone()),
            Instruction::DefVar(sym) => {
                let has_value = sym.global().is_some();
                ctx.vm.stack.push(has_value.into());
            }
            Instruction::ConditionCase {
                binds,
                body,
                handlers,
                success,
            } => {
                let (binds, body, handlers, success) =
                    (*binds, body.clone(), handlers.clone(), success.clone());
                drop(instr_ref);
                let result = match run_block(ctx, &body, None) {
                    Err(err) => run_handler(ctx, binds, &handlers, err),
                    // The success block runs outside the handlers.
                    Ok(value) => match &success {
                        Some(success) => run_block(ctx, success, binds.then_some(value)),
                        None => Ok(value),
                    },
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
                name,
                form,
                call,
                args_count,
                keep_result,
            } => {
                let args_count = *args_count;
                // A function registered since the call compiled may
                // have replaced the one it keeps.
                let generation = ctx.vm.generation;
                if call.0 != generation {
                    call.1 = resolve_rust_function(name, args_count)
                        .map_err(|e| e.with_trace(form.clone()))?;
                    call.0 = generation;
                }
                // The function, or the name to call the general way when
                // it is no longer a Rust function.
                let target = match &call.1 {
                    Some(func) => Ok(func.clone()),
                    None => Err(name.clone()),
                };
                let split_at = ctx.vm.stack.len() - args_count;
                let args: Vec<TulispObject> = ctx.vm.stack.drain(split_at..).collect();
                // Clone what the call needs and release the program
                // borrow: the closure receives `ctx` and may re-enter
                // the interpreter, which re-borrows this instruction
                // list.
                let form = form.clone();
                let keep_result = *keep_result;
                drop(instr_ref);
                let result = match target {
                    Ok(call) => call(ctx, &args),
                    Err(name) => funcall_inline(ctx, &name, args),
                }
                .map_err(|e| e.with_trace(form))?;
                instr_ref = program.borrow_mut();
                if keep_result {
                    ctx.vm.stack.push(result);
                }
            }
            Instruction::SpecialCall {
                name,
                form,
                call,
                kinds,
                eager_count,
                blocks,
                keep_result,
            } => {
                // A special form registered since the call compiled may
                // have replaced the one it keeps.
                let generation = ctx.vm.generation;
                if call.0 != generation {
                    let found = resolve_special_form(name, kinds)
                        .map_err(|e| e.with_trace(form.clone()))?;
                    *call = (generation, found);
                }
                let split_at = ctx.vm.stack.len() - *eager_count;
                let values: Vec<TulispObject> = ctx.vm.stack.drain(split_at..).collect();
                let call_forms =
                    crate::context::special::CallForms::new(ctx.vm.id, ctx.vm.frame_state());
                let forms = blocks.iter().map(|arg| call_forms.form(arg)).collect();
                // The closure re-enters the machine, which re-borrows
                // this instruction list: release it first.
                let form = form.clone();
                let call = call.1.clone();
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
                let [ref plist, ref key] = ctx.vm.stack[(ctx.vm.stack.len() - 2)..] else {
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

/// Moves the arguments of CALL, on top of the stack, into the first
/// slots of the frame: the required and optional ones in order, then
/// the rest as a list. A missing optional stays empty, which reads as
/// nil.
fn init_defun_args(ctx: &mut TulispContext, call: &TailCallInfo) -> Result<(), Error> {
    let params = &call.function.params;
    let required = params.required.len();
    let Machine {
        stack,
        locals,
        base,
        ..
    } = &mut ctx.vm;
    if params.rest.is_some() {
        let mut list = TulispObject::nil();
        for _ in 0..call.rest_count {
            list = TulispObject::cons(stack.pop().ok_or_else(missing_arguments)?, list);
        }
        let index = *base + required + params.optional.len();
        *locals.get_mut(index).ok_or_else(slot_past_frame)? = Slot::Value(list);
    }
    let positional = required + call.optional_count;
    let slots = locals
        .get_mut(*base..*base + positional)
        .ok_or_else(slot_past_frame)?;
    for slot in slots.iter_mut().rev() {
        *slot = Slot::Value(stack.pop().ok_or_else(missing_arguments)?);
    }
    Ok(())
}

fn missing_arguments() -> Error {
    Error::lisp_error("internal: missing arguments")
}

fn slot_past_frame() -> Error {
    Error::lisp_error("internal: a slot past the frame")
}

/// Runs `call`'s function on arguments already on the stack and
/// follows its tail calls until one returns a value, so a chain of
/// tail calls costs no native stack.
fn run_tail_calls(ctx: &mut TulispContext, mut call: TailCallInfo) -> Result<(), Error> {
    // The frame takes the captures; the call keeps the code.
    let captures = std::mem::take(&mut call.function.captures);
    let locals = LocalsGuard::new(ctx, call.function.slot_count, captures);
    loop {
        init_defun_args(locals.ctx, &call)?;
        let tail = run_impl(
            locals.ctx,
            &call.function.instructions,
            call.function.trace_ranges.as_slice(),
            false,
        )?;
        match tail {
            Some(next) => {
                call = next;
                // The next function replaces this one's frame.
                let captures = std::mem::take(&mut call.function.captures);
                locals
                    .ctx
                    .vm
                    .replace_frame(call.function.slot_count, captures);
            }
            None => return Ok(()),
        }
    }
}

/// NAME's function, looked up again for a call compiled for a Rust
/// function: the Rust function it holds, with its argument count
/// checked against ARGS_COUNT, or `None` for any other value.
fn resolve_rust_function(
    name: &TulispObject,
    args_count: usize,
) -> Result<Option<Shared<dyn DefunFn>>, Error> {
    let Ok(func) = name.get() else {
        return Ok(None);
    };
    match &func.inner_ref().0 {
        TulispValue::Defun { call, arity } => {
            arity.check(args_count)?;
            Ok(Some(call.clone()))
        }
        _ => Ok(None),
    }
}

/// NAME's special form, looked up again for a call compiled for one
/// that takes the parameter KINDS. Any other value is an error.
fn resolve_special_form(
    name: &TulispObject,
    kinds: &[ParamKind],
) -> Result<Shared<dyn SpecialFn>, Error> {
    let func = name.get().map_err(|_| Error::void_function(name))?;
    match &func.inner_ref().0 {
        TulispValue::Special {
            call,
            kinds: current,
            ..
        } => {
            if current[..] == kinds[..] {
                Ok(call.clone())
            } else {
                Err(Error::lisp_error(format!(
                    "special form {name} changed its parameters since this call compiled"
                )))
            }
        }
        _ => Err(Error::lisp_error(format!(
            "{name} is no longer a special form, as it was when this call compiled"
        ))),
    }
}

/// Calls NAME the general way, with the top ARGS_COUNT values on the
/// stack as its arguments, and pushes its value. An error is traced to
/// FORM.
fn call_by_name(
    ctx: &mut TulispContext,
    name: &TulispObject,
    form: TulispObject,
    args_count: usize,
) -> Result<(), Error> {
    let split_at = ctx.vm.stack.len() - args_count;
    let args: Vec<TulispObject> = ctx.vm.stack.drain(split_at..).collect();
    let result = funcall_inline(ctx, name, args).map_err(|e| e.with_trace(form))?;
    ctx.vm.stack.push(result);
    Ok(())
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

/// Calls FUNCTION, such as the value `resolve_function` gave, with ARGS, which
/// are passed as they are. A FUNCTION that is not a function value is an error.
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
        _ => Err(Error::void_function(function)),
    }
}

/// Makes a closure of TEMPLATE: its shared body, with the variables it
/// captures from the running frame.
fn make_lambda(ctx: &TulispContext, template: &LambdaTemplate) -> Result<TulispObject, Error> {
    let mut captures = Vec::with_capacity(template.capture_sources.len());
    for (source, name) in &template.capture_sources {
        let value = match source {
            // A shared variable's slot holds its cell; any other's, its
            // value.
            CaptureSource::Local(n) => match slot(&ctx.vm, *n)? {
                Slot::Cell(cell) => CapturedValue::Cell(cell.clone()),
                Slot::Value(value) => CapturedValue::Value(value.clone()),
                Slot::Empty => CapturedValue::Value(TulispObject::nil()),
            },
            CaptureSource::Capture(index) => capture(&ctx.vm.captures, *index)?.value.clone(),
        };
        captures.push(Captured {
            value,
            name: name.clone(),
        });
    }
    let function = template.function.with_captures(Captures::new(captures));
    Ok(TulispValue::CompiledDefun { value: function }.into_ref(None))
}

/// The cell of the captured variable at INDEX of CAPTURES, which a
/// `setq` sets.
fn capture_cell(captures: &Captures, index: u16) -> Result<&crate::bytecode::Cell, Error> {
    match &capture(captures, index)?.value {
        CapturedValue::Cell(cell) => Ok(cell),
        CapturedValue::Value(_) => Err(Error::lisp_error(
            "internal: a set of a captured variable nobody assigns",
        )),
    }
}

/// Slot N of the running frame.
#[inline(always)]
fn slot_mut(vm: &mut Machine, n: u16) -> Result<&mut Slot, Error> {
    let index = vm.base + usize::from(n);
    vm.locals.get_mut(index).ok_or_else(slot_past_frame)
}

/// Slot N of the running frame, to read.
#[inline(always)]
fn slot(vm: &Machine, n: u16) -> Result<&Slot, Error> {
    vm.locals
        .get(vm.base + usize::from(n))
        .ok_or_else(slot_past_frame)
}

/// The cell in slot N of the running frame.
#[inline(always)]
fn slot_cell(vm: &Machine, n: u16) -> Result<&crate::bytecode::Cell, Error> {
    match slot(vm, n)? {
        Slot::Cell(cell) => Ok(cell),
        Slot::Value(_) | Slot::Empty => Err(Error::lisp_error(
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
            assert!(err.to_string().contains("invalid function: raw"));
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
        {
            let seen = seen.lock().unwrap();
            assert_eq!(seen.len(), 51);
            assert!(seen.windows(2).all(|w| w[0] == w[1]), "{seen:?}");
        }
        // Two functions tail-calling each other replace each other's
        // frame too.
        seen.lock().unwrap().clear();
        ctx.eval_string(
            "(defun ping (n) (let ((a n)) (note-locals) (if (= n 0) 0 (pong (- n 1)))))
             (defun pong (n) (let ((b n) (c n)) (note-locals) (ping n)))
             (ping 20)",
        )
        .unwrap();
        let seen = seen.lock().unwrap();
        assert_eq!(seen.len(), 41);
        assert!(
            seen.iter().all(|n| *n == seen[0] || *n == seen[1]),
            "{seen:?}"
        );
        assert!(seen.windows(3).all(|w| w[0] == w[2]), "{seen:?}");
    }

    /// A context where `(make-token)` makes a host value and
    /// `(tokens-dropped)` counts how many such values were let go.
    fn token_context() -> TulispContext {
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
        let mut ctx = TulispContext::new();
        let made = dropped.clone();
        ctx.defun("make-token", move || Token(made.clone()));
        ctx.defun("tokens-dropped", move || {
            dropped.load(Ordering::SeqCst) as i64
        });
        ctx
    }

    // A `let` lets go of its values when it ends, not when its function
    // returns.
    #[test]
    fn a_let_lets_go_of_its_values_when_it_ends() {
        let ctx = &mut token_context();
        eval_assert_equal(
            ctx,
            "(defun f () (let ((x (make-token))) nil) (tokens-dropped)) (f)",
            "1",
        );
    }

    // A run of a special form's form that an error ends lets go of its
    // `let` values too.
    #[test]
    fn a_form_run_ended_by_an_error_lets_go_of_its_values() {
        let ctx = &mut token_context();
        ctx.defspecial(
            "run-twice",
            |ctx: &mut TulispContext, body: Form| -> Result<TulispObject, Error> {
                let _ = body.eval(ctx);
                body.eval(ctx)
            },
        );
        eval_assert_equal(
            ctx,
            "(setq n 0)
             (defun f ()
               (run-twice
                (let ((x (make-token)))
                  (setq n (1+ n))
                  (when (= n 1) (error \"boom\"))
                  n))
               (tokens-dropped))
             (f)",
            "2",
        );
    }

    // A special `let` in a `catch` or `condition-case` body is undone
    // when a throw or an error leaves the body, before the handler runs.
    #[test]
    fn special_lets_unwind_out_of_blocks() {
        let ctx = &mut TulispContext::new();
        ctx.eval_string("(defvar sp 'outer)").unwrap();
        let before = ctx.debug_special_stacks_total();
        eval_assert_equal(
            ctx,
            "(list (catch 'k (let ((sp 'inner)) (throw 'k sp)))
                   (condition-case nil (let ((sp 'inner)) (error \"x\")) (error sp))
                   (condition-case nil
                       (funcall (lambda () (let ((sp 'inner)) (error \"y\"))))
                     (error sp))
                   sp)",
            "'(inner outer outer outer)",
        );
        assert_eq!(ctx.debug_special_stacks_total(), before);
    }
}
