use super::{
    Instruction, LambdaTemplate, bytecode::Bytecode, bytecode::CompiledDefun, bytecode::TraceRange,
    compiler::VMDefunParams,
};
use crate::{
    Error, Number, TulispContext, TulispObject, TulispValue, bytecode::Pos, plist,
    object::wrappers::generic::SharedMut,
};
use std::collections::HashMap;

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

/// Per-frame Drop guard for `BeginScope` bindings — `let` / `let*` /
/// `dolist` / `dotimes` / inline `lambda` body bindings. Mirrors
/// `SetParams` (function params) and `LexScopeGuard` (TW path).
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
    bytecode: Bytecode,
    labels: HashMap<usize, usize>, // TulispObject.addr -> instruction index
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
                Pos::Label(p) => {
                    let abs_pos = *$ctx.vm.labels.get(&p.addr_as_usize()).unwrap();
                    *$pos = Pos::Abs(abs_pos); // TODO: uncomment
                    abs_pos
                }
            }
        }
    };
}

impl Machine {
    pub(crate) fn new() -> Self {
        Machine {
            stack: Vec::new(),
            bytecode: Bytecode::new(),
            labels: HashMap::new(),
        }
    }
}

fn locate_labels(bytecode: &Bytecode) -> HashMap<usize, usize> {
    // TODO: intern-soft and make sure that the labels are unique
    let mut labels = HashMap::new();
    for (i, instr) in bytecode.global.borrow().iter().enumerate() {
        if let Instruction::Label(name) = instr {
            labels.insert(name.addr_as_usize(), i + 1);
        }
    }
    for func in bytecode.functions.values() {
        for (i, instr) in func.instructions.borrow().iter().enumerate() {
            if let Instruction::Label(name) = instr {
                labels.insert(name.addr_as_usize(), i + 1);
            }
        }
    }
    labels
}

#[allow(dead_code)]
fn print_stack(ctx: &TulispContext, func: Option<usize>, pc: usize, recursion_depth: u32) {
    println!("Stack:");
    for obj in ctx.vm.stack.iter() {
        println!("  {}", obj);
    }
    println!(
        "\nDepth: {}: PC: {}; Executing: {}",
        recursion_depth,
        pc,
        if let Some(func) = func {
            ctx.vm
                .bytecode
                .functions
                .get(&func)
                .unwrap()
                .instructions
                .borrow()[pc]
                .clone()
        } else {
            ctx.vm.bytecode.global.borrow()[pc].clone()
        }
    );
}

pub fn run(ctx: &mut TulispContext, bytecode: Bytecode) -> Result<TulispObject, Error> {
    let labels = locate_labels(&bytecode);
    ctx.vm.labels.extend(labels);
    ctx.vm.bytecode.import_functions(&bytecode);
    ctx.vm.bytecode.global = bytecode.global;
    ctx.vm.bytecode.global_trace_ranges = bytecode.global_trace_ranges;
    let global_program = ctx.vm.bytecode.global.clone();
    let global_ranges = ctx.vm.bytecode.global_trace_ranges.clone();
    run_impl(ctx, &global_program, global_ranges.as_slice(), 0)?;
    // When the top-level form has no value (e.g., a program of
    // only `defun`s), the compiler emits no trailing Push — the
    // stack is empty, not underflowed. Return nil in that case.
    Ok(ctx.vm.stack.pop().unwrap_or_else(TulispObject::nil))
}

/// Invoke a VM-compiled lambda with already-evaluated args. Used by
/// `eval::funcall` when it encounters a `TulispValue::CompiledDefun`.
pub(crate) fn run_lambda(
    ctx: &mut TulispContext,
    compiled: &CompiledDefun,
    args: &[TulispObject],
) -> Result<TulispObject, Error> {
    // Make sure the closure's `Instruction::Label` positions are
    // present in *this* ctx's `vm.labels`. The parent ctx that
    // compiled the closure registered them at `MakeLambda` time
    // (`register_lambda_labels`), but a host that hands the closure
    // off to a fresh ctx — `tulisp-async`'s per-firing timer ctx is
    // the canonical case — never saw that registration. Without
    // this, any `Pos::Label` jump emitted by `cond` / `and` / `or`
    // would panic in `jump_to_pos!`. Idempotent for the same-ctx
    // case (same key, same value).
    register_compiled_labels(ctx, compiled);
    let required = compiled.params.required.len();
    let optional = compiled.params.optional.len();
    let has_rest = compiled.params.rest.is_some();

    if args.len() < required {
        return Err(Error::missing_argument("Too few arguments".to_string()));
    }
    if !has_rest && args.len() > required + optional {
        return Err(Error::invalid_argument("Too many arguments".to_string()));
    }

    let left_args = args.len() - required;
    let (optional_count, rest_count) = if left_args > optional {
        (optional, left_args - optional)
    } else {
        (left_args, 0)
    };

    // Push args in order; `init_defun_args` pops them in reverse
    // to match `params.required` + `params.optional` + `rest` layout.
    for a in args {
        ctx.vm.stack.push(a.clone());
    }

    // Use the same trampoline the `Call` handler uses so tail calls
    // from the lambda body unwind without Rust-stack growth.
    let mut current = compiled.clone();
    let mut current_optional = optional_count;
    let mut current_rest = rest_count;
    loop {
        let params = init_defun_args(ctx, &current.params, &current_optional, &current_rest)?;
        let tail = run_function(
            ctx,
            &current.instructions,
            current.trace_ranges.as_slice(),
            1,
        )?;
        drop(params);
        match tail {
            Some(info) => {
                current = info.function;
                current_optional = info.optional_count;
                current_rest = info.rest_count;
            }
            None => break,
        }
    }

    Ok(ctx.vm.stack.pop().unwrap())
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
    recursion_depth: u32,
) -> Result<Option<TailCallInfo>, Error> {
    let mut pc: usize = 0;
    match run_impl_inner(ctx, program, &mut pc, recursion_depth) {
        Ok(v) => Ok(v),
        Err(mut e) => {
            // `strip_trace_markers` pushes ranges as it encounters
            // each closing `PopTrace`, so the vector is sorted
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
    recursion_depth: u32,
) -> Result<Option<TailCallInfo>, Error> {
    let mut pc: usize = 0;
    let program_size = program.borrow().len();
    let mut instr_ref = program.borrow_mut();
    let mut active = ActiveScopes::new();
    while pc < program_size {
        // drop(instr_ref);
        // self.print_stack(func, pc, recursion_depth);
        // instr_ref = program.borrow_mut();

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
                    BinaryOp::Div => {
                        // Match Emacs: error only when *both* operands
                        // are integers and the divisor is zero (would
                        // underlying-panic on `i64::div`); any float
                        // operand falls through to `Number::Div` which
                        // produces ±inf for zero divisors, matching
                        // Emacs' `1.0e+INF` shape.
                        let an = a.as_number()?;
                        let bn = b.as_number()?;
                        if matches!((an, bn), (Number::Int(_), Number::Int(0))) {
                            return Err(Error::out_of_range("Division by zero"));
                        }
                        (an / bn).into()
                    }
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
                // in the loaded file register in `bytecode.functions`
                // and subsequent calls dispatch directly via the
                // `Call` instruction (same as if they had been
                // written in the outer file).
                drop(instr_ref);
                let result = vm_eval_file_inline(ctx, full_path)?;
                instr_ref = program.borrow_mut();
                ctx.vm.stack.push(result);
            }
            Instruction::PrintPop => {
                let a = ctx.vm.stack.pop().unwrap();
                println!("{}", a.fmt_string());
            }
            Instruction::Print => {
                let a = ctx.vm.stack.last().unwrap();
                println!("{}", a.fmt_string());
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
            Instruction::JumpIfNeq(pos) => {
                let minus2 = ctx.vm.stack.len() - 2;
                let [ref b, ref a] = ctx.vm.stack[minus2..] else {
                    unreachable!()
                };
                let cmp = !a.eq(b);
                ctx.vm.stack.truncate(minus2);
                if cmp {
                    jump_to_pos!(ctx, pc, pos);
                    continue;
                }
            }
            Instruction::JumpIfLt(pos) => {
                let minus2 = ctx.vm.stack.len() - 2;
                let [ref b, ref a] = ctx.vm.stack[minus2..] else {
                    unreachable!()
                };
                let cmp = compare_op(a, b, |a, b| a < b)?;
                ctx.vm.stack.truncate(minus2);
                if cmp {
                    jump_to_pos!(ctx, pc, pos);
                    continue;
                }
            }
            Instruction::JumpIfLtEq(pos) => {
                let minus2 = ctx.vm.stack.len() - 2;
                let [ref b, ref a] = ctx.vm.stack[minus2..] else {
                    unreachable!()
                };
                let cmp = compare_op(a, b, |a, b| a <= b)?;
                ctx.vm.stack.truncate(minus2);
                if cmp {
                    jump_to_pos!(ctx, pc, pos);
                    continue;
                }
            }
            Instruction::JumpIfGt(pos) => {
                let minus2 = ctx.vm.stack.len() - 2;
                let [ref b, ref a] = ctx.vm.stack[minus2..] else {
                    unreachable!()
                };
                let cmp = compare_op(a, b, |a, b| a > b)?;
                ctx.vm.stack.truncate(minus2);
                if cmp {
                    jump_to_pos!(ctx, pc, pos);
                    continue;
                }
            }
            Instruction::JumpIfGtEq(pos) => {
                let minus2 = ctx.vm.stack.len() - 2;
                let [ref b, ref a] = ctx.vm.stack[minus2..] else {
                    unreachable!()
                };
                let cmp = compare_op(a, b, |a, b| a >= b)?;
                ctx.vm.stack.truncate(minus2);
                if cmp {
                    jump_to_pos!(ctx, pc, pos);
                    continue;
                }
            }
            Instruction::Jump(pos) => {
                jump_to_pos!(ctx, pc, pos);
                continue;
            }
            Instruction::Equal => {
                let a = ctx.vm.stack.pop().unwrap();
                let b = ctx.vm.stack.pop().unwrap();
                ctx.vm.stack.push(a.equal(&b).into());
            }
            Instruction::Eq => {
                let a = ctx.vm.stack.pop().unwrap();
                let b = ctx.vm.stack.pop().unwrap();
                ctx.vm.stack.push(a.eq(&b).into());
            }
            Instruction::Lt => {
                let a = ctx.vm.stack.pop().unwrap();
                let b = ctx.vm.stack.pop().unwrap();
                ctx.vm.stack.push(compare_op(&a, &b, |a, b| a < b)?.into());
            }
            Instruction::LtEq => {
                let a = ctx.vm.stack.pop().unwrap();
                let b = ctx.vm.stack.pop().unwrap();
                ctx.vm.stack.push(compare_op(&a, &b, |a, b| a <= b)?.into());
            }
            Instruction::Gt => {
                let a = ctx.vm.stack.pop().unwrap();
                let b = ctx.vm.stack.pop().unwrap();
                ctx.vm.stack.push(compare_op(&a, &b, |a, b| a > b)?.into());
            }
            Instruction::GtEq => {
                let a = ctx.vm.stack.pop().unwrap();
                let b = ctx.vm.stack.pop().unwrap();
                ctx.vm.stack.push(compare_op(&a, &b, |a, b| a >= b)?.into());
            }
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
                    if let Some(func) = ctx.vm.bytecode.functions.get(&addr) {
                        let func = func.clone();

                        if *args_count < func.params.required.len() {
                            return Err(Error::missing_argument("Too few arguments".to_string())
                                .with_trace(form.clone()));
                        }
                        if func.params.rest.is_none()
                            && *args_count > func.params.required.len() + func.params.optional.len()
                        {
                            return Err(Error::invalid_argument("Too many arguments".to_string())
                                .with_trace(form.clone()));
                        }
                        let left_args = *args_count - func.params.required.len();
                        if left_args > func.params.optional.len() {
                            *rest_count = left_args - func.params.optional.len();
                            *optional_count = func.params.optional.len();
                        } else if left_args > 0 {
                            *optional_count = left_args
                        }
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
                        let result = funcall_inline(ctx, &name, args, recursion_depth)
                            .map_err(|e| e.with_trace(form))?;
                        ctx.vm.stack.push(result);
                        instr_ref = program.borrow_mut();
                        pc += 1;
                        continue;
                    }
                }

                let mut current_function = function.as_ref().unwrap().clone();
                let mut current_optional = *optional_count;
                let mut current_rest = *rest_count;
                let form = form.clone();

                drop(instr_ref);
                loop {
                    let params = init_defun_args(
                        ctx,
                        &current_function.params,
                        &current_optional,
                        &current_rest,
                    )?;
                    let tail = run_function(
                        ctx,
                        &current_function.instructions,
                        current_function.trace_ranges.as_slice(),
                        recursion_depth + 1,
                    )
                    .map_err(|e| e.with_trace(form.clone()))?;
                    drop(params);

                    match tail {
                        Some(info) => {
                            current_function = info.function;
                            current_optional = info.optional_count;
                            current_rest = info.rest_count;
                        }
                        None => break,
                    }
                }
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
                    let Some(func) = ctx.vm.bytecode.functions.get(&addr) else {
                        return Err(Error::new(
                            crate::ErrorKind::Undefined,
                            format!("undefined function: {}", name),
                        )
                        .with_trace(form.clone()));
                    };
                    let func = func.clone();

                    if *args_count < func.params.required.len() {
                        return Err(Error::missing_argument("Too few arguments".to_string())
                            .with_trace(form.clone()));
                    }
                    if func.params.rest.is_none()
                        && *args_count > func.params.required.len() + func.params.optional.len()
                    {
                        return Err(Error::invalid_argument("Too many arguments".to_string())
                            .with_trace(form.clone()));
                    }
                    let left_args = *args_count - func.params.required.len();
                    if left_args > func.params.optional.len() {
                        *rest_count = left_args - func.params.optional.len();
                        *optional_count = func.params.optional.len();
                    } else if left_args > 0 {
                        *optional_count = left_args
                    }
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
                register_lambda_labels(ctx, &closure);
                ctx.vm.stack.push(closure);
            }
            Instruction::Funcall { args_count } => {
                let args_count = *args_count;
                let split_at = ctx.vm.stack.len() - args_count;
                let args: Vec<TulispObject> = ctx.vm.stack.drain(split_at..).collect();
                let func = ctx.vm.stack.pop().unwrap();
                drop(instr_ref);
                let result = funcall_inline(ctx, &func, args, recursion_depth)?;
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
                let mut iter = final_list.clone();
                while iter.consp() {
                    args.push(iter.car()?);
                    iter = iter.cdr()?;
                }
                if !iter.null() {
                    return Err(Error::type_mismatch(format!(
                        "apply: last argument must be a proper list, got non-nil tail: {iter}"
                    )));
                }
                drop(instr_ref);
                let result = funcall_inline(ctx, &func, args, recursion_depth)?;
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
                let result = func(ctx, &args).map_err(|e| e.with_trace(form.clone()))?;
                if *keep_result {
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
                let result = call(ctx, &args).map_err(|e| e.with_trace(form.clone()))?;
                if *keep_result {
                    ctx.vm.stack.push(result);
                }
            }
            // `PushTrace` / `PopTrace` should never reach the
            // interpreter: `strip_trace_markers` removes them
            // at compile time and lifts the form spans into a
            // `TraceRange` side-table consulted by `run_impl`
            // on the error path.
            Instruction::PushTrace(_) | Instruction::PopTrace => {
                debug_assert!(
                    false,
                    "trace marker reached interpreter; strip_trace_markers should have lifted it",
                );
            }
            Instruction::Label(_) => {}
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
                // Emacs `append`: copy every arg except the last;
                // share the last arg's cells with the result.
                let mut iter = ctx.vm.stack.drain(ctx.vm.stack.len() - *len..);
                let result = if let Some(last) = iter.next_back() {
                    let last: TulispObject = last;
                    let mut builder = crate::cons::ListBuilder::new();
                    for arg in iter.by_ref() {
                        let arg: TulispObject = arg;
                        if !arg.listp() {
                            return Err(Error::type_mismatch(format!(
                                "append: expected list, got: {arg}"
                            )));
                        }
                        for elem in arg.base_iter() {
                            builder.push(elem);
                        }
                    }
                    builder.build_with_tail(last)
                } else {
                    TulispObject::nil()
                };
                drop(iter);
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
        }
        pc += 1;
    }
    Ok(None)
}

fn init_defun_args(
    ctx: &mut TulispContext,
    params: &VMDefunParams,
    optional_count: &usize,
    rest_count: &usize,
) -> Result<SetParams, Error> {
    let mut set_params = SetParams::new();
    if let Some(rest) = &params.rest {
        let mut rest_value = TulispObject::nil();
        for _ in 0..*rest_count {
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
        let val = if ii >= *optional_count {
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

fn run_function(
    ctx: &mut TulispContext,
    instructions: &SharedMut<Vec<Instruction>>,
    trace_ranges: &[TraceRange],
    recursion_depth: u32,
) -> Result<Option<TailCallInfo>, Error> {
    run_impl(ctx, instructions, trace_ranges, recursion_depth)
}

/// In-VM `funcall` dispatch used by `Instruction::Funcall`. Args are
/// already fully evaluated — `ctx.vm` is actively borrowed by the
/// caller, so we can't go through `eval::funcall` (it would
/// re-borrow for the VM path). Instead we dispatch each callable
/// variant using the machine we already have.
fn funcall_inline(
    ctx: &mut TulispContext,
    func: &TulispObject,
    args: Vec<TulispObject>,
    recursion_depth: u32,
) -> Result<TulispObject, Error> {
    // `(funcall 'funcall fn …)` — unwrap the redundant outer
    // `funcall`. If we didn't, the symbol would eval to the
    // `funcall` defspecial `Func` and we'd fall through to the
    // Lambda/Func arm below, which hands control back to
    // `eval::funcall`. That in turn would dispatch the *real*
    // `fn` via `ctx.vm.borrow_mut()`, deadlocking on the lock
    // we're currently inside. Peel one layer: the first arg is
    // the new func, the rest are its args.
    if func.eq(&ctx.keywords.funcall) && !args.is_empty() {
        let mut args = args;
        let inner_func = args.remove(0);
        return funcall_inline(ctx, &inner_func, args, recursion_depth);
    }
    // Mirror the `funcall` defspecial's double-eval: a bare symbol
    // resolves to its bound function; a list such as `(lambda …)`
    // passed as-is (from `(funcall '(lambda …) …)`) needs the
    // inner form executed to become a Lambda value.
    let resolved = if (func.symbolp() && !func.keywordp()) || func.consp() {
        ctx.eval(func)?
    } else {
        func.clone()
    };
    let inner = resolved.inner_ref();
    match &inner.0 {
        TulispValue::CompiledDefun { value } => {
            let cd = value.clone();
            drop(inner);
            run_lambda_with(ctx, &cd, args, recursion_depth)
        }
        TulispValue::Defun { call, arity } => {
            // Args are already evaluated values from the VM stack
            // — hand them straight to the typed-args closure.
            // No ctx.vm.borrow_mut() re-entry: we're using the
            // closure's `&[TulispObject]` shape directly.
            //
            // Arity check mirrors `eval::funcall`'s `Defun` arm —
            // the typed closure's `@bind` macro indexes
            // `&args[..required]` without bounds-checking, so a
            // too-few-args call would panic without this gate
            // (e.g. `(funcall '+)` would crash inside the macro).
            let call = call.clone();
            let arity = arity.clone();
            drop(inner);
            if args.len() < arity.required {
                return Err(Error::missing_argument("Too few arguments".to_string()));
            }
            if !arity.has_rest && args.len() > arity.required + arity.optional {
                return Err(Error::invalid_argument("Too many arguments".to_string()));
            }
            call(ctx, &args)
        }
        TulispValue::Lambda { .. } | TulispValue::Func(_) => {
            drop(inner);
            // Rebuild an arg list TulispObject (quoted so the TW
            // side doesn't re-evaluate already-resolved values).
            let list = TulispObject::nil();
            for a in args {
                list.push(TulispValue::Quote { value: a }.into_ref(None))?;
            }
            crate::eval::funcall::<crate::eval::Eval>(ctx, &resolved, &list)
        }
        _ => Err(Error::undefined(format!("function is void: {}", resolved))),
    }
}

/// Internal variant of `run_lambda` used when we're already inside
/// a VM run and have `&mut self` on the current machine.
fn run_lambda_with(
    ctx: &mut TulispContext,
    compiled: &CompiledDefun,
    args: Vec<TulispObject>,
    recursion_depth: u32,
) -> Result<TulispObject, Error> {
    // See `run_lambda` for why — closures invoked from a fresh ctx
    // need their labels registered before any `Pos::Label` jump runs.
    register_compiled_labels(ctx, compiled);
    let required = compiled.params.required.len();
    let optional = compiled.params.optional.len();
    let has_rest = compiled.params.rest.is_some();

    if args.len() < required {
        return Err(Error::missing_argument("Too few arguments".to_string()));
    }
    if !has_rest && args.len() > required + optional {
        return Err(Error::invalid_argument("Too many arguments".to_string()));
    }

    let left_args = args.len() - required;
    let (optional_count, rest_count) = if left_args > optional {
        (optional, left_args - optional)
    } else {
        (left_args, 0)
    };

    for a in args {
        ctx.vm.stack.push(a);
    }

    let mut current = compiled.clone();
    let mut current_optional = optional_count;
    let mut current_rest = rest_count;
    loop {
        let params = init_defun_args(ctx, &current.params, &current_optional, &current_rest)?;
        let tail = run_function(
            ctx,
            &current.instructions,
            current.trace_ranges.as_slice(),
            recursion_depth + 1,
        )?;
        drop(params);
        match tail {
            Some(info) => {
                current = info.function;
                current_optional = info.optional_count;
                current_rest = info.rest_count;
            }
            None => break,
        }
    }
    Ok(ctx.vm.stack.pop().unwrap())
}

/// In-VM version of `ctx.eval_file` — parses & compiles the given
/// file, merges its labels + `bytecode.functions` into the running
/// machine, and evaluates its top-level forms on the current stack.
/// Unlike the external `eval_file`, this doesn't call
/// `vm.borrow_mut().run(…)` — we're already holding `&mut self`.
fn vm_eval_file_inline(ctx: &mut TulispContext, path: &str) -> Result<TulispObject, Error> {
    let ast = ctx.parse_file(path)?;
    let bytecode = crate::bytecode::compile(ctx, &ast)?;
    let labels = locate_labels(&bytecode);
    ctx.vm.labels.extend(labels);
    ctx.vm.bytecode.import_functions(&bytecode);
    let sub_global = bytecode.global.clone();
    let sub_ranges = bytecode.global_trace_ranges.clone();
    run_impl(ctx, &sub_global, sub_ranges.as_slice(), 1)?;
    // `compile_progn` only keeps the result of the last form on the
    // stack; for forms that produced no value (e.g., a `defun`) we
    // return nil.
    Ok(ctx.vm.stack.pop().unwrap_or_else(TulispObject::nil))
}

/// After phase-2 materialization, the closure's `Instruction::Label`
/// positions need to be known to the running machine so that
/// `Pos::Label` jumps (emitted by `and`/`or`/`cond`/etc.) can resolve.
/// `locate_labels` only walks the top-level bytecode, not the lambda
/// templates nested inside `MakeLambda`, so register them here when a
/// closure is built.
fn register_lambda_labels(ctx: &mut TulispContext, closure: &TulispObject) {
    let inner = closure.inner_ref();
    let TulispValue::CompiledDefun { value } = &inner.0 else {
        return;
    };
    let instructions = value.instructions.clone();
    drop(inner);
    let borrow = instructions.borrow();
    for (i, instr) in borrow.iter().enumerate() {
        if let Instruction::Label(name) = instr {
            ctx.vm.labels.insert(name.addr_as_usize(), i + 1);
        }
    }
}

/// Like `register_lambda_labels`, but takes a `CompiledDefun`
/// directly (no enclosing TulispObject). Called by `run_lambda` /
/// `run_lambda_with` so a closure invoked through a ctx that didn't
/// see its `MakeLambda` (e.g. tulisp-async's per-firing timer ctx)
/// still has its `Pos::Label` jumps resolvable.
fn register_compiled_labels(ctx: &mut TulispContext, compiled: &CompiledDefun) {
    let borrow = compiled.instructions.borrow();
    for (i, instr) in borrow.iter().enumerate() {
        if let Instruction::Label(name) = instr {
            ctx.vm.labels.insert(name.addr_as_usize(), i + 1);
        }
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

    let mut instructions = template.instructions.clone();
    for insn in instructions.iter_mut() {
        rewrite_instruction(insn, &mapping, &rewrite);
    }

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
        instructions: SharedMut::new(instructions),
        // PCs are unchanged by `rewrite_instruction` (it only
        // swaps placeholder objects, never adds or removes
        // instructions), so the template's trace ranges remain
        // valid for the materialized closure. Wrap in a `Shared`
        // since `LambdaTemplate::trace_ranges` is owned.
        trace_ranges: crate::object::wrappers::generic::Shared::new_sized(
            template.trace_ranges.clone(),
        ),
        params,
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
) {
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
                *o = rewrite_ast(o, mapping);
            }
        Instruction::MakeLambda(template) => {
            let rebuilt = rewrite_template(template, mapping);
            *template = crate::object::wrappers::generic::Shared::new_sized(rebuilt);
        }
        _ => {}
    }
}

/// Quick check: does `obj` or any cons-cell descendant reference a
/// placeholder? Avoids the expensive deep-copy when Push holds plain
/// literals (numbers, strings, etc.).
fn ast_contains_placeholder(obj: &TulispObject, mapping: &HashMap<usize, TulispObject>) -> bool {
    if mapping.contains_key(&obj.addr_as_usize()) {
        return true;
    }
    if obj.consp() {
        let mut cur = obj.clone();
        while cur.consp() {
            let Ok(car) = cur.car() else { break };
            if ast_contains_placeholder(&car, mapping) {
                return true;
            }
            let Ok(next) = cur.cdr() else { break };
            if !next.consp() {
                if mapping.contains_key(&next.addr_as_usize()) {
                    return true;
                }
                break;
            }
            cur = next;
        }
    }
    false
}

/// Deep-clone `obj`, substituting any placeholder reference with the
/// mapped binding. Only descends through cons lists; quoted literals
/// and other value kinds pass through unchanged.
fn rewrite_ast(obj: &TulispObject, mapping: &HashMap<usize, TulispObject>) -> TulispObject {
    if let Some(replacement) = mapping.get(&obj.addr_as_usize()) {
        return replacement.clone();
    }
    if obj.consp() {
        let span = obj.span();
        let mut builder = crate::cons::ListBuilder::new();
        let mut cur = obj.clone();
        loop {
            let Ok(car) = cur.car() else { break };
            builder.push(rewrite_ast(&car, mapping));
            let Ok(next) = cur.cdr() else { break };
            if next.null() {
                break;
            }
            if !next.consp() {
                let tail = mapping
                    .get(&next.addr_as_usize())
                    .cloned()
                    .unwrap_or_else(|| next.clone());
                let _ = builder.append(tail);
                break;
            }
            cur = next;
        }
        return builder.build().with_span(span);
    }
    obj.clone()
}

/// Clone `template` but with every `(orig, placeholder)` in `free_vars`
/// whose `orig` appears in `mapping` rewritten to the mapped binding —
/// and with nested `MakeLambda` instructions in the body recursively
/// rebuilt the same way. The inner placeholder keys themselves stay
/// intact so the inner's phase-2 rewrite still finds them.
fn rewrite_template(
    template: &LambdaTemplate,
    mapping: &HashMap<usize, TulispObject>,
) -> LambdaTemplate {
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

    let mut new_instructions = template.instructions.clone();
    for insn in new_instructions.iter_mut() {
        rewrite_instruction(insn, mapping, &rewrite);
    }

    LambdaTemplate {
        instructions: new_instructions,
        // `rewrite_instruction` swaps in-place; PCs are stable so
        // the inner template's trace ranges still apply.
        trace_ranges: template.trace_ranges.clone(),
        param_placeholders: template.param_placeholders.clone(),
        params: template.params.clone(),
        free_vars: new_free_vars,
    }
}
