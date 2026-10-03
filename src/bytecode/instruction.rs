use crate::{
    Error, Number, TulispObject,
    object::wrappers::{DefunFn, SpecialFn, generic::Shared},
};

use super::block::{Block, FormBlock, Handler};
use super::bytecode::CompiledDefun;
use super::lambda_template::LambdaTemplate;

#[derive(Clone)]
pub(crate) enum Pos {
    Abs(usize),
    Rel(isize),
    Label(TulispObject),
}

impl std::fmt::Display for Pos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Pos::Abs(p) => write!(f, "{}", p),
            Pos::Rel(p) => write!(f, ". {}", p),
            Pos::Label(p) => write!(f, "{}", p),
        }
    }
}

#[derive(Clone, Copy)]
pub(crate) enum Cxr {
    Car,
    Cdr,
    Caar,
    Cadr,
    Cdar,
    Cddr,
    Caaar,
    Caadr,
    Cadar,
    Caddr,
    Cdaar,
    Cdadr,
    Cddar,
    Cdddr,
    Caaaar,
    Caaadr,
    Caadar,
    Caaddr,
    Cadaar,
    Cadadr,
    Caddar,
    Cadddr,
    Cdaaar,
    Cdaadr,
    Cdadar,
    Cdaddr,
    Cddaar,
    Cddadr,
    Cdddar,
    Cddddr,
}

/// An arithmetic [`BinaryOp`](Instruction::BinaryOp) or
/// [`ArithChain`](Instruction::ArithChain).
#[derive(Clone, Copy)]
pub(crate) enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

impl BinaryOp {
    /// A OP B. Division errors on an integer zero divisor and on
    /// `i64::MIN / -1`; a float operand gives ±inf for a zero divisor,
    /// as in Emacs.
    pub(crate) fn apply(self, a: Number, b: Number) -> Result<Number, Error> {
        match self {
            BinaryOp::Add => a.checked_add(b),
            BinaryOp::Sub => a.checked_sub(b),
            BinaryOp::Mul => a.checked_mul(b),
            BinaryOp::Div => a.checked_div(b),
        }
    }

    /// OP folded over FIRST and REST, from FIRST, each converted to a
    /// number as the fold reaches it. A float anywhere makes all of a
    /// division float, as in Emacs: `(/ 7 2 2.0)` is 1.75.
    pub(crate) fn fold(self, first: &TulispObject, rest: &[TulispObject]) -> Result<Number, Error> {
        let mut value = first.as_number()?;
        if matches!(self, BinaryOp::Div) && rest.iter().any(TulispObject::floatp) {
            value = value.to_float();
        }
        for arg in rest {
            value = self.apply(value, arg.as_number()?)?;
        }
        Ok(value)
    }

    fn mnemonic(self) -> &'static str {
        match self {
            BinaryOp::Add => "add",
            BinaryOp::Sub => "sub",
            BinaryOp::Mul => "mul",
            BinaryOp::Div => "div",
        }
    }
}

/// The comparison of a [`CompareChain`](Instruction::CompareChain).
#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) enum Comparison {
    Lt,
    LtEq,
    Gt,
    GtEq,
}

impl Comparison {
    pub(crate) fn holds(self, a: Number, b: Number) -> bool {
        match self {
            Comparison::Lt => a < b,
            Comparison::LtEq => a <= b,
            Comparison::Gt => a > b,
            Comparison::GtEq => a >= b,
        }
    }

    fn mnemonic(self) -> &'static str {
        match self {
            Comparison::Lt => "clt",
            Comparison::LtEq => "cle",
            Comparison::Gt => "cgt",
            Comparison::GtEq => "cge",
        }
    }
}

/// A single instruction in the VM.
#[derive(Clone)]
pub(crate) enum Instruction {
    // stack
    Push(TulispObject),
    Pop,
    // variables
    /// Pop a value, on top, and a symbol below it, set the symbol's
    /// value to it, and push the value back.
    Set,
    /// Like `Set`, without pushing the value back.
    SetPop,
    StorePop(TulispObject),
    Store(TulispObject),
    Load(TulispObject),
    /// Pop a value into the running frame's slot `n`: a lexical
    /// variable's binding.
    BindLocal(u16),
    /// Pop a value into a fresh cell in slot `n`: the binding of a
    /// variable shared with a closure.
    BindCell(u16),
    /// Read or write the value in slot `n`.
    LoadLocal(u16),
    StoreLocal(u16),
    StorePopLocal(u16),
    /// Read or write through the cell in slot `n`.
    LoadCell(u16),
    StoreCell(u16),
    StorePopCell(u16),
    /// Let go of the values in slots `from..to`, at the end of a `let`.
    ClearLocals {
        from: u16,
        to: u16,
    },
    /// Read the running closure's captured variable at this index, or
    /// write it through its cell.
    LoadCapture(u16),
    StoreCapture(u16),
    StorePopCapture(u16),
    BeginScope(TulispObject),
    EndScope(TulispObject),
    // arithmetic
    /// Pop two values and push OP of them. The one below the top is the
    /// first operand, evaluated first.
    BinaryOp(BinaryOp),
    /// Pop `count` values and push OP folded over them from the first,
    /// the deepest: `(- a b c)` is `(a - b) - c`. Every argument has run
    /// before any of the arithmetic does.
    ArithChain {
        op: BinaryOp,
        count: usize,
    },
    // io
    LoadFile,
    PrintPop,
    Print,
    // comparison: pop two values and push whether the lower one
    // compares to the top one as named. `(< a b)` pushes `a`, then
    // `b`, so its arguments run left to right. The fused jumps below
    // read their operands the same way.
    Equal,
    Eq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    // predicates
    Null,
    // control flow
    JumpIfNil(Pos),
    JumpIfNotNil(Pos),
    JumpIfNilElsePop(Pos),
    JumpIfNotNilElsePop(Pos),
    JumpIfNeq(Pos),
    JumpIfEq(Pos),
    JumpIfEqual(Pos),
    JumpIfNotEqual(Pos),
    JumpIfLt(Pos),
    JumpIfLtEq(Pos),
    JumpIfGt(Pos),
    JumpIfGtEq(Pos),
    JumpIfNotLt(Pos),
    JumpIfNotLtEq(Pos),
    JumpIfNotGt(Pos),
    JumpIfNotGtEq(Pos),
    Jump(Pos),
    /// Pops `count` values and pushes t when each of them compares
    /// to the next one as `comparison` says, in order from the
    /// bottom; nil at the first pair that does not. The arguments
    /// of `(< a b c)` and its friends, evaluated left to right.
    CompareChain {
        comparison: Comparison,
        count: usize,
    },
    // functions
    Label(TulispObject),
    /// A call to a `ctx.defun`-registered Rust function. Args have
    /// already been pushed on the stack (compiled with
    /// `keep_result=true`); the handler pops `args_count` of them in
    /// source order, hands them to the function in `call`, or calls the
    /// name the general way when it no longer holds a Rust function, and
    /// pushes the result if `keep_result`.
    RustCall {
        name: TulispObject,
        /// Source AST of the full call form (`(name args…)`), so an
        /// error from `call` has the call's `at (form)` trace line.
        form: TulispObject,
        /// The machine's `generation` when the call last looked the name
        /// up, with the Rust function it found, or `None` when the name
        /// held no Rust function. It is stale once `generation` moves.
        call: (u64, Option<Shared<dyn DefunFn>>),
        args_count: usize,
        keep_result: bool,
    },
    /// A call to a special form. Its `eager_count` evaluated arguments
    /// are on the stack; each unevaluated one is a block of its own.
    /// The handler pops the values, hands them and the forms to
    /// `call`, and pushes the result if `keep_result`.
    SpecialCall {
        name: TulispObject,
        /// See `RustCall::form`.
        form: TulispObject,
        /// The machine's `generation` when the call last looked the name
        /// up, with the special form it found. It is stale once
        /// `generation` moves.
        call: (u64, Shared<dyn SpecialFn>),
        /// The parameter kinds the call was compiled for.
        kinds: Shared<Vec<crate::ParamKind>>,
        eager_count: usize,
        blocks: Shared<Vec<FormBlock>>,
        keep_result: bool,
    },
    Call {
        name: TulispObject,
        /// See `RustCall::form`.
        form: TulispObject,
        args_count: usize,
        /// The function the call last reached, with the machine's
        /// `generation` then: a later replacement makes it stale.
        function: Option<(u64, CompiledDefun)>,
        optional_count: usize,
        rest_count: usize,
    },
    TailCall {
        name: TulispObject,
        /// The marked call, `(Bounce name args…)`, for the error trace;
        /// it prints as `(name args…)`. See `RustCall::form`.
        form: TulispObject,
        args_count: usize,
        /// The function the call last reached, with the machine's
        /// `generation` then: a later replacement makes it stale.
        function: Option<(u64, CompiledDefun)>,
        optional_count: usize,
        rest_count: usize,
    },
    /// Make a closure of a compiled `(lambda …)` body, with the
    /// variables it captures, and push it (as a
    /// `TulispValue::CompiledDefun`) on the stack.
    MakeLambda(Shared<LambdaTemplate>),
    /// Pops a function `MakeLambda` made for a `defun` that closes over
    /// variables, and makes it what the name runs, unless a later
    /// `defun` of the name has replaced the function of this form.
    DefineFunction(TulispObject),
    /// `(catch TAG BODY...)`: pops the tag, runs BODY, and pushes its
    /// value, or the value of a `throw` to the tag.
    Catch {
        body: Block,
    },
    /// `(unwind-protect BODYFORM UNWINDFORMS...)`: runs BODY, then
    /// CLEANUP whatever BODY did, and pushes BODY's value. An error or
    /// throw from CLEANUP replaces BODY's result.
    UnwindProtect {
        body: Block,
        cleanup: Block,
    },
    /// `(condition-case VAR BODYFORM HANDLERS...)`: runs BODY; on an
    /// error, runs the first handler whose condition matches, and pushes
    /// the value. When BINDS, the error data is pushed for the handler
    /// to bind to VAR.
    ConditionCase {
        binds: bool,
        body: Block,
        handlers: Shared<Vec<Handler>>,
    },
    /// `(defvar SYM ...)`: pushes whether SYM is bound, so the value is
    /// evaluated and stored only when it is not.
    DefVar(TulispObject),
    /// Raises an error the compiler found, once it is reached: a
    /// `condition-case` handler whose VAR is a constant.
    Raise(Box<crate::Error>),
    /// Inline `(funcall fn arg1 …)` dispatch. The function value is
    /// pushed first, then each arg, in source order (so at execution
    /// the top of stack is the last arg, `args_count + 1` below it is
    /// the function).
    Funcall {
        args_count: usize,
    },
    /// Inline `(apply fn arg1 … final-list)` dispatch. The function is
    /// pushed first, then each intermediate arg, then the final list
    /// (which must evaluate to a list at runtime; its elements are
    /// spliced after the intermediate args). `args_count` is the
    /// number of intermediate args — the final list is on top of the
    /// stack with `args_count + 1` items below it (the function and
    /// the intermediate args). Same anti-reborrow rationale as
    /// [`Funcall`](Self::Funcall).
    Apply {
        args_count: usize,
    },
    Ret,
    /// Push `form` onto the machine's `trace_stack`. Errors that
    /// propagate out of any subsequent instruction (until a matching
    /// `PopTrace`) get `form` appended to their backtrace by the
    /// `run_impl` wrapper. Emitted by `compile_expr` around every
    /// list-form's compiled bytecode.
    PushTrace(TulispObject),
    PopTrace,
    // lists
    Cons,
    List(usize),
    Append(usize),
    Cxr(Cxr),
    /// Pop a property, on top, and a plist below it, and push the
    /// property's value in the plist.
    PlistGet,
    // values
    Quote,
    /// Pop one value and push it wrapped in a `TulispValue::Backquote`.
    /// Emitted for nested backquotes — the inner `\`X` becomes
    /// `WrapBackquote` after compiling `X` at the bumped quasi-quote
    /// depth.
    WrapBackquote,
    /// Pop one value and push it wrapped in `TulispValue::Unquote`.
    /// Emitted at quasi-quote depth ≥ 2 for `,X` — the inner is
    /// compiled at `depth - 1` and the wrap re-emits the comma at
    /// the outer level.
    WrapUnquote,
    /// Pop one value and push it wrapped in `TulispValue::Splice`.
    /// Same as `WrapUnquote` but for `,@X` at quasi-quote depth ≥ 2,
    /// and for a `,@X` in a dotted tail at any depth.
    WrapSplice,
    /// Pop a list of one element and push that element, or with
    /// `empty_is_nil`, pop an empty list and push nil. Emitted before
    /// a `WrapUnquote`, `WrapSplice` or `Quote` whose content is a
    /// `,@Y` spliced at quasi-quote depth 1, as in `,,@y`.
    SoleElement {
        empty_is_nil: bool,
    },
}

/// The `Pos` inside a jump instruction, shared by `pos` and `pos_mut`.
macro_rules! jump_pos {
    ($instr:expr) => {
        match $instr {
            Instruction::JumpIfNil(p)
            | Instruction::JumpIfNotNil(p)
            | Instruction::JumpIfNilElsePop(p)
            | Instruction::JumpIfNotNilElsePop(p)
            | Instruction::JumpIfNeq(p)
            | Instruction::JumpIfEq(p)
            | Instruction::JumpIfEqual(p)
            | Instruction::JumpIfNotEqual(p)
            | Instruction::JumpIfLt(p)
            | Instruction::JumpIfLtEq(p)
            | Instruction::JumpIfGt(p)
            | Instruction::JumpIfGtEq(p)
            | Instruction::JumpIfNotLt(p)
            | Instruction::JumpIfNotLtEq(p)
            | Instruction::JumpIfNotGt(p)
            | Instruction::JumpIfNotGtEq(p)
            | Instruction::Jump(p) => Some(p),
            _ => None,
        }
    };
}

impl Instruction {
    /// Whether this instruction runs blocks. Every variant is named, so
    /// a new one must say.
    pub(crate) fn holds_blocks(&self) -> bool {
        match self {
            Instruction::Catch { .. }
            | Instruction::UnwindProtect { .. }
            | Instruction::ConditionCase { .. }
            | Instruction::SpecialCall { .. } => true,
            Instruction::Push(..)
            | Instruction::Pop
            | Instruction::Set
            | Instruction::SetPop
            | Instruction::StorePop(..)
            | Instruction::Store(..)
            | Instruction::Load(..)
            | Instruction::BindLocal(..)
            | Instruction::BindCell(..)
            | Instruction::LoadLocal(..)
            | Instruction::StoreLocal(..)
            | Instruction::StorePopLocal(..)
            | Instruction::LoadCell(..)
            | Instruction::StoreCell(..)
            | Instruction::StorePopCell(..)
            | Instruction::ClearLocals { .. }
            | Instruction::LoadCapture(..)
            | Instruction::StoreCapture(..)
            | Instruction::StorePopCapture(..)
            | Instruction::BeginScope(..)
            | Instruction::EndScope(..)
            | Instruction::BinaryOp(..)
            | Instruction::ArithChain { .. }
            | Instruction::LoadFile
            | Instruction::PrintPop
            | Instruction::Print
            | Instruction::Equal
            | Instruction::Eq
            | Instruction::Lt
            | Instruction::LtEq
            | Instruction::Gt
            | Instruction::GtEq
            | Instruction::Null
            | Instruction::JumpIfNil(..)
            | Instruction::JumpIfNotNil(..)
            | Instruction::JumpIfNilElsePop(..)
            | Instruction::JumpIfNotNilElsePop(..)
            | Instruction::JumpIfNeq(..)
            | Instruction::JumpIfEq(..)
            | Instruction::JumpIfEqual(..)
            | Instruction::JumpIfNotEqual(..)
            | Instruction::JumpIfLt(..)
            | Instruction::JumpIfLtEq(..)
            | Instruction::JumpIfGt(..)
            | Instruction::JumpIfGtEq(..)
            | Instruction::JumpIfNotLt(..)
            | Instruction::JumpIfNotLtEq(..)
            | Instruction::JumpIfNotGt(..)
            | Instruction::JumpIfNotGtEq(..)
            | Instruction::Jump(..)
            | Instruction::CompareChain { .. }
            | Instruction::Label(..)
            | Instruction::RustCall { .. }
            | Instruction::Call { .. }
            | Instruction::TailCall { .. }
            | Instruction::MakeLambda(..)
            | Instruction::DefineFunction(..)
            | Instruction::DefVar(..)
            | Instruction::Raise(..)
            | Instruction::Funcall { .. }
            | Instruction::Apply { .. }
            | Instruction::Ret
            | Instruction::PushTrace(..)
            | Instruction::PopTrace
            | Instruction::Cons
            | Instruction::List(..)
            | Instruction::Append(..)
            | Instruction::Cxr(..)
            | Instruction::PlistGet
            | Instruction::Quote
            | Instruction::WrapBackquote
            | Instruction::WrapUnquote
            | Instruction::WrapSplice
            | Instruction::SoleElement { .. } => false,
        }
    }

    /// The blocks this instruction runs, each with a name, for listings.
    pub(crate) fn blocks(&self) -> Vec<(String, Block)> {
        match self {
            Instruction::Catch { body } => vec![("body".to_string(), body.clone())],
            Instruction::UnwindProtect { body, cleanup } => vec![
                ("body".to_string(), body.clone()),
                ("cleanup".to_string(), cleanup.clone()),
            ],
            Instruction::ConditionCase { body, handlers, .. } => {
                let mut blocks = vec![("body".to_string(), body.clone())];
                for handler in handlers.iter() {
                    blocks.push((
                        format!("handler {}", handler.condition),
                        handler.body.clone(),
                    ));
                }
                blocks
            }
            Instruction::SpecialCall { blocks, .. } => blocks
                .iter()
                .enumerate()
                .map(|(index, arg)| (format!("form {index}"), arg.block.clone()))
                .collect(),
            _ => {
                debug_assert!(!self.holds_blocks(), "{self} holds blocks it does not list");
                Vec::new()
            }
        }
    }

    /// The target of a jump; `None` for anything else.
    pub(crate) fn pos(&self) -> Option<&Pos> {
        jump_pos!(self)
    }

    /// The target of a jump, to change it; `None` for anything else.
    pub(crate) fn pos_mut(&mut self) -> Option<&mut Pos> {
        jump_pos!(self)
    }

    /// Where a relative jump sitting at index `at` lands, as an index
    /// into the same instruction vector. `None` for a target before
    /// the start.
    pub(crate) fn rel_target(&self, at: usize) -> Option<usize> {
        match self.pos() {
            Some(Pos::Rel(rel)) => usize::try_from(at as isize + rel + 1).ok(),
            _ => None,
        }
    }

    /// For a comparison, the jump taken when its result is nil
    /// (`when_nil`) or not nil. The jump tests the operands itself,
    /// so no boolean object is built. A failed comparison is not the
    /// reversed comparison: with a NaN, `a < b` and `a >= b` are both
    /// false. So the nil side uses the "not" jumps.
    pub(crate) fn fused_jump(&self, when_nil: bool) -> Option<fn(Pos) -> Instruction> {
        let jump: fn(Pos) -> Instruction = match (self, when_nil) {
            (Instruction::Gt, true) => Instruction::JumpIfNotGt,
            (Instruction::Gt, false) => Instruction::JumpIfGt,
            (Instruction::Lt, true) => Instruction::JumpIfNotLt,
            (Instruction::Lt, false) => Instruction::JumpIfLt,
            (Instruction::GtEq, true) => Instruction::JumpIfNotGtEq,
            (Instruction::GtEq, false) => Instruction::JumpIfGtEq,
            (Instruction::LtEq, true) => Instruction::JumpIfNotLtEq,
            (Instruction::LtEq, false) => Instruction::JumpIfLtEq,
            (Instruction::Eq, true) => Instruction::JumpIfNeq,
            (Instruction::Eq, false) => Instruction::JumpIfEq,
            (Instruction::Equal, true) => Instruction::JumpIfNotEqual,
            (Instruction::Equal, false) => Instruction::JumpIfEqual,
            _ => return None,
        };
        Some(jump)
    }
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Push(obj) => write!(f, "    push {}", obj),
            Instruction::Pop => write!(f, "    pop"),
            Instruction::Set => write!(f, "    set"),
            Instruction::SetPop => write!(f, "    set_pop"),
            Instruction::StorePop(obj) => write!(f, "    store_pop {}", obj),
            Instruction::Store(obj) => write!(f, "    store {}", obj),
            Instruction::Load(obj) => write!(f, "    load {}", obj),
            Instruction::BindLocal(n) => write!(f, "    bind_local {}", n),
            Instruction::BindCell(n) => write!(f, "    bind_cell {}", n),
            Instruction::LoadLocal(n) => write!(f, "    load_local {}", n),
            Instruction::StoreLocal(n) => write!(f, "    store_local {}", n),
            Instruction::StorePopLocal(n) => write!(f, "    store_pop_local {}", n),
            Instruction::LoadCell(n) => write!(f, "    load_cell {}", n),
            Instruction::StoreCell(n) => write!(f, "    store_cell {}", n),
            Instruction::StorePopCell(n) => write!(f, "    store_pop_cell {}", n),
            Instruction::ClearLocals { from, to } => write!(f, "    clear_locals {}..{}", from, to),
            Instruction::LoadCapture(i) => write!(f, "    load_capture {}", i),
            Instruction::StoreCapture(i) => write!(f, "    store_capture {}", i),
            Instruction::StorePopCapture(i) => write!(f, "    store_pop_capture {}", i),
            Instruction::BeginScope(obj) => write!(f, "    begin_scope {}", obj),
            Instruction::EndScope(obj) => write!(f, "    end_scope {}", obj),
            Instruction::BinaryOp(op) => write!(f, "    {}", op.mnemonic()),
            Instruction::ArithChain { op, count } => {
                write!(f, "    {}_chain {}", op.mnemonic(), count)
            }
            Instruction::LoadFile => write!(f, "    load_file"),
            Instruction::PrintPop => write!(f, "    print_pop"),
            Instruction::Print => write!(f, "    print"),
            Instruction::Null => write!(f, "    null"),
            Instruction::JumpIfNil(pos) => write!(f, "    jnil {}", pos),
            Instruction::JumpIfNotNil(pos) => write!(f, "    jnnil {}", pos),
            Instruction::JumpIfNilElsePop(pos) => write!(f, "    jnil_else_pop {}", pos),
            Instruction::JumpIfNotNilElsePop(pos) => write!(f, "    jnnil_else_pop {}", pos),
            Instruction::JumpIfNeq(pos) => write!(f, "    jne {}", pos),
            Instruction::JumpIfEq(pos) => write!(f, "    jeq {}", pos),
            Instruction::JumpIfEqual(pos) => write!(f, "    jequal {}", pos),
            Instruction::JumpIfNotEqual(pos) => write!(f, "    jnequal {}", pos),
            Instruction::JumpIfLt(pos) => write!(f, "    jlt {}", pos),
            Instruction::JumpIfLtEq(pos) => write!(f, "    jle {}", pos),
            Instruction::JumpIfGt(pos) => write!(f, "    jgt {}", pos),
            Instruction::JumpIfGtEq(pos) => write!(f, "    jge {}", pos),
            Instruction::JumpIfNotLt(pos) => write!(f, "    jnlt {}", pos),
            Instruction::JumpIfNotLtEq(pos) => write!(f, "    jnle {}", pos),
            Instruction::JumpIfNotGt(pos) => write!(f, "    jngt {}", pos),
            Instruction::JumpIfNotGtEq(pos) => write!(f, "    jnge {}", pos),
            Instruction::CompareChain { comparison, count } => {
                write!(f, "    {}_chain {}", comparison.mnemonic(), count)
            }
            Instruction::Equal => write!(f, "    equal"),
            Instruction::Eq => write!(f, "    ceq"),
            Instruction::Lt => write!(f, "    clt"),
            Instruction::LtEq => write!(f, "    cle"),
            Instruction::Gt => write!(f, "    cgt"),
            Instruction::GtEq => write!(f, "    cge"),
            Instruction::Jump(pos) => write!(f, "    jmp {}", pos),
            Instruction::Call { name, .. } => write!(f, "    call {}", name),
            Instruction::TailCall { name, .. } => write!(f, "    tcall {}", name),
            Instruction::MakeLambda(_) => write!(f, "    make_lambda"),
            Instruction::DefineFunction(name) => write!(f, "    define_function {name}"),
            Instruction::Catch { .. } => write!(f, "    catch"),
            Instruction::UnwindProtect { .. } => write!(f, "    unwind_protect"),
            Instruction::ConditionCase { .. } => write!(f, "    condition_case"),
            Instruction::Raise(err) => write!(f, "    raise {}", err),
            Instruction::DefVar(sym) => write!(f, "    defvar {}", sym),
            Instruction::Funcall { args_count } => write!(f, "    funcall {}", args_count),
            Instruction::Apply { args_count } => write!(f, "    apply {}", args_count),
            Instruction::Ret => write!(f, "    ret"),
            Instruction::PushTrace(obj) => write!(f, "    push_trace {}", obj),
            Instruction::PopTrace => write!(f, "    pop_trace"),
            Instruction::RustCall {
                name, args_count, ..
            } => write!(f, "    rustcall {} {}", name, args_count),
            Instruction::SpecialCall {
                name,
                eager_count,
                blocks,
                ..
            } => write!(
                f,
                "    specialcall {} {} {}",
                name,
                eager_count,
                blocks.len()
            ),
            Instruction::Label(name) => write!(f, "{}", name),
            Instruction::Cons => write!(f, "    cons"),
            Instruction::List(len) => write!(f, "    list {}", len),
            Instruction::Append(len) => write!(f, "    append {}", len),
            Instruction::Cxr(cxr) => match cxr {
                Cxr::Car => write!(f, "    car"),
                Cxr::Cdr => write!(f, "    cdr"),
                Cxr::Caar => write!(f, "    caar"),
                Cxr::Cadr => write!(f, "    cadr"),
                Cxr::Cdar => write!(f, "    cdar"),
                Cxr::Cddr => write!(f, "    cddr"),
                Cxr::Caaar => write!(f, "    caaar"),
                Cxr::Caadr => write!(f, "    caadr"),
                Cxr::Cadar => write!(f, "    cadar"),
                Cxr::Caddr => write!(f, "    caddr"),
                Cxr::Cdaar => write!(f, "    cdaar"),
                Cxr::Cdadr => write!(f, "    cdadr"),
                Cxr::Cddar => write!(f, "    cddar"),
                Cxr::Cdddr => write!(f, "    cdddr"),
                Cxr::Caaaar => write!(f, "    caaaar"),
                Cxr::Caaadr => write!(f, "    caaadr"),
                Cxr::Caadar => write!(f, "    caadar"),
                Cxr::Caaddr => write!(f, "    caaddr"),
                Cxr::Cadaar => write!(f, "    cadaar"),
                Cxr::Cadadr => write!(f, "    cadadr"),
                Cxr::Caddar => write!(f, "    caddar"),
                Cxr::Cadddr => write!(f, "    cadddr"),
                Cxr::Cdaaar => write!(f, "    cdaaar"),
                Cxr::Cdaadr => write!(f, "    cdaadr"),
                Cxr::Cdadar => write!(f, "    cdadar"),
                Cxr::Cdaddr => write!(f, "    cdaddr"),
                Cxr::Cddaar => write!(f, "    cddaar"),
                Cxr::Cddadr => write!(f, "    cddadr"),
                Cxr::Cdddar => write!(f, "    cdddar"),
                Cxr::Cddddr => write!(f, "    cddddr"),
            },
            Instruction::PlistGet => write!(f, "    plist_get"),
            Instruction::Quote => write!(f, "    quote"),
            Instruction::WrapBackquote => write!(f, "    wrap_backquote"),
            Instruction::WrapUnquote => write!(f, "    wrap_unquote"),
            Instruction::WrapSplice => write!(f, "    wrap_splice"),
            Instruction::SoleElement {
                empty_is_nil: false,
            } => write!(f, "    sole_element"),
            Instruction::SoleElement { empty_is_nil: true } => {
                write!(f, "    sole_element_or_nil")
            }
        }
    }
}
