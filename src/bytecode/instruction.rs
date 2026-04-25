use crate::{
    TulispObject,
    object::wrappers::{DefunFn, TulispFn, generic::Shared},
};

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

#[derive(Clone, Copy)]
pub(crate) enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

/// A single instruction in the VM.
#[derive(Clone)]
pub(crate) enum Instruction {
    // stack
    Push(TulispObject),
    Pop,
    // variables
    Set,
    SetPop,
    StorePop(TulispObject),
    Store(TulispObject),
    Load(TulispObject),
    BeginScope(TulispObject),
    EndScope(TulispObject),
    // arithmetic
    BinaryOp(BinaryOp),
    // io
    LoadFile,
    PrintPop,
    Print,
    // comparison
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
    JumpIfLt(Pos),
    JumpIfLtEq(Pos),
    JumpIfGt(Pos),
    JumpIfGtEq(Pos),
    Jump(Pos),
    // functions
    Label(TulispObject),
    RustCall {
        name: TulispObject,
        /// Source AST of the full call form (`(name args…)`),
        /// recorded so an error from `func` carries the same outer
        /// `at (form)` trace line that `eval_basic`'s `with_trace`
        /// adds in the TW path.
        form: TulispObject,
        func: Shared<dyn TulispFn>,
        keep_result: bool,
    },
    /// Like `RustCall` but for `ctx.defun`-registered fns. Args have
    /// already been pushed on the stack (compiled with
    /// `keep_result=true`); the handler pops `args_count` of them in
    /// source order, hands them to `call(ctx, &args)`, and pushes the
    /// result if `keep_result`. Avoids the `RustCall` re-entry path
    /// because the closure never calls `ctx.eval`.
    RustCallTyped {
        name: TulispObject,
        /// See `RustCall::form`.
        form: TulispObject,
        call: Shared<dyn DefunFn>,
        args_count: usize,
        keep_result: bool,
    },
    Call {
        name: TulispObject,
        /// See `RustCall::form`.
        form: TulispObject,
        args_count: usize,
        function: Option<CompiledDefun>,
        optional_count: usize,
        rest_count: usize,
    },
    TailCall {
        name: TulispObject,
        /// See `RustCall::form`.
        form: TulispObject,
        args_count: usize,
        function: Option<CompiledDefun>,
        optional_count: usize,
        rest_count: usize,
    },
    /// Instantiate an anonymous `(lambda …)` at runtime: capture the
    /// enclosing scope's slots for each free var, bind fresh slots for
    /// params, rewrite the template's instruction vector with those
    /// bindings, and push the resulting closure (as a
    /// `TulispValue::CompiledDefun`) on the stack.
    MakeLambda(Shared<LambdaTemplate>),
    /// Inline `(funcall fn arg1 …)` dispatch. The function value is
    /// pushed first, then each arg, in source order (so at execution
    /// the top of stack is the last arg, `args_count + 1` below it is
    /// the function). Used to keep nested calls inside a VM run from
    /// re-entering `eval::funcall` (which would re-borrow `ctx.vm`).
    Funcall {
        args_count: usize,
    },
    Ret,
    /// Push `form` onto the machine's `trace_stack`. Errors that
    /// propagate out of any subsequent instruction (until a matching
    /// `PopTrace`) get `form` appended to their backtrace by the
    /// `run_impl` wrapper. Mirrors how TW's `eval_basic` wraps every
    /// list-form evaluation with `with_trace(expr)`. Emitted by
    /// `compile_expr` around every list-form's compiled bytecode.
    PushTrace(TulispObject),
    PopTrace,
    // lists
    Cons,
    List(usize),
    Append(usize),
    Cxr(Cxr),
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
    /// Same as `WrapUnquote` but for `,@X` at quasi-quote depth ≥ 2.
    WrapSplice,
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
            Instruction::BeginScope(obj) => write!(f, "    begin_scope {}", obj),
            Instruction::EndScope(obj) => write!(f, "    end_scope {}", obj),
            Instruction::BinaryOp(op) => match op {
                BinaryOp::Add => write!(f, "    add"),
                BinaryOp::Sub => write!(f, "    sub"),
                BinaryOp::Mul => write!(f, "    mul"),
                BinaryOp::Div => write!(f, "    div"),
            },
            Instruction::LoadFile => write!(f, "    load_file"),
            Instruction::PrintPop => write!(f, "    print_pop"),
            Instruction::Print => write!(f, "    print"),
            Instruction::Null => write!(f, "    null"),
            Instruction::JumpIfNil(pos) => write!(f, "    jnil {}", pos),
            Instruction::JumpIfNotNil(pos) => write!(f, "    jnnil {}", pos),
            Instruction::JumpIfNilElsePop(pos) => write!(f, "    jnil_else_pop {}", pos),
            Instruction::JumpIfNotNilElsePop(pos) => write!(f, "    jnnil_else_pop {}", pos),
            Instruction::JumpIfNeq(pos) => write!(f, "    jne {}", pos),
            Instruction::JumpIfLt(pos) => write!(f, "    jlt {}", pos),
            Instruction::JumpIfLtEq(pos) => write!(f, "    jle {}", pos),
            Instruction::JumpIfGt(pos) => write!(f, "    jgt {}", pos),
            Instruction::JumpIfGtEq(pos) => write!(f, "    jge {}", pos),
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
            Instruction::Funcall { args_count } => write!(f, "    funcall {}", args_count),
            Instruction::Ret => write!(f, "    ret"),
            Instruction::PushTrace(obj) => write!(f, "    push_trace {}", obj),
            Instruction::PopTrace => write!(f, "    pop_trace"),
            Instruction::RustCall { name, .. } => write!(f, "    rustcall {}", name),
            Instruction::RustCallTyped {
                name, args_count, ..
            } => write!(f, "    rustcall_typed {} {}", name, args_count),
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
        }
    }
}
