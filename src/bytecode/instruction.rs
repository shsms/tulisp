use std::rc::Rc;

use crate::{value::TulispFn, TulispObject};

use super::bytecode::CompiledDefun;

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
        func: Rc<TulispFn>,
        keep_result: bool,
    },
    Call {
        name: TulispObject,
        args_count: usize,
        function: Option<CompiledDefun>,
        optional_count: usize,
        rest_count: usize,
    },
    Ret,
    // lists
    Cons,
    List(usize),
    Append(usize),
    Cxr(Cxr),
    PlistGet,
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
            Instruction::Ret => write!(f, "    ret"),
            Instruction::RustCall { name, .. } => write!(f, "    rustcall {}", name),
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
        }
    }
}
