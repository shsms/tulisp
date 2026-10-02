//! The variables in scope while a function compiles, and how a name
//! resolves against them.

use crate::{
    Error, TulispContext, TulispObject,
    bytecode::{CaptureSource, Instruction},
};

/// A lexical variable in scope: a slot of the function's frame. A
/// `defvar` variable that a `let` binds never enters the scope, so it
/// never hides an enclosing lexical variable of the same name, as in
/// Emacs.
pub(crate) struct ScopeVar {
    pub(crate) name: TulispObject,
    pub(crate) slot: u16,
    /// Whether a closure captured the variable: its slot then holds a
    /// cell, shared with the closure.
    pub(crate) captured: bool,
}

/// A function being compiled: a `defun`, a `lambda`, or a top-level
/// program. Only the top-level program, the first, uses no variables of
/// enclosing functions.
#[derive(Default)]
pub(crate) struct FunctionScope {
    pub(crate) vars: Vec<ScopeVar>,
    /// The variables of enclosing functions this one uses, in the
    /// order of the cells of a closure made from it: where each cell
    /// comes from, and the variable's name.
    pub(crate) captures: Vec<(CaptureSource, TulispObject)>,
    /// The slot the next variable takes; a slot is free again once its
    /// variable leaves the scope.
    pub(crate) next_slot: u16,
    /// How many slots a call of this function reserves.
    pub(crate) slot_count: u16,
    /// The label after the function's prologue, where a self tail call
    /// jumps once it has rebound the parameters.
    pub(crate) body_start: Option<TulispObject>,
}

impl crate::bytecode::Compiler {
    pub(crate) fn push_function(&mut self) {
        self.functions.push(FunctionScope::default());
    }

    pub(crate) fn pop_function(&mut self) -> FunctionScope {
        self.functions.pop().unwrap_or_default()
    }

    /// Puts NAME in scope in the next free slot of the function being
    /// compiled, and gives the slot.
    pub(crate) fn bind_slot(&mut self, name: TulispObject) -> Result<u16, Error> {
        let Some(function) = self.functions.last_mut() else {
            return Err(Error::lisp_error("internal: a slot outside a function"));
        };
        let slot = function.next_slot;
        function.next_slot = slot
            .checked_add(1)
            .ok_or_else(|| Error::lisp_error("a function holds more than 65535 variables"))?;
        function.slot_count = function.slot_count.max(function.next_slot);
        function.vars.push(ScopeVar {
            name,
            slot,
            captured: false,
        });
        Ok(slot)
    }

    /// Takes the innermost N variables of the function being compiled
    /// out of scope, and gives them back.
    pub(crate) fn unbind(&mut self, n: usize) -> Vec<ScopeVar> {
        let Some(function) = self.functions.last_mut() else {
            return Vec::new();
        };
        let len = function.vars.len().saturating_sub(n);
        function.vars.split_off(len)
    }

    /// The slot the next variable of the function being compiled takes.
    pub(crate) fn next_slot(&self) -> u16 {
        self.functions
            .last()
            .map_or(0, |function| function.next_slot)
    }

    /// Frees the slots of the function being compiled from NEXT on.
    pub(crate) fn free_slots_to(&mut self, next: u16) {
        if let Some(function) = self.functions.last_mut() {
            function.next_slot = next;
        }
    }

    /// The end of the slots the function being compiled has used, set
    /// to END; gives the old end.
    pub(crate) fn replace_slot_count(&mut self, end: u16) -> u16 {
        self.functions.last_mut().map_or(0, |function| {
            std::mem::replace(&mut function.slot_count, end)
        })
    }
}

/// What a name reads and writes.
#[derive(Clone, Copy)]
pub(crate) enum Resolved {
    /// The name's own value: a global or special variable.
    Global,
    /// The value in the running frame's slot.
    Local(u16),
    /// The cell in the running frame's slot, for a captured variable.
    Cell(u16),
    /// The running closure's captured cell at this index.
    Capture(u16),
}

impl Resolved {
    /// The instruction that reads NAME. A keyword is its own value,
    /// unless it names a parameter.
    pub(crate) fn load(self, name: &TulispObject) -> Instruction {
        match self {
            Resolved::Global if name.keywordp() => Instruction::Push(name.clone()),
            Resolved::Global => Instruction::Load(name.clone()),
            Resolved::Local(slot) => Instruction::LoadLocal(slot),
            Resolved::Cell(slot) => Instruction::LoadCell(slot),
            Resolved::Capture(index) => Instruction::LoadCapture(index),
        }
    }

    /// The instruction that sets NAME to the value on the stack, popping
    /// it unless KEEP.
    pub(crate) fn store(self, name: &TulispObject, keep: bool) -> Instruction {
        match (self, keep) {
            (Resolved::Global, true) => Instruction::Store(name.clone()),
            (Resolved::Global, false) => Instruction::StorePop(name.clone()),
            (Resolved::Local(slot), true) => Instruction::StoreLocal(slot),
            (Resolved::Local(slot), false) => Instruction::StorePopLocal(slot),
            (Resolved::Cell(slot), true) => Instruction::StoreCell(slot),
            (Resolved::Cell(slot), false) => Instruction::StorePopCell(slot),
            (Resolved::Capture(index), true) => Instruction::StoreCapture(index),
            (Resolved::Capture(index), false) => Instruction::StorePopCapture(index),
        }
    }
}

/// What NAME reads and writes: the innermost lexical variable of the
/// name, a capture of an enclosing function's variable, or the name's
/// own value for a global or special variable.
pub(crate) fn resolve(ctx: &mut TulispContext, name: &TulispObject) -> Result<Resolved, Error> {
    let Some(compiler) = ctx.compiler.as_mut() else {
        return Ok(Resolved::Global);
    };
    let Some(depth) = compiler.functions.len().checked_sub(1) else {
        return Ok(Resolved::Global);
    };
    resolve_in(compiler, name, depth)
}

fn resolve_in(
    compiler: &mut crate::bytecode::Compiler,
    name: &TulispObject,
    depth: usize,
) -> Result<Resolved, Error> {
    let function = &compiler.functions[depth];
    if let Some(var) = function.vars.iter().rev().find(|var| var.name.eq(name)) {
        return Ok(if var.captured {
            Resolved::Cell(var.slot)
        } else {
            Resolved::Local(var.slot)
        });
    }
    if let Some(index) = function
        .captures
        .iter()
        .position(|(_, captured)| captured.eq(name))
    {
        return Ok(Resolved::Capture(capture_index(index)?));
    }
    if depth == 0 {
        return Ok(Resolved::Global);
    }
    let source = match resolve_in(compiler, name, depth - 1)? {
        // A global or special variable in every enclosing function.
        Resolved::Global => return Ok(Resolved::Global),
        Resolved::Local(slot) | Resolved::Cell(slot) => {
            mark_captured(&mut compiler.functions[depth - 1], name);
            CaptureSource::Local(slot)
        }
        Resolved::Capture(index) => CaptureSource::Capture(index),
    };
    let captures = &mut compiler.functions[depth].captures;
    let index = capture_index(captures.len())?;
    captures.push((source, name.clone()));
    Ok(Resolved::Capture(index))
}

/// Marks the innermost variable NAME of FUNCTION as captured: it holds
/// a cell from its binding on.
fn mark_captured(function: &mut FunctionScope, name: &TulispObject) {
    if let Some(var) = function.vars.iter_mut().rev().find(|var| var.name.eq(name)) {
        var.captured = true;
    }
}

fn capture_index(index: usize) -> Result<u16, Error> {
    u16::try_from(index)
        .map_err(|_| Error::lisp_error("a function captures more than 65535 variables"))
}
