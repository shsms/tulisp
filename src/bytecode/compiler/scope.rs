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
    /// Whether a closure captured the variable.
    pub(crate) captured: bool,
    /// Whether a `setq` sets the variable, in its own function or in a
    /// closure over it.
    pub(crate) assigned: bool,
}

impl ScopeVar {
    /// Whether the variable lives in a cell, shared with the closures
    /// that captured it: it is captured and assigned. A captured
    /// variable nobody assigns keeps its value in its slot, and a
    /// closure copies it, which no program can tell apart.
    pub(crate) fn shared(&self) -> bool {
        self.captured && self.assigned
    }
}

/// A function being compiled: a `defun`, a `lambda`, or a top-level
/// program. Only the top-level program, the first, uses no variables of
/// enclosing functions.
#[derive(Default)]
pub(crate) struct FunctionScope {
    pub(crate) vars: Vec<ScopeVar>,
    /// The variables of enclosing functions this one uses, in the
    /// order of a closure's captures: where each comes from, and the
    /// variable's name.
    pub(crate) capture_sources: Vec<(CaptureSource, TulispObject)>,
    /// The slot the next variable takes; a slot is free again once its
    /// variable leaves the scope.
    pub(crate) next_slot: u16,
    /// How many slots a call of this function reserves.
    pub(crate) slot_count: u16,
    /// The label after the function's prologue, where a self tail call
    /// jumps once it has rebound the parameters.
    pub(crate) body_start: Option<TulispObject>,
    /// How many special variables the `let`s around the code being
    /// compiled bind. A tail call would leave them bound, as it skips
    /// the `EndScope`s after a `let`'s body, so while any is, a tail
    /// call compiles as an ordinary call.
    pub(crate) special_lets: usize,
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
            assigned: false,
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

    /// Adds N special `let` bindings to the count of the function being
    /// compiled, or with IN_FORCE false takes them away.
    pub(crate) fn count_special_lets(&mut self, n: usize, in_force: bool) {
        if let Some(function) = self.functions.last_mut() {
            function.special_lets = if in_force {
                function.special_lets + n
            } else {
                function.special_lets.saturating_sub(n)
            };
        }
    }

    /// Whether a special `let` binding is in force in the code being
    /// compiled.
    pub(crate) fn in_special_let(&self) -> bool {
        self.functions
            .last()
            .is_some_and(|function| function.special_lets > 0)
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

/// Where a name's value is.
#[derive(Clone, Copy)]
pub(crate) enum Resolved {
    /// The name's own value: a global or special variable.
    Global,
    /// The running frame's slot. When the variable's scope ends, its
    /// uses are swapped for cell forms if it turned out to be shared.
    Local(u16),
    /// The running closure's captured variable at this index.
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
            Resolved::Capture(index) => Instruction::LoadCapture(index),
        }
    }
}

/// What a `setq` of a name sets, from [`resolve_assignment`]. Only it
/// stores to a lexical variable or a capture, so every such store marks
/// the variable it sets as assigned.
#[derive(Clone, Copy)]
pub(crate) struct Assignment(Resolved);

impl Assignment {
    /// Whether it sets a lexical variable of the running function.
    pub(crate) fn is_local(self) -> bool {
        matches!(self.0, Resolved::Local(_))
    }

    /// The instruction that sets NAME to the value on the stack, popping
    /// it unless KEEP.
    pub(crate) fn store(self, name: &TulispObject, keep: bool) -> Instruction {
        match (self.0, keep) {
            (Resolved::Global, true) => Instruction::Store(name.clone()),
            (Resolved::Global, false) => Instruction::StorePop(name.clone()),
            (Resolved::Local(slot), true) => Instruction::StoreLocal(slot),
            (Resolved::Local(slot), false) => Instruction::StorePopLocal(slot),
            (Resolved::Capture(index), true) => Instruction::StoreCapture(index),
            (Resolved::Capture(index), false) => Instruction::StorePopCapture(index),
        }
    }
}

/// Where NAME's value is: the innermost lexical variable of the
/// name, a capture of an enclosing function's variable, or the name's
/// own value for a global or special variable.
pub(crate) fn resolve(ctx: &mut TulispContext, name: &TulispObject) -> Result<Resolved, Error> {
    resolve_for(ctx, name, false)
}

/// Like [`resolve`], for a `setq` of NAME: a lexical variable it finds
/// is marked assigned.
pub(crate) fn resolve_assignment(
    ctx: &mut TulispContext,
    name: &TulispObject,
) -> Result<Assignment, Error> {
    resolve_for(ctx, name, true).map(Assignment)
}

fn resolve_for(
    ctx: &mut TulispContext,
    name: &TulispObject,
    assigns: bool,
) -> Result<Resolved, Error> {
    let Some(compiler) = ctx.compiler.as_mut() else {
        return Ok(Resolved::Global);
    };
    let Some(depth) = compiler.functions.len().checked_sub(1) else {
        return Ok(Resolved::Global);
    };
    resolve_in(compiler, name, depth, assigns)
}

fn resolve_in(
    compiler: &mut crate::bytecode::Compiler,
    name: &TulispObject,
    depth: usize,
    assigns: bool,
) -> Result<Resolved, Error> {
    let function = &mut compiler.functions[depth];
    if let Some(var) = function.vars.iter_mut().rev().find(|var| var.name.eq(name)) {
        var.assigned |= assigns;
        return Ok(Resolved::Local(var.slot));
    }
    if let Some(index) = function
        .capture_sources
        .iter()
        .position(|(_, captured)| captured.eq(name))
    {
        if assigns && depth > 0 {
            // Mark the variable the capture comes from, however far out.
            resolve_in(compiler, name, depth - 1, true)?;
        }
        return Ok(Resolved::Capture(capture_index(index)?));
    }
    if depth == 0 {
        return Ok(Resolved::Global);
    }
    let source = match resolve_in(compiler, name, depth - 1, assigns)? {
        // A global or special variable in every enclosing function.
        Resolved::Global => return Ok(Resolved::Global),
        Resolved::Local(slot) => {
            mark_captured(&mut compiler.functions[depth - 1], name);
            CaptureSource::Local(slot)
        }
        Resolved::Capture(index) => CaptureSource::Capture(index),
    };
    let sources = &mut compiler.functions[depth].capture_sources;
    let index = capture_index(sources.len())?;
    sources.push((source, name.clone()));
    Ok(Resolved::Capture(index))
}

/// Marks the innermost variable NAME of FUNCTION as captured: if it is
/// also assigned, it holds a cell from its binding on.
fn mark_captured(function: &mut FunctionScope, name: &TulispObject) {
    if let Some(var) = function.vars.iter_mut().rev().find(|var| var.name.eq(name)) {
        var.captured = true;
    }
}

fn capture_index(index: usize) -> Result<u16, Error> {
    u16::try_from(index)
        .map_err(|_| Error::lisp_error("a function captures more than 65535 variables"))
}
