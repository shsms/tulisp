use std::{collections::HashMap, fmt};

use super::Instruction;
use crate::{
    bytecode::compiler::VMDefunParams,
    object::wrappers::generic::{Shared, SharedMut},
    TulispObject,
};

/// One source-form span pinned to a half-open instruction range
/// `[start_pc, end_pc)`. When an error propagates out of an
/// instruction inside the range, `run_impl` adds `form` to the
/// error's backtrace via `with_trace`. Built at compile time by
/// `strip_trace_markers`, which walks the
/// `Instruction::PushTrace` / `PopTrace` pairs and converts them to
/// this side-table — so the runtime pays nothing on the happy path.
#[derive(Clone, Debug)]
pub(crate) struct TraceRange {
    pub start_pc: usize,
    pub end_pc: usize,
    pub form: TulispObject,
}

#[doc(hidden)]
#[derive(Clone)]
pub struct CompiledDefun {
    pub(crate) name: TulispObject,
    pub(crate) instructions: SharedMut<Vec<Instruction>>,
    /// Trace ranges for `instructions`, populated by
    /// `strip_trace_markers`. Stored behind a `Shared` so cloning
    /// the `CompiledDefun` (which the `Call` handler does on every
    /// dispatch) only bumps a refcount instead of copying the vec
    /// element-wise. Empty for functions whose bytecode contains no
    /// list-form markers — represented by an empty shared vec
    /// rather than a special-case to keep the runtime path
    /// uniform.
    pub(crate) trace_ranges: Shared<Vec<TraceRange>>,
    pub(crate) params: VMDefunParams,
}

impl Default for CompiledDefun {
    fn default() -> Self {
        Self {
            name: TulispObject::nil(),
            instructions: SharedMut::default(),
            trace_ranges: Shared::new_sized(Vec::new()),
            params: VMDefunParams::default(),
        }
    }
}

#[derive(Clone)]
pub(crate) struct Bytecode {
    pub(crate) global: SharedMut<Vec<Instruction>>,
    /// Trace ranges paired with `global`; see `TraceRange`.
    pub(crate) global_trace_ranges: Shared<Vec<TraceRange>>,
    pub(crate) functions: HashMap<usize, CompiledDefun>, // key: fn_name.addr_as_usize()
}

impl Default for Bytecode {
    fn default() -> Self {
        Self {
            global: SharedMut::default(),
            global_trace_ranges: Shared::new_sized(Vec::new()),
            functions: HashMap::default(),
        }
    }
}

impl fmt::Display for Bytecode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, instr) in self.global.borrow().iter().enumerate() {
            write!(f, "\n{:<40}   # {}", instr.to_string(), i)?;
        }
        for (key, func) in &self.functions {
            write!(f, "\n\n{} (#{}):", func.name, key)?;
            for (i, instr) in func.instructions.borrow().iter().enumerate() {
                write!(f, "\n{:<40}   # {}", instr.to_string(), i)?;
            }
        }
        Ok(())
    }
}

impl Bytecode {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    pub(crate) fn import_functions(&mut self, other: &Bytecode) {
        self.functions.extend(other.functions.clone());
    }
}

/// Convert `PushTrace` / `PopTrace` markers in `input` into a
/// side-table of `TraceRange`s and return the bytecode with the
/// markers removed. Patches every `Pos::Rel` jump whose origin or
/// target falls on opposite sides of a removed slot so it still
/// lands on the same logical instruction in the stripped output.
///
/// `Pos::Label` jumps resolve through the machine's `labels`
/// HashMap (rebuilt by `Machine::locate_labels` *after* this strip
/// runs), so they need no fixup. `Pos::Abs(0)` (self-recursion's
/// jump-to-function-start) also stays valid because the function's
/// first non-marker instruction lives at PC 0 in the stripped
/// vector regardless of whether it was preceded by a `PushTrace`.
pub(crate) fn strip_trace_markers(input: Vec<Instruction>) -> (Vec<Instruction>, Vec<TraceRange>) {
    use crate::bytecode::instruction::Pos;

    // shift[i] = number of marker instructions strictly before
    // original index `i`. shift.len() == input.len() + 1.
    let mut shift: Vec<usize> = Vec::with_capacity(input.len() + 1);
    let mut cumulative = 0usize;
    for instr in &input {
        shift.push(cumulative);
        if matches!(
            instr,
            Instruction::PushTrace(_) | Instruction::PopTrace
        ) {
            cumulative += 1;
        }
    }
    shift.push(cumulative);

    let mut output: Vec<Instruction> = Vec::with_capacity(input.len() - cumulative);
    let mut ranges: Vec<TraceRange> = Vec::new();
    let mut stack: Vec<(usize, TulispObject)> = Vec::new();

    for (orig_pc, mut instr) in input.into_iter().enumerate() {
        match &instr {
            Instruction::PushTrace(form) => {
                stack.push((output.len(), form.clone()));
                continue;
            }
            Instruction::PopTrace => {
                let (start, form) = stack
                    .pop()
                    .expect("PopTrace without matching PushTrace in compiled bytecode");
                ranges.push(TraceRange {
                    start_pc: start,
                    end_pc: output.len(),
                    form,
                });
                continue;
            }
            _ => {}
        }
        // Adjust any `Pos::Rel` whose target lies on the other
        // side of stripped instructions. `Pos::Label` resolves via
        // the labels HashMap (rebuilt post-strip) so it's fine.
        // `Pos::Abs` is only used for self-recursion's jump-to-PC-0,
        // which remains correct.
        let pos_field: Option<&mut Pos> = match &mut instr {
            Instruction::JumpIfNil(p)
            | Instruction::JumpIfNotNil(p)
            | Instruction::JumpIfNilElsePop(p)
            | Instruction::JumpIfNotNilElsePop(p)
            | Instruction::JumpIfNeq(p)
            | Instruction::JumpIfLt(p)
            | Instruction::JumpIfLtEq(p)
            | Instruction::JumpIfGt(p)
            | Instruction::JumpIfGtEq(p)
            | Instruction::Jump(p) => Some(p),
            _ => None,
        };
        if let Some(Pos::Rel(rel)) = pos_field {
            let orig_target = (orig_pc as isize + *rel + 1) as usize;
            let new_pc = orig_pc - shift[orig_pc];
            // shift.len() == input.len() + 1, so target == input.len() is OK.
            let new_target = orig_target - shift[orig_target];
            *rel = new_target as isize - new_pc as isize - 1;
        }
        output.push(instr);
    }

    debug_assert!(
        stack.is_empty(),
        "PushTrace without matching PopTrace in compiled bytecode"
    );
    (output, ranges)
}
