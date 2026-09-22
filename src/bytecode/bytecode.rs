use std::{collections::HashMap, fmt};

use super::Instruction;
use crate::{
    Error, TulispObject,
    bytecode::compiler::VMDefunParams,
    object::wrappers::generic::{Shared, SharedMut},
};

/// One source-form span pinned to a half-open instruction range
/// `[start_pc, end_pc)`. When an error propagates out of an
/// instruction inside the range, `run_impl` adds `form` to the
/// error's backtrace via `with_trace`. Built at compile time by
/// `assemble`, which walks the `Instruction::PushTrace` /
/// `PopTrace` pairs and converts them to this side-table — so the
/// runtime pays nothing on the happy path.
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
    /// Trace ranges for `instructions`, populated by `assemble`.
    /// Stored behind a `Shared` so cloning the `CompiledDefun`
    /// (which the `Call` handler does on every dispatch) only bumps
    /// a refcount instead of copying the vec element-wise. Empty
    /// for functions whose bytecode contains no list-form markers.
    pub(crate) trace_ranges: Shared<Vec<TraceRange>>,
    /// Behind a `Shared` for the same reason as `trace_ranges`.
    pub(crate) params: Shared<VMDefunParams>,
    /// The tree-walker's lambda for the same `defun` form, which `run`
    /// installs on `name` when it loads this copy: the symbol holds it for
    /// as long as this copy is its definition. `None` for an anonymous
    /// lambda.
    pub(crate) source: Option<TulispObject>,
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
            global_trace_ranges: Shared::new(Vec::new()),
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

/// Turns compiled instructions into runnable ones. Every
/// `PushTrace` / `PopTrace` pair becomes a `TraceRange` in the
/// returned side table, every `Label` is dropped, and every jump
/// that names a label is resolved to the absolute position of the
/// next kept instruction, or the end of the unit when the label is
/// last, where jumping ends the run. `Pos::Rel` and `Pos::Abs`
/// jumps are patched for the removed slots so they still land on
/// the same logical instruction. A unit the compiler left
/// malformed, with unbalanced trace markers, a duplicate label, a
/// jump outside the unit, or a jump to a label it does not have, is
/// an internal error.
pub(crate) fn assemble(
    input: Vec<Instruction>,
) -> Result<(Vec<Instruction>, Vec<TraceRange>), Error> {
    use crate::bytecode::instruction::Pos;

    const JUMP_PAST_END: &str = "internal: jump past the end of the unit";

    fn is_removed(instr: &Instruction) -> bool {
        matches!(
            instr,
            Instruction::PushTrace(_) | Instruction::PopTrace | Instruction::Label(_)
        )
    }

    // shift[i] = number of removed instructions strictly before
    // original index `i`; shift.len() == input.len() + 1. A label's
    // target is the kept instruction after it.
    let mut shift: Vec<usize> = Vec::with_capacity(input.len() + 1);
    let mut labels: HashMap<usize, usize> = HashMap::new();
    let mut cumulative = 0usize;
    for (i, instr) in input.iter().enumerate() {
        shift.push(cumulative);
        if is_removed(instr) {
            cumulative += 1;
        }
        if let Instruction::Label(name) = instr
            && labels
                .insert(name.addr_as_usize(), i + 1 - cumulative)
                .is_some()
        {
            return Err(Error::lisp_error(format!(
                "internal: duplicate label {name}"
            )));
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
                let Some((start, form)) = stack.pop() else {
                    return Err(Error::lisp_error(
                        "internal: PopTrace without a matching PushTrace",
                    ));
                };
                ranges.push(TraceRange {
                    start_pc: start,
                    end_pc: output.len(),
                    form,
                });
                continue;
            }
            Instruction::Label(_) => continue,
            _ => {}
        }
        let rel_target = instr.rel_target(orig_pc);
        if let Some(pos) = instr.pos_mut() {
            match pos {
                Pos::Rel(rel) => {
                    let Some(orig_target) = rel_target else {
                        return Err(Error::lisp_error(
                            "internal: relative jump before the start of the unit",
                        ));
                    };
                    // shift.len() == input.len() + 1, so target ==
                    // input.len() is OK.
                    let Some(target_shift) = shift.get(orig_target) else {
                        return Err(Error::lisp_error(JUMP_PAST_END));
                    };
                    let new_pc = orig_pc - shift[orig_pc];
                    let new_target = orig_target - target_shift;
                    *rel = new_target as isize - new_pc as isize - 1;
                }
                Pos::Abs(p) => {
                    let Some(target_shift) = shift.get(*p) else {
                        return Err(Error::lisp_error(JUMP_PAST_END));
                    };
                    *p -= target_shift;
                }
                Pos::Label(name) => {
                    let Some(target) = labels.get(&name.addr_as_usize()).copied() else {
                        return Err(Error::lisp_error(format!(
                            "internal: jump to unknown label {name}"
                        )));
                    };
                    *pos = Pos::Abs(target);
                }
            }
        }
        output.push(instr);
    }

    if !stack.is_empty() {
        return Err(Error::lisp_error(
            "internal: PushTrace without a matching PopTrace",
        ));
    }
    Ok((output, ranges))
}

#[cfg(test)]
mod tests {
    use super::{Instruction, assemble};
    use crate::TulispObject;
    use crate::bytecode::instruction::Pos;

    fn label() -> TulispObject {
        TulispObject::symbol("l".to_string(), true)
    }

    fn jump_target(instr: &Instruction) -> Option<usize> {
        if let Instruction::Jump(Pos::Abs(target)) = instr {
            Some(*target)
        } else {
            None
        }
    }

    #[test]
    fn a_label_jump_lands_on_the_instruction_after_its_label() {
        let l = label();
        let (out, _) = assemble(vec![
            Instruction::Push(TulispObject::nil()),
            Instruction::Jump(Pos::Label(l.clone())),
            Instruction::Push(TulispObject::nil()),
            Instruction::Label(l),
            Instruction::Push(TulispObject::nil()),
        ])
        .unwrap();
        assert_eq!(out.len(), 4);
        assert_eq!(jump_target(&out[1]), Some(3));
    }

    #[test]
    fn a_label_at_the_end_resolves_to_the_unit_length() {
        let l = label();
        let (out, _) = assemble(vec![
            Instruction::Jump(Pos::Label(l.clone())),
            Instruction::Push(TulispObject::nil()),
            Instruction::Label(l),
        ])
        .unwrap();
        assert_eq!(out.len(), 2);
        assert_eq!(jump_target(&out[0]), Some(2));
    }

    #[test]
    fn relative_and_absolute_jumps_skip_removed_slots() {
        let (out, ranges) = assemble(vec![
            Instruction::Jump(Pos::Rel(3)),
            Instruction::PushTrace(TulispObject::nil()),
            Instruction::Label(label()),
            Instruction::PopTrace,
            Instruction::Push(TulispObject::nil()),
            Instruction::Jump(Pos::Abs(4)),
        ])
        .unwrap();
        assert_eq!(out.len(), 3);
        assert!(matches!(out[0], Instruction::Jump(Pos::Rel(0))));
        assert_eq!(jump_target(&out[2]), Some(1));
        assert_eq!((ranges[0].start_pc, ranges[0].end_pc), (1, 1));
    }

    #[test]
    fn unbalanced_trace_markers_are_an_error() {
        for (unit, message) in [
            (vec![Instruction::PopTrace], "without a matching PushTrace"),
            (
                vec![Instruction::PushTrace(TulispObject::nil())],
                "without a matching PopTrace",
            ),
        ] {
            let Err(err) = assemble(unit) else {
                panic!("unbalanced trace markers assembled");
            };
            assert!(err.to_string().contains(message), "{err}");
        }
    }

    #[test]
    fn a_duplicate_label_is_an_error() {
        let l = label();
        let Err(err) = assemble(vec![Instruction::Label(l.clone()), Instruction::Label(l)]) else {
            panic!("a duplicate label assembled");
        };
        assert!(err.to_string().contains("duplicate label"), "{err}");
    }

    #[test]
    fn a_jump_to_the_end_of_the_unit_is_accepted() {
        let (out, _) = assemble(vec![Instruction::Jump(Pos::Rel(0))]).unwrap();
        assert!(matches!(out[0], Instruction::Jump(Pos::Rel(0))));
        let (out, _) = assemble(vec![Instruction::Jump(Pos::Abs(1))]).unwrap();
        assert_eq!(jump_target(&out[0]), Some(1));
    }

    #[test]
    fn a_jump_outside_the_unit_is_an_error() {
        for unit in [
            vec![Instruction::Jump(Pos::Abs(2))],
            vec![Instruction::Jump(Pos::Rel(1))],
            vec![Instruction::Jump(Pos::Rel(-2))],
        ] {
            let Err(err) = assemble(unit) else {
                panic!("a jump outside the unit assembled");
            };
            assert!(err.to_string().contains("jump"), "{err}");
        }
    }

    #[test]
    fn a_jump_to_an_unknown_label_is_an_error() {
        let Err(err) = assemble(vec![Instruction::Jump(Pos::Label(label()))]) else {
            panic!("a jump to an unknown label assembled");
        };
        assert!(err.to_string().contains("unknown label"), "{err}");
    }
}
