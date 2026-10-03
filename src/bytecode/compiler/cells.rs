//! Turning a variable shared with a closure into a cell, once the
//! scope it is bound in has compiled.

use super::scope::ScopeVar;
use crate::bytecode::{Block, Instruction};

/// Swaps the uses of each of VARS that is shared with a closure, in
/// INSTRUCTIONS, for their cell forms: VARS are leaving scope, and
/// INSTRUCTIONS run from their binding on.
pub(crate) fn swap_shared(vars: &[ScopeVar], instructions: &mut [Instruction]) {
    for var in vars.iter().filter(|var| var.shared()) {
        swap_to_cells(instructions, var.slot);
    }
}

/// Turns every use of SLOT in INSTRUCTIONS, nested blocks included,
/// into its cell form. Positions do not change, so jumps stay valid.
pub(crate) fn swap_to_cells(instructions: &mut [Instruction], slot: u16) {
    for instruction in instructions.iter_mut() {
        match instruction {
            Instruction::BindLocal(n) if *n == slot => *instruction = Instruction::BindCell(slot),
            Instruction::LoadLocal(n) if *n == slot => *instruction = Instruction::LoadCell(slot),
            Instruction::StoreLocal(n) if *n == slot => *instruction = Instruction::StoreCell(slot),
            Instruction::StorePopLocal(n) if *n == slot => {
                *instruction = Instruction::StorePopCell(slot)
            }
            Instruction::Catch { body } => swap_in_block(body, slot),
            Instruction::UnwindProtect { body, cleanup } => {
                swap_in_block(body, slot);
                swap_in_block(cleanup, slot);
            }
            Instruction::ConditionCase { body, handlers, .. } => {
                swap_in_block(body, slot);
                for handler in handlers.iter() {
                    swap_in_block(&handler.body, slot);
                }
            }
            Instruction::SpecialCall { blocks, .. } => {
                for form in blocks.iter() {
                    swap_in_block(&form.block, slot);
                }
            }
            _ => {}
        }
    }
}

fn swap_in_block(block: &Block, slot: u16) {
    swap_to_cells(&mut block.instructions.borrow_mut(), slot);
}

#[cfg(test)]
mod tests {
    use crate::test_utils::eval_assert_equal;
    use crate::{Rest, TulispContext};

    // A captured variable written inside each kind of block the
    // compiler nests: every write goes through the cell.
    #[test]
    fn the_swap_reaches_every_nested_block() {
        let ctx = &mut TulispContext::new();
        ctx.defspecial(
            "body-of",
            |ctx: &mut TulispContext, body: Rest<crate::Form>| body.eval_progn(ctx),
        );
        eval_assert_equal(
            ctx,
            "(defun f ()
               (let ((n 0))
                 (catch 'k (setq n (1+ n)))
                 (unwind-protect (setq n (1+ n)) (setq n (1+ n)))
                 (condition-case nil
                     (progn (setq n (1+ n)) (error \"x\"))
                   (error (setq n (1+ n))))
                 (body-of (setq n (1+ n)))
                 (funcall (lambda () n))))
             (f)",
            "6",
        );
    }
}
