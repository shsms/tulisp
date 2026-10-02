use crate::{
    Error, ParamKind, TulispContext, TulispObject,
    bytecode::{FormBlock, Instruction},
    context::special::takes_form,
    object::wrappers::{SpecialFn, generic::Shared},
    value::DefunArity,
};

use super::super::compiler::{compile_block, compile_expr_keep_result};

/// The arguments of a special-form call: the code of the evaluated
/// ones, how many there are, and a block for each unevaluated one.
fn compile_arguments(
    ctx: &mut TulispContext,
    args: &TulispObject,
    kinds: &[ParamKind],
) -> Result<(Vec<Instruction>, usize, Vec<FormBlock>), Error> {
    let mut result = Vec::new();
    let mut eager_count = 0;
    let mut blocks = Vec::new();
    // Each block's slots start where the blocks before it end.
    let mut end = ctx.compiler.as_ref().unwrap().next_slot();
    for (index, arg) in args.base_iter().enumerate() {
        if takes_form(kinds, index) {
            let start = end;
            let compiler = ctx.compiler.as_mut().unwrap();
            compiler.free_slots_to(end);
            let count = compiler.replace_slot_count(end);
            let forms = TulispObject::cons(arg.clone(), TulispObject::nil());
            let block = compile_block(ctx, &forms, None);
            let compiler = ctx.compiler.as_mut().unwrap();
            end = compiler.replace_slot_count(count);
            compiler.replace_slot_count(count.max(end));
            blocks.push(FormBlock {
                block: block?,
                source: arg,
                slots: start..end,
            });
        } else {
            result.append(&mut compile_expr_keep_result(ctx, &arg)?);
            eager_count += 1;
        }
    }
    Ok((result, eager_count, blocks))
}

/// A call to a special form: each evaluated argument compiles to code
/// that leaves its value, and each unevaluated one to a block of its
/// own.
pub(super) fn compile_special_call(
    ctx: &mut TulispContext,
    name: &TulispObject,
    form: &TulispObject,
    args: &TulispObject,
    call: Shared<dyn SpecialFn>,
    kinds: &[ParamKind],
    arity: &DefunArity,
) -> Result<Vec<Instruction>, Error> {
    // The special form may run one form while another runs, so each
    // form's variables keep their slots until the call's forms have all
    // compiled.
    let first_slot = ctx.compiler.as_ref().unwrap().next_slot();
    let compiled = compile_arguments(ctx, args, kinds);
    ctx.compiler.as_mut().unwrap().free_slots_to(first_slot);
    let (mut result, eager_count, blocks) = compiled?;
    arity
        .check(eager_count + blocks.len())
        .map_err(|e| e.with_trace(form.clone()))?;
    let keep_result = ctx.compiler.as_ref().unwrap().keep_result;
    result.push(Instruction::SpecialCall {
        name: name.clone(),
        form: form.clone(),
        call,
        eager_count,
        blocks: Shared::new(blocks),
        keep_result,
    });
    Ok(result)
}
