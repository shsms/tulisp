use crate::{
    Error, ParamKind, TulispContext, TulispObject,
    bytecode::{FormBlock, Instruction},
    context::special::takes_form,
    object::wrappers::{SpecialFn, generic::Shared},
    value::DefunArity,
};

use super::super::compiler::{compile_block, compile_expr_keep_result};

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
    let mut result = Vec::new();
    let mut eager_count = 0;
    let mut blocks = Vec::new();
    for (index, arg) in args.base_iter().enumerate() {
        if takes_form(kinds, index) {
            let forms = TulispObject::cons(arg.clone(), TulispObject::nil());
            blocks.push(FormBlock {
                block: compile_block(ctx, &forms, None)?,
                source: arg,
            });
        } else {
            result.append(&mut compile_expr_keep_result(ctx, &arg)?);
            eager_count += 1;
        }
    }
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
