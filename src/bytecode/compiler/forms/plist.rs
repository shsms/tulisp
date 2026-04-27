use crate::{
    Error, TulispContext, TulispObject,
    bytecode::{Instruction, compiler::compiler::compile_expr},
};

pub(super) fn compile_fn_plist_get(
    ctx: &mut TulispContext,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    ctx.compile_2_arg_call(name, args, false, |ctx, plist, property, _| {
        let mut result = compile_expr(ctx, property)?;
        result.append(&mut compile_expr(ctx, plist)?);
        if ctx.compiler.as_ref().unwrap().keep_result {
            result.push(Instruction::PlistGet);
        }
        Ok(result)
    })
}
