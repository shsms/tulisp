use crate::{
    bytecode::{compiler::compiler::compile_expr, instruction::Cxr, Instruction},
    Error, ErrorKind, TulispContext, TulispObject,
};

pub(super) fn compile_fn_cxr(
    ctx: &mut TulispContext,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    let mut result =
        ctx.compile_1_arg_call(name, args, false, |ctx, arg1, _| compile_expr(ctx, arg1))?;
    let compiler = ctx.compiler.as_mut().unwrap();
    if compiler.keep_result {
        let name = name.to_string();
        match name.as_str() {
            "car" => result.push(Instruction::Cxr(Cxr::Car)),
            "cdr" => result.push(Instruction::Cxr(Cxr::Cdr)),
            "caar" => result.push(Instruction::Cxr(Cxr::Caar)),
            "cadr" => result.push(Instruction::Cxr(Cxr::Cadr)),
            "cdar" => result.push(Instruction::Cxr(Cxr::Cdar)),
            "cddr" => result.push(Instruction::Cxr(Cxr::Cddr)),
            "caaar" => result.push(Instruction::Cxr(Cxr::Caaar)),
            "caadr" => result.push(Instruction::Cxr(Cxr::Caadr)),
            "cadar" => result.push(Instruction::Cxr(Cxr::Cadar)),
            "caddr" => result.push(Instruction::Cxr(Cxr::Caddr)),
            "cdaar" => result.push(Instruction::Cxr(Cxr::Cdaar)),
            "cdadr" => result.push(Instruction::Cxr(Cxr::Cdadr)),
            "cddar" => result.push(Instruction::Cxr(Cxr::Cddar)),
            "cdddr" => result.push(Instruction::Cxr(Cxr::Cdddr)),
            "caaaar" => result.push(Instruction::Cxr(Cxr::Caaaar)),
            "caaadr" => result.push(Instruction::Cxr(Cxr::Caaadr)),
            "caadar" => result.push(Instruction::Cxr(Cxr::Caadar)),
            "caaddr" => result.push(Instruction::Cxr(Cxr::Caaddr)),
            "cadaar" => result.push(Instruction::Cxr(Cxr::Cadaar)),
            "cadadr" => result.push(Instruction::Cxr(Cxr::Cadadr)),
            "caddar" => result.push(Instruction::Cxr(Cxr::Caddar)),
            "cadddr" => result.push(Instruction::Cxr(Cxr::Cadddr)),
            "cdaaar" => result.push(Instruction::Cxr(Cxr::Cdaaar)),
            "cdaadr" => result.push(Instruction::Cxr(Cxr::Cdaadr)),
            "cdadar" => result.push(Instruction::Cxr(Cxr::Cdadar)),
            "cdaddr" => result.push(Instruction::Cxr(Cxr::Cdaddr)),
            "cddaar" => result.push(Instruction::Cxr(Cxr::Cddaar)),
            "cddadr" => result.push(Instruction::Cxr(Cxr::Cddadr)),
            "cdddar" => result.push(Instruction::Cxr(Cxr::Cdddar)),
            "cddddr" => result.push(Instruction::Cxr(Cxr::Cddddr)),
            _ => return Err(Error::new(ErrorKind::Undefined, "unknown cxr".to_string())),
        }
    }
    Ok(result)
}
