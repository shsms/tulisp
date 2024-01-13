use crate::{
    byte_compile::Compiler,
    bytecode::{Instruction, InstructionCxr},
    Error, ErrorKind, TulispObject,
};

pub(super) fn compile_fn_cxr(
    compiler: &mut Compiler<'_>,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    let mut result = compiler.compile_1_arg_call(name, args, false, |compiler, arg1, _| {
        compiler.compile_expr(arg1)
    })?;
    if compiler.keep_result {
        let name = name.to_string();
        match name.as_str() {
            "car" => result.push(Instruction::Cxr(InstructionCxr::Car)),
            "cdr" => result.push(Instruction::Cxr(InstructionCxr::Cdr)),
            "caar" => result.push(Instruction::Cxr(InstructionCxr::Caar)),
            "cadr" => result.push(Instruction::Cxr(InstructionCxr::Cadr)),
            "cdar" => result.push(Instruction::Cxr(InstructionCxr::Cdar)),
            "cddr" => result.push(Instruction::Cxr(InstructionCxr::Cddr)),
            "caaar" => result.push(Instruction::Cxr(InstructionCxr::Caaar)),
            "caadr" => result.push(Instruction::Cxr(InstructionCxr::Caadr)),
            "cadar" => result.push(Instruction::Cxr(InstructionCxr::Cadar)),
            "caddr" => result.push(Instruction::Cxr(InstructionCxr::Caddr)),
            "cdaar" => result.push(Instruction::Cxr(InstructionCxr::Cdaar)),
            "cdadr" => result.push(Instruction::Cxr(InstructionCxr::Cdadr)),
            "cddar" => result.push(Instruction::Cxr(InstructionCxr::Cddar)),
            "cdddr" => result.push(Instruction::Cxr(InstructionCxr::Cdddr)),
            "caaaar" => result.push(Instruction::Cxr(InstructionCxr::Caaaar)),
            "caaadr" => result.push(Instruction::Cxr(InstructionCxr::Caaadr)),
            "caadar" => result.push(Instruction::Cxr(InstructionCxr::Caadar)),
            "caaddr" => result.push(Instruction::Cxr(InstructionCxr::Caaddr)),
            "cadaar" => result.push(Instruction::Cxr(InstructionCxr::Cadaar)),
            "cadadr" => result.push(Instruction::Cxr(InstructionCxr::Cadadr)),
            "caddar" => result.push(Instruction::Cxr(InstructionCxr::Caddar)),
            "cadddr" => result.push(Instruction::Cxr(InstructionCxr::Cadddr)),
            "cdaaar" => result.push(Instruction::Cxr(InstructionCxr::Cdaaar)),
            "cdaadr" => result.push(Instruction::Cxr(InstructionCxr::Cdaadr)),
            "cdadar" => result.push(Instruction::Cxr(InstructionCxr::Cdadar)),
            "cdaddr" => result.push(Instruction::Cxr(InstructionCxr::Cdaddr)),
            "cddaar" => result.push(Instruction::Cxr(InstructionCxr::Cddaar)),
            "cddadr" => result.push(Instruction::Cxr(InstructionCxr::Cddadr)),
            "cdddar" => result.push(Instruction::Cxr(InstructionCxr::Cdddar)),
            "cddddr" => result.push(Instruction::Cxr(InstructionCxr::Cddddr)),
            _ => return Err(Error::new(ErrorKind::Undefined, "unknown cxr".to_string())),
        }
    }
    Ok(result)
}
