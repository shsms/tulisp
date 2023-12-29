use std::collections::HashMap;

use crate::{vm::Instruction, Error, ErrorKind, TulispContext, TulispObject};

use super::{byte_compile::CompileResult, Compiler};

mod arithmetic_operations;
mod common;
mod comparison_of_numbers;
mod other_functions;

type FnCallCompiler = fn(&mut Compiler, &TulispObject, &TulispObject) -> CompileResult;

pub(crate) struct VMFunctions {
    // TulispObject.addr() -> implementation
    pub functions: HashMap<usize, FnCallCompiler>,
}

macro_rules! map_fn_call_compilers {
    ($ctx:ident, $functions: ident, $(($name:literal, $compiler:path),)+) => {
        $(
            $functions.insert(
                $ctx.intern($name).addr_as_usize(),
                $compiler as FnCallCompiler,
            );
        )+
    };
}

impl VMFunctions {
    pub fn new(ctx: &mut TulispContext) -> Self {
        let mut functions = HashMap::new();
        map_fn_call_compilers! {
            ctx, functions,
            ("<=", comparison_of_numbers::compile_fn_le),
            ("<", comparison_of_numbers::compile_fn_lt),
            (">=", comparison_of_numbers::compile_fn_ge),
            (">", comparison_of_numbers::compile_fn_gt),
            ("eq", comparison_of_numbers::compile_fn_eq),
            ("+", arithmetic_operations::compile_fn_plus),
            ("-", arithmetic_operations::compile_fn_minus),
            ("print", other_functions::compile_fn_print),
            ("setq", other_functions::compile_fn_setq),
            ("if", other_functions::compile_fn_if),
            ("defun", other_functions::compile_fn_defun),
            ("progn", other_functions::compile_fn_progn),
        }
        VMFunctions { functions }
    }
}

impl Compiler<'_> {
    pub(super) fn compile_form(
        &mut self,
        cons: &crate::cons::Cons,
    ) -> Result<Vec<Instruction>, Error> {
        let name = cons.car();
        let args = cons.cdr();
        if let Some(compiler) = self.functions.functions.get(&name.addr_as_usize()) {
            compiler(self, &name, &args)
        } else {
            Err(Error::new(
                ErrorKind::Undefined,
                format!("undefined function: {}", name),
            ))
        }
    }
}
