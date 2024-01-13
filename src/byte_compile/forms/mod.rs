use std::collections::HashMap;

use crate::{bytecode::Instruction, Error, ErrorKind, TulispContext, TulispObject};

use super::{byte_compile::CompileResult, Compiler};

mod arithmetic_operations;
mod common;
mod comparison_of_numbers;
mod conditionals;
mod list_elements;
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
            ("equal", comparison_of_numbers::compile_fn_equal),
            // arithmetic
            ("+", arithmetic_operations::compile_fn_plus),
            ("-", arithmetic_operations::compile_fn_minus),
            ("*", arithmetic_operations::compile_fn_mul),
            ("/", arithmetic_operations::compile_fn_div),
            // other functions
            ("print", other_functions::compile_fn_print),
            ("setq", other_functions::compile_fn_setq),
            ("defun", other_functions::compile_fn_defun),
            ("progn", other_functions::compile_fn_progn),
            ("let", other_functions::compile_fn_let_star),
            ("let*", other_functions::compile_fn_let_star),
            // lists
            ("cons", other_functions::compile_fn_cons),
            ("list", other_functions::compile_fn_list),
            ("append", other_functions::compile_fn_append),
            // cxr
            ("car", list_elements::compile_fn_cxr),
            ("cdr", list_elements::compile_fn_cxr),
            ("caar", list_elements::compile_fn_cxr),
            ("cadr", list_elements::compile_fn_cxr),
            ("cdar", list_elements::compile_fn_cxr),
            ("cddr", list_elements::compile_fn_cxr),
            ("caaar", list_elements::compile_fn_cxr),
            ("caadr", list_elements::compile_fn_cxr),
            ("cadar", list_elements::compile_fn_cxr),
            ("caddr", list_elements::compile_fn_cxr),
            ("cdaar", list_elements::compile_fn_cxr),
            ("cdadr", list_elements::compile_fn_cxr),
            ("cddar", list_elements::compile_fn_cxr),
            ("cdddr", list_elements::compile_fn_cxr),
            ("caaaar", list_elements::compile_fn_cxr),
            ("caaadr", list_elements::compile_fn_cxr),
            ("caadar", list_elements::compile_fn_cxr),
            ("caaddr", list_elements::compile_fn_cxr),
            ("cadaar", list_elements::compile_fn_cxr),
            ("cadadr", list_elements::compile_fn_cxr),
            ("caddar", list_elements::compile_fn_cxr),
            ("cadddr", list_elements::compile_fn_cxr),
            ("cdaaar", list_elements::compile_fn_cxr),
            ("cdaadr", list_elements::compile_fn_cxr),
            ("cdadar", list_elements::compile_fn_cxr),
            ("cdaddr", list_elements::compile_fn_cxr),
            ("cddaar", list_elements::compile_fn_cxr),
            ("cddadr", list_elements::compile_fn_cxr),
            ("cdddar", list_elements::compile_fn_cxr),
            ("cddddr", list_elements::compile_fn_cxr),
            // conditionals
            ("if", conditionals::compile_fn_if),
            ("cond", conditionals::compile_fn_cond),
            ("while", conditionals::compile_fn_while),
            // noop
            ("defmacro", other_functions::compile_fn_noop),
            ("macroexpand", other_functions::compile_fn_noop),
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
