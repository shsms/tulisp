use std::collections::HashMap;

use crate::{
    bytecode::Instruction, eval::macroexpand, Error, TulispContext, TulispObject, TulispValue,
};

use super::compiler::compile_expr;

mod arithmetic_operations;
mod common;
mod comparison_of_numbers;
mod conditionals;
mod list_elements;
mod other_functions;
mod plist;
mod setting;

type FnCallCompiler =
    fn(&mut TulispContext, &TulispObject, &TulispObject) -> Result<Vec<Instruction>, Error>;

pub(crate) struct VMCompilers {
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

impl VMCompilers {
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
            ("load", other_functions::compile_fn_load_file),
            ("print", other_functions::compile_fn_print),
            ("quote", other_functions::compile_fn_quote),
            ("defun", other_functions::compile_fn_defun),
            ("progn", other_functions::compile_fn_progn),
            // setting
            ("let", setting::compile_fn_let_star),
            ("let*", setting::compile_fn_let_star),
            ("setq", setting::compile_fn_setq),
            ("set", setting::compile_fn_set),
            // lists
            ("cons", other_functions::compile_fn_cons),
            ("list", other_functions::compile_fn_list),
            ("append", other_functions::compile_fn_append),
            ("plist-get", plist::compile_fn_plist_get),
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
            ("and", conditionals::compile_fn_and),
            ("or", conditionals::compile_fn_or),
            ("not", conditionals::compile_fn_not),
            // noop
            ("defmacro", other_functions::compile_fn_noop),
        }
        VMCompilers { functions }
    }
}

pub(super) fn compile_form(
    ctx: &mut TulispContext,
    form: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    let name = form.car()?;
    let args = form.cdr()?;
    if let Some(compiler) = ctx
        .compiler
        .as_ref()
        .unwrap()
        .vm_compilers
        .functions
        .get(&name.addr_as_usize())
    {
        return compiler(ctx, &name, &args);
    }
    if let Ok(func) = ctx.eval(&name) {
        match &*func.inner_ref() {
            TulispValue::Func(func) => {
                let compiler = ctx.compiler.as_mut().unwrap();
                return Ok(vec![
                    Instruction::Push(args.clone()),
                    Instruction::RustCall {
                        name: name.clone(),
                        func: func.clone(),
                        keep_result: compiler.keep_result,
                    },
                ]);
            }
            TulispValue::Defmacro { .. } | TulispValue::Macro(..) => {
                // TODO: this should not be necessary, this should be
                // handled in the parser instead.
                let form = macroexpand(ctx, form.clone())?;
                return compile_expr(ctx, &form);
            }
            _ => {}
        }
    }

    other_functions::compile_fn_defun_call(ctx, &name, &args)
}