use std::collections::HashMap;

use crate::{
    vm::{Bytecode, Instruction, VMBindings},
    Error, TulispContext, TulispObject, TulispValue,
};

use super::forms::VMFunctions;

pub(crate) type CompileResult = Result<Vec<Instruction>, Error>;

#[allow(dead_code)]
pub(crate) struct Compiler<'a> {
    pub ctx: &'a mut TulispContext,
    pub functions: VMFunctions,
    pub defun_args: HashMap<usize, Vec<usize>>, // fn_name.addr_as_usize() -> arg symbol idx
    pub bindings: Vec<VMBindings>,
    pub symbol_to_binding_idx: HashMap<usize, usize>,
    pub keep_result: bool,
}

impl<'a> Compiler<'a> {
    pub fn new(ctx: &'a mut TulispContext) -> Self {
        let functions = VMFunctions::new(ctx);
        Compiler {
            ctx,
            functions,
            defun_args: HashMap::new(),
            bindings: Vec::new(),
            symbol_to_binding_idx: HashMap::new(),
            keep_result: true,
        }
    }

    pub fn get_symbol_idx(&mut self, symbol: &TulispObject) -> usize {
        let addr = &symbol.addr_as_usize();
        if let Some(idx) = self.symbol_to_binding_idx.get(addr) {
            *idx
        } else {
            self.bindings.push(VMBindings::new(symbol.to_string()));
            let idx = self.bindings.len() - 1;
            self.symbol_to_binding_idx.insert(*addr, idx);
            idx
        }
    }

    pub fn compile(mut self, value: &TulispObject) -> Result<Bytecode, Error> {
        Ok(Bytecode::new(
            self.compile_progn(value)?,
            self.bindings,
            self.symbol_to_binding_idx,
        ))
    }

    pub fn compile_progn(&mut self, value: &TulispObject) -> Result<Vec<Instruction>, Error> {
        let mut result = vec![];
        let mut prev = None;
        let keep_result = self.keep_result;
        for expr in value.base_iter() {
            if let Some(prev) = prev {
                self.keep_result = false;
                result.append(&mut self.compile_expr(&prev)?);
            }
            prev = Some(expr);
        }
        self.keep_result = keep_result;
        if let Some(prev) = prev {
            result.append(&mut self.compile_expr(&prev)?);
        }
        Ok(result)
    }

    pub(crate) fn compile_expr(&mut self, expr: &TulispObject) -> Result<Vec<Instruction>, Error> {
        match &*expr.inner_ref() {
            TulispValue::Int { .. }
            | TulispValue::Float { .. }
            | TulispValue::String { .. }
            | TulispValue::Lambda { .. }
            | TulispValue::Func(_)
            | TulispValue::Macro(_)
            | TulispValue::Defmacro { .. }
            | TulispValue::Any(_)
            | TulispValue::Bounce { .. }
            | TulispValue::Nil
            | TulispValue::Sharpquote { .. }
            | TulispValue::Backquote { .. }
            | TulispValue::Unquote { .. }
            | TulispValue::Splice { .. }
            | TulispValue::T => {
                if self.keep_result {
                    return Ok(vec![Instruction::Push(expr.clone())]);
                } else {
                    return Ok(vec![]);
                }
            }
            TulispValue::Quote { value } => {
                if self.keep_result {
                    return Ok(vec![Instruction::Push(value.clone())]);
                } else {
                    return Ok(vec![]);
                }
            }
            TulispValue::List { cons, .. } => self
                .compile_form(cons)
                .map_err(|e| e.with_trace(expr.clone())),
            TulispValue::Symbol { .. } => Ok(vec![Instruction::Load(self.get_symbol_idx(expr))]),
        }
    }

    pub(crate) fn compile_expr_keep_result(
        &mut self,
        expr: &TulispObject,
    ) -> Result<Vec<Instruction>, Error> {
        let keep_result = self.keep_result;
        self.keep_result = true;
        let ret = self.compile_expr(expr);
        self.keep_result = keep_result;
        ret
    }

    pub(crate) fn compile_progn_keep_result(
        &mut self,
        expr: &TulispObject,
    ) -> Result<Vec<Instruction>, Error> {
        let keep_result = self.keep_result;
        self.keep_result = true;
        let ret = self.compile_progn(expr);
        self.keep_result = keep_result;
        ret
    }
}
