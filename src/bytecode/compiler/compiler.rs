use std::collections::HashMap;

use crate::{
    bytecode::{Bytecode, Instruction, VMBindings},
    Error, ErrorKind, TulispContext, TulispObject, TulispValue,
};

use super::forms::VMFunctions;

#[allow(dead_code)]
pub(crate) struct Compiler<'a> {
    pub ctx: &'a mut TulispContext,
    pub functions: VMFunctions,
    pub defun_args: HashMap<usize, Vec<usize>>, // fn_name.addr_as_usize() -> arg symbol idx
    pub bindings: Vec<VMBindings>,
    pub symbol_to_binding_idx: HashMap<usize, Vec<usize>>,
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

    pub fn get_symbol_idx(&mut self, symbol: &TulispObject) -> (usize, bool) {
        let addr = &symbol.addr_as_usize();
        if let Some(scopes) = self.symbol_to_binding_idx.get(addr) {
            (*scopes.last().unwrap(), false)
        } else {
            self.bindings.push(VMBindings::new(symbol.to_string()));
            let idx = self.bindings.len() - 1;
            self.symbol_to_binding_idx.insert(*addr, vec![idx]);
            (idx, true)
        }
    }

    pub fn begin_scope(&mut self, symbol: &TulispObject) -> usize {
        let addr = &symbol.addr_as_usize();
        self.bindings.push(VMBindings::new(symbol.to_string()));
        let idx = self.bindings.len() - 1;
        if let Some(scopes) = self.symbol_to_binding_idx.get_mut(addr) {
            scopes.push(idx);
        } else {
            self.symbol_to_binding_idx
                .insert(*addr, vec![self.bindings.len() - 1]);
        }
        idx
    }

    pub fn end_scope(&mut self, symbol: &TulispObject) {
        let addr = &symbol.addr_as_usize();
        if let Some(scopes) = self.symbol_to_binding_idx.get_mut(addr) {
            scopes.pop();
        }
    }

    pub fn compile(mut self, value: &TulispObject) -> Result<Bytecode, Error> {
        Ok(Bytecode::new(self.compile_progn(value)?, self.bindings))
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
            TulispValue::Int { value } => {
                if self.keep_result {
                    return Ok(vec![Instruction::Push(value.clone().into())]);
                } else {
                    return Ok(vec![]);
                }
            }
            TulispValue::Float { value } => {
                if self.keep_result {
                    return Ok(vec![Instruction::Push(value.clone().into())]);
                } else {
                    return Ok(vec![]);
                }
            }
            TulispValue::Nil => {
                if self.keep_result {
                    return Ok(vec![Instruction::Push(false.into())]);
                } else {
                    return Ok(vec![]);
                }
            }
            TulispValue::T => {
                if self.keep_result {
                    return Ok(vec![Instruction::Push(true.into())]);
                } else {
                    return Ok(vec![]);
                }
            }
            TulispValue::String { .. }
            | TulispValue::Lambda { .. }
            | TulispValue::Func(_)
            | TulispValue::Macro(_)
            | TulispValue::Defmacro { .. }
            | TulispValue::Any(_)
            | TulispValue::Bounce { .. } => {
                if self.keep_result {
                    return Ok(vec![Instruction::Push(expr.clone().into())]);
                } else {
                    return Ok(vec![]);
                }
            }
            TulispValue::Backquote { value } => self.compile_back_quote(value),
            TulispValue::Quote { value } | TulispValue::Sharpquote { value } => {
                if self.keep_result {
                    return Ok(vec![Instruction::Push(value.clone().into())]);
                } else {
                    return Ok(vec![]);
                }
            }
            TulispValue::List { cons, .. } => self
                .compile_form(cons)
                .map_err(|e| e.with_trace(expr.clone())),
            TulispValue::Symbol { .. } => Ok(vec![Instruction::Load({
                match self.get_symbol_idx(expr) {
                    (_, true) => {
                        return Err(Error::new(
                            crate::ErrorKind::SyntaxError,
                            format!("Unbound symbol: {}", expr),
                        )
                        .with_trace(expr.clone()))
                    }
                    (idx, false) => idx,
                }
            })]),
            TulispValue::Unquote { .. } | TulispValue::Splice { .. } => {
                return Err(Error::new(
                    crate::ErrorKind::SyntaxError,
                    "Unquote/Splice must be within a backquoted list.".to_string(),
                )
                .with_trace(expr.clone()));
            }
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

    fn compile_back_quote(&mut self, value: &TulispObject) -> Result<Vec<Instruction>, Error> {
        if !self.keep_result {
            return Ok(vec![]);
        }
        if !value.consp() {
            return Ok(vec![Instruction::Push(value.clone().into())]);
        }
        let mut result = vec![];

        let mut value = value.clone();
        let mut items = 0;
        let mut need_list = true;
        let mut need_append = false;
        loop {
            value.car_and_then(|first| {
                let first_inner = &*first.inner_ref();
                if let TulispValue::Unquote { value } = first_inner {
                    items += 1;
                    result.append(&mut self.compile_expr(&value)?);
                } else if let TulispValue::Splice { value } = first_inner {
                    let mut splice_result = self.compile_expr(&value)?;
                    let list_inst = splice_result.pop().unwrap();
                    if let Instruction::List(n) = list_inst {
                        result.append(&mut splice_result);
                        items += n;
                    } else if let Instruction::Load(idx) = list_inst {
                        result.append(&mut splice_result);
                        result.push(Instruction::List(items));
                        if need_append {
                            result.push(Instruction::Append(2));
                        }
                        result.append(&mut vec![Instruction::Load(idx), Instruction::Append(2)]);
                        need_append = true;
                        items = 0;
                    } else {
                        if !value.consp() {
                            return Err(Error::new(
                                ErrorKind::SyntaxError,
                                format!(
                                    "Can only splice an inplace-list or a variable binding: {}",
                                    value
                                ),
                            )
                            .with_trace(first.clone()));
                        }
                        result.push(Instruction::List(items));
                        if need_append {
                            result.push(Instruction::Append(2));
                        }
                        result.append(&mut splice_result);
                        result.push(list_inst);
                        result.push(Instruction::Append(2));
                        need_append = true;
                        items = 0;
                    }
                } else {
                    items += 1;
                    result.append(&mut self.compile_back_quote(first)?);
                }
                Ok(())
            })?;
            let rest = value.cdr()?;
            if let TulispValue::Unquote { value } = &*rest.inner_ref() {
                result.append(&mut self.compile_expr(&value)?);
                result.push(Instruction::Cons);
                need_list = false;
                break;
            }
            if !rest.consp() {
                if !rest.null() {
                    result.push(Instruction::Push(rest.clone().into()));
                    result.push(Instruction::Cons);
                    need_list = false;
                }
                break;
            }
            value = rest;
        }
        if need_list {
            result.push(Instruction::List(items));
        }
        if need_append {
            result.push(Instruction::Append(2));
        }
        Ok(result)
    }
}
