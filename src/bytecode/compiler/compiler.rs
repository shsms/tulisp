use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    bytecode::{Bytecode, Instruction},
    Error, ErrorKind, TulispContext, TulispObject, TulispValue,
};

use super::forms::{compile_form, VMCompilers};

#[derive(Default, Clone)]
pub(crate) struct VMDefunParams {
    pub required: Vec<TulispObject>,
    pub optional: Vec<TulispObject>,
    pub rest: Option<TulispObject>,
}

#[allow(dead_code)]
pub(crate) struct Compiler {
    pub vm_compilers: VMCompilers,
    pub defun_args: HashMap<usize, VMDefunParams>, // fn_name.addr_as_usize() -> arg symbol idx
    pub bytecode: Bytecode,
    pub keep_result: bool,
    label_counter: usize,
}

impl Compiler {
    pub fn new(vm_compilers: VMCompilers) -> Self {
        Compiler {
            vm_compilers,
            defun_args: HashMap::new(),
            bytecode: Bytecode::new(),
            keep_result: true,
            label_counter: 0,
        }
    }

    pub fn new_label(&mut self) -> TulispObject {
        self.label_counter += 1;
        TulispObject::symbol(format!(":{}", self.label_counter), true)
    }
}

pub fn compile(ctx: &mut TulispContext, value: &TulispObject) -> Result<Bytecode, Error> {
    let output = compile_progn(ctx, value)?;
    let compiler = ctx.compiler.as_mut().unwrap();
    compiler.bytecode.global = Rc::new(RefCell::new(output));
    Ok(compiler.bytecode.clone())
}

pub fn compile_progn(
    ctx: &mut TulispContext,
    value: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    let mut result = vec![];
    let mut prev = None;
    let compiler = ctx.compiler.as_mut().unwrap();
    let keep_result = compiler.keep_result;
    compiler.keep_result = false;
    #[allow(dropping_references)]
    drop(compiler);
    for expr in value.base_iter() {
        if let Some(prev) = prev {
            result.append(&mut compile_expr(ctx, &prev)?);
        }
        prev = Some(expr);
    }
    let compiler = ctx.compiler.as_mut().unwrap();
    compiler.keep_result = keep_result;
    #[allow(dropping_references)]
    drop(compiler);
    if let Some(prev) = prev {
        result.append(&mut compile_expr(ctx, &prev)?);
    }
    Ok(result)
}

pub(crate) fn compile_expr_keep_result(
    ctx: &mut TulispContext,
    expr: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    let compiler = ctx.compiler.as_mut().unwrap();
    let keep_result = compiler.keep_result;
    compiler.keep_result = true;
    #[allow(dropping_references)]
    drop(compiler);
    let ret = compile_expr(ctx, expr);
    ctx.compiler.as_mut().unwrap().keep_result = keep_result;
    ret
}

pub(crate) fn compile_progn_keep_result(
    ctx: &mut TulispContext,
    expr: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    let compiler = ctx.compiler.as_mut().unwrap();
    let keep_result = compiler.keep_result;
    compiler.keep_result = true;
    #[allow(dropping_references)]
    drop(compiler);
    let ret = compile_progn(ctx, expr);
    ctx.compiler.as_mut().unwrap().keep_result = keep_result;
    ret
}

fn compile_back_quote(
    ctx: &mut TulispContext,
    value: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    let compiler = ctx.compiler.as_mut().unwrap();
    if !compiler.keep_result {
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
                result.append(&mut compile_expr(ctx, &value)?);
            } else if let TulispValue::Splice { value } = first_inner {
                let mut splice_result = compile_expr(ctx, &value)?;
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
                result.append(&mut compile_back_quote(ctx, first)?);
            }
            Ok(())
        })?;
        let rest = value.cdr()?;
        if let TulispValue::Unquote { value } = &*rest.inner_ref() {
            result.append(&mut compile_expr(ctx, &value)?);
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

pub(crate) fn compile_expr(
    ctx: &mut TulispContext,
    expr: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    let expr_ref = expr.inner_ref();
    let compiler = ctx.compiler.as_mut().unwrap();
    match &*expr_ref {
        TulispValue::Int { value } => {
            if compiler.keep_result {
                return Ok(vec![Instruction::Push(value.clone().into())]);
            } else {
                return Ok(vec![]);
            }
        }
        TulispValue::Float { value } => {
            if compiler.keep_result {
                return Ok(vec![Instruction::Push(value.clone().into())]);
            } else {
                return Ok(vec![]);
            }
        }
        TulispValue::Nil => {
            if compiler.keep_result {
                return Ok(vec![Instruction::Push(false.into())]);
            } else {
                return Ok(vec![]);
            }
        }
        TulispValue::T => {
            if compiler.keep_result {
                return Ok(vec![Instruction::Push(true.into())]);
            } else {
                return Ok(vec![]);
            }
        }
        TulispValue::String { .. } | TulispValue::Any(_) => {
            if compiler.keep_result {
                return Ok(vec![Instruction::Push(expr.clone().into())]);
            } else {
                return Ok(vec![]);
            }
        }
        TulispValue::Lambda { .. }
        | TulispValue::Func(_)
        | TulispValue::CompiledDefun { .. }
        | TulispValue::Macro(_)
        | TulispValue::Defmacro { .. }
        | TulispValue::Bounce { .. } => return Ok(vec![]),

        TulispValue::Backquote { value } => compile_back_quote(ctx, value),
        TulispValue::Quote { value } | TulispValue::Sharpquote { value } => {
            if compiler.keep_result {
                return Ok(vec![Instruction::Push(value.clone().into())]);
            } else {
                return Ok(vec![]);
            }
        }
        TulispValue::List { .. } => {
            drop(expr_ref);
            compile_form(ctx, expr).map_err(|e| e.with_trace(expr.clone()))
        }
        TulispValue::Symbol { .. } | TulispValue::LexicalBinding { .. } => {
            if !compiler.keep_result {
                return Ok(vec![]);
            }
            return Ok(vec![if expr.keywordp() {
                Instruction::Push(expr.clone())
            } else {
                Instruction::Load(expr.clone())
            }]);
        }
        TulispValue::Unquote { .. } | TulispValue::Splice { .. } => {
            return Err(Error::new(
                crate::ErrorKind::SyntaxError,
                "Unquote/Splice must be within a backquoted list.".to_string(),
            )
            .with_trace(expr.clone()));
        }
    }
}