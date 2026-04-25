use std::collections::HashMap;

use crate::{
    bytecode::{Bytecode, Instruction},
    object::wrappers::generic::SharedMut,
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
    pub current_defun: Option<TulispObject>,
    /// Lexical bindings introduced by the enclosing `let` / `let*`
    /// forms in source-order. Forms that emit a function-escaping
    /// instruction (`TailCall`, self-recursion's
    /// `Jump(Pos::Abs(0))`) read this list and emit `EndScope`s for
    /// the active bindings before the escape — otherwise the trailing
    /// `EndScope`s appended by `compile_fn_let_star` are skipped on
    /// the escape path and the bindings stay pushed on `LEX_STACKS`
    /// permanently. Saved/restored at lambda + defun boundaries so
    /// nested function bodies start fresh.
    pub active_let_scopes: Vec<TulispObject>,
    label_counter: usize,
}

impl Compiler {
    pub fn new(vm_compilers: VMCompilers) -> Self {
        Compiler {
            vm_compilers,
            defun_args: HashMap::new(),
            bytecode: Bytecode::new(),
            keep_result: true,
            current_defun: None,
            active_let_scopes: Vec::new(),
            label_counter: 0,
        }
    }

    pub fn new_label(&mut self) -> TulispObject {
        self.label_counter += 1;
        TulispObject::symbol(format!(":{}", self.label_counter), true)
    }

    pub fn reset_label_counter(&mut self) {
        self.label_counter = 0;
    }
}

pub fn compile(ctx: &mut TulispContext, value: &TulispObject) -> Result<Bytecode, Error> {
    // Snapshot the accumulated function set before compilation so the
    // returned `Bytecode` carries only the defuns produced by *this*
    // compile. The compiler itself keeps accumulating so subsequent
    // compiles (e.g., REPL-style) can resolve names that were defined
    // earlier, and the machine's own `bytecode.functions` grows via
    // `import_functions` on each run.
    let before: std::collections::HashSet<usize> = ctx
        .compiler
        .as_ref()
        .unwrap()
        .bytecode
        .functions
        .keys()
        .copied()
        .collect();
    let output = compile_progn(ctx, value)?;
    let compiler = ctx.compiler.as_mut().unwrap();
    compiler.bytecode.global = SharedMut::new(output);
    let new_functions = compiler
        .bytecode
        .functions
        .iter()
        .filter(|(k, _)| !before.contains(k))
        .map(|(k, v)| (*k, v.clone()))
        .collect();
    Ok(Bytecode {
        global: compiler.bytecode.global.clone(),
        functions: new_functions,
    })
}

pub fn compile_progn(
    ctx: &mut TulispContext,
    value: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    // Pre-pass: register the arities of every top-level
    // `(defun NAME PARAMS …)` in this progn before compiling any
    // body. Without this, `mark_tail_calls` only sees siblings that
    // were defined earlier in the form list, so cyclic mutual
    // recursion `(defun a () (b))` / `(defun b () (a))` wouldn't get
    // both directions TCO'd.
    pre_register_defun_arities(ctx, value);

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

/// Walk a progn-shaped form list and pre-populate `defun_args` with
/// arity entries for every top-level `(defun NAME (PARAMS) …)` it
/// contains. Used so `mark_tail_calls` can identify mutual-recursion
/// targets even before their own `compile_fn_defun` runs.
///
/// Only required/optional/rest **lengths** are consulted by
/// `mark_tail_calls` and `compile_fn_defun_bounce_call`'s non-self
/// arity check — the actual `TulispObject` values stored here are
/// just placeholders (the parameter names from source). When the
/// real `compile_fn_defun` runs for that defun, it overwrites this
/// entry with one that carries fresh `LexicalBinding` objects.
fn pre_register_defun_arities(ctx: &mut TulispContext, body: &TulispObject) {
    for expr in body.base_iter() {
        try_pre_register_one(ctx, &expr);
    }
}

fn try_pre_register_one(ctx: &mut TulispContext, expr: &TulispObject) {
    if !expr.consp() {
        return;
    }
    let Ok(head) = expr.car() else { return };
    let Ok(head_sym) = head.as_symbol() else {
        return;
    };
    if head_sym != "defun" {
        return;
    }
    let Ok(after_defun) = expr.cdr() else { return };
    let Ok(name_obj) = after_defun.car() else {
        return;
    };
    let Ok(after_name) = after_defun.cdr() else {
        return;
    };
    let Ok(params) = after_name.car() else { return };

    let mut required = Vec::new();
    let mut optional = Vec::new();
    let mut rest_param: Option<TulispObject> = None;
    let mut is_optional = false;
    let mut is_rest = false;
    for p in params.base_iter() {
        if p.eq(&ctx.keywords.amp_optional) {
            is_optional = true;
        } else if p.eq(&ctx.keywords.amp_rest) {
            is_optional = false;
            is_rest = true;
        } else if is_rest {
            rest_param = Some(p.clone());
        } else if is_optional {
            optional.push(p.clone());
        } else {
            required.push(p.clone());
        }
    }

    let params_struct = VMDefunParams {
        required,
        optional,
        rest: rest_param,
    };
    let compiler = ctx.compiler.as_mut().unwrap();
    compiler
        .defun_args
        .insert(name_obj.addr_as_usize(), params_struct);
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
    match &*value.inner_ref() {
        (TulispValue::Quote { value }, _) => {
            return compile_back_quote(ctx, value).map(|mut v| {
                v.push(Instruction::Quote);
                v
            })
        }
        (TulispValue::Unquote { value }, _) => {
            return compile_expr(ctx, value).map_err(|e| e.with_trace(value.clone()));
        }
        (TulispValue::Splice { .. }, _) => {
            return Err(Error::new(
                crate::ErrorKind::SyntaxError,
                "Splice must be within a backquoted list.".to_string(),
            ));
        }
        (TulispValue::List { .. }, _) => {}
        _ => return Ok(vec![Instruction::Push(value.clone().into())]),
    }
    let mut result = vec![];

    let mut value = value.clone();
    let mut items = 0;
    let mut need_list = true;
    let mut need_append = false;
    loop {
        value.car_and_then(|first| {
            let first_inner = &*first.inner_ref();
            if let (TulispValue::Unquote { value }, _) = first_inner {
                items += 1;
                result.append(
                    &mut compile_expr(ctx, &value).map_err(|e| e.with_trace(first.clone()))?,
                );
            } else if let (TulispValue::Splice { value }, _) = first_inner {
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
        if let (TulispValue::Unquote { value }, _) = &*rest.inner_ref() {
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
        (TulispValue::Number { .. }, _) => {
            if compiler.keep_result {
                // Preserve the original AST cell so identity-based ops
                // (`eq`) keep working when an int literal here is
                // compared against the same int from the parser's
                // per-parse cache. Going through `Number::into()`
                // would route through `INT_CACHE` instead and break
                // pointer equality across the two cells.
                return Ok(vec![Instruction::Push(expr.clone())]);
            } else {
                return Ok(vec![]);
            }
        }
        (TulispValue::Nil, _) => {
            if compiler.keep_result {
                return Ok(vec![Instruction::Push(false.into())]);
            } else {
                return Ok(vec![]);
            }
        }
        (TulispValue::T, _) => {
            if compiler.keep_result {
                return Ok(vec![Instruction::Push(true.into())]);
            } else {
                return Ok(vec![]);
            }
        }
        (TulispValue::String { .. }, _) | (TulispValue::Any(_), _) => {
            if compiler.keep_result {
                return Ok(vec![Instruction::Push(expr.clone().into())]);
            } else {
                return Ok(vec![]);
            }
        }
        (TulispValue::Lambda { .. }, _)
        | (TulispValue::Func(_), _)
        | (TulispValue::Defun { .. }, _)
        | (TulispValue::CompiledDefun { .. }, _)
        | (TulispValue::Macro(_), _)
        | (TulispValue::Defmacro { .. }, _)
        | (TulispValue::Bounce, _) => return Ok(vec![]),

        (TulispValue::Backquote { value }, _) => compile_back_quote(ctx, value),
        (TulispValue::Quote { value }, _) | (TulispValue::Sharpquote { value }, _) => {
            if compiler.keep_result {
                return Ok(vec![Instruction::Push(value.clone().into())]);
            } else {
                return Ok(vec![]);
            }
        }
        (TulispValue::List { .. }, _) => {
            drop(expr_ref);
            compile_form(ctx, expr).map_err(|e| e.with_trace(expr.clone()))
        }
        (TulispValue::Symbol { .. }, _) | (TulispValue::LexicalBinding { .. }, _) => {
            if !compiler.keep_result {
                return Ok(vec![]);
            }
            return Ok(vec![if expr.keywordp() {
                Instruction::Push(expr.clone())
            } else {
                Instruction::Load(expr.clone())
            }]);
        }
        (TulispValue::Unquote { .. }, _) | (TulispValue::Splice { .. }, _) => {
            return Err(Error::new(
                crate::ErrorKind::SyntaxError,
                "Unquote without backquote".to_string(),
            ));
        }
    }
}
