use std::collections::HashMap;

use crate::{
    Error, TulispContext, TulispObject, TulispValue,
    bytecode::{Bytecode, Instruction},
    object::wrappers::generic::SharedMut,
};

use super::forms::{VMCompilers, compile_form};

#[derive(Default, Clone)]
pub(crate) struct VMDefunParams {
    pub required: Vec<TulispObject>,
    pub optional: Vec<TulispObject>,
    pub rest: Option<TulispObject>,
}

impl VMDefunParams {
    /// The arity this parameter list accepts.
    pub(crate) fn arity(&self) -> crate::value::DefunArity {
        crate::value::DefunArity {
            required: self.required.len(),
            optional: self.optional.len(),
            has_rest: self.rest.is_some(),
        }
    }
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
            bytecode: Bytecode::default(),
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
    // earlier, and the machine's own function table grows on each
    // run.
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
    // Assemble the global instruction stream. Per-function bodies
    // were already assembled at their `CompiledDefun` boundary
    // inside `compile_fn_defun`.
    let (output, global_trace_ranges) = crate::bytecode::bytecode::assemble(output)?;
    let global_trace_ranges = crate::object::wrappers::generic::Shared::new(global_trace_ranges);
    let compiler = ctx.compiler.as_mut().unwrap();
    compiler.bytecode.global = SharedMut::new(output);
    compiler.bytecode.global_trace_ranges = global_trace_ranges.clone();
    let new_functions = compiler
        .bytecode
        .functions
        .iter()
        .filter(|(k, _)| !before.contains(k))
        .map(|(k, v)| (*k, v.clone()))
        .collect();
    Ok(Bytecode {
        global: compiler.bytecode.global.clone(),
        global_trace_ranges,
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
    // Restore `keep_result` even when a form fails to compile, so
    // the context stays usable after the error.
    let mut compiled = Ok(());
    for expr in value.base_iter() {
        if let Some(prev) = &prev {
            match compile_expr(ctx, prev) {
                Ok(mut code) => result.append(&mut code),
                Err(e) => {
                    compiled = Err(e);
                    break;
                }
            }
        }
        prev = Some(expr);
    }
    let compiler = ctx.compiler.as_mut().unwrap();
    compiler.keep_result = keep_result;
    #[allow(dropping_references)]
    drop(compiler);
    compiled?;
    if let Some(prev) = prev {
        result.append(&mut compile_expr(ctx, &prev)?);
    } else if keep_result {
        // Empty progn body — `(progn)` and `(let ((x 1)))` both end
        // up here. Per Emacs semantics, the form's value is `nil`.
        // Without this push, the VM stack underflows when the caller
        // expects a value.
        result.push(Instruction::Push(false.into()));
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

/// Compile a progn whose value is dropped, such as a loop body.
/// The caller's `keep_result` is restored even when compiling fails.
pub(crate) fn compile_progn_drop_result(
    ctx: &mut TulispContext,
    expr: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    let compiler = ctx.compiler.as_mut().unwrap();
    let keep_result = compiler.keep_result;
    compiler.keep_result = false;
    #[allow(dropping_references)]
    drop(compiler);
    let ret = compile_progn(ctx, expr);
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

/// Compile a backquoted form at quasi-quote `depth` (1 inside the
/// outer `\``, bumped by inner `\``, decremented by `,` / `,@`).
/// At depth 1 unquote/splice expressions are evaluated: a `,X`
/// element becomes the value of `X`, and a `,@X` element splices in
/// the elements of that value. A lone `,X` or `,@X` template is the
/// value of `X` itself.
/// At depth > 1 they're treated as data: the inner expression is
/// compiled at `depth - 1` and the result is wrapped back in a
/// `Backquote` / `Unquote` / `Splice` cell with the matching `Wrap*`
/// instruction. A `,@X` in a dotted tail, `(a . ,@X)`, is data at
/// every depth, because Emacs reads it as `(a \,@ X)`: it stays, and
/// `X` is compiled at the depth of the list.
/// This native compilation matches Emacs' nested
/// backquote semantics — `\`(a \`(b ,,x ,y) c)` with `x = 1`
/// produces `(a \`(b ,1 ,y) c)` — without falling back to a TW
/// runtime helper that would re-borrow `ctx.vm` on `CompiledDefun`
/// callees.
fn compile_back_quote(
    ctx: &mut TulispContext,
    value: &TulispObject,
    depth: u32,
) -> Result<Vec<Instruction>, Error> {
    match &*value.inner_ref() {
        (TulispValue::Quote { value }, _) => {
            // `'X` inside a backquote is data — descend at the same
            // depth so nested unquotes inside still resolve.
            return compile_back_quote_operand(ctx, value, depth, false).map(|mut v| {
                v.push(Instruction::Quote);
                v
            });
        }
        (TulispValue::Unquote { value }, _) => {
            if depth == 1 {
                return compile_expr(ctx, value).map_err(|e| e.with_trace(value.clone()));
            }
            let mut v = compile_back_quote_operand(ctx, value, depth - 1, true)?;
            v.push(Instruction::WrapUnquote);
            return Ok(v);
        }
        (TulispValue::Splice { value }, _) => {
            if depth == 1 {
                // A whole template `,@x` is the value of `x`.
                return compile_expr(ctx, value).map_err(|e| e.with_trace(value.clone()));
            }
            let mut v = compile_back_quote_operand(ctx, value, depth - 1, true)?;
            v.push(Instruction::WrapSplice);
            return Ok(v);
        }
        (TulispValue::Backquote { value }, _) => {
            let mut v = compile_back_quote(ctx, value, depth + 1)?;
            v.push(Instruction::WrapBackquote);
            return Ok(v);
        }
        (TulispValue::List { .. }, _) => {}
        _ => return Ok(vec![Instruction::Push(value.clone())]),
    }
    let mut result = vec![];

    // Like Emacs, build the list with one `append` whose arguments
    // are the value of each splice and a list of each run of other
    // elements. Every element is evaluated before they are joined,
    // and the last argument is shared.
    let mut elements = value.base_iter();
    // Elements pushed since the last splice.
    let mut items = 0;
    // Arguments for the `append`, not counting `items`.
    let mut pieces = 0;
    for first in elements.by_ref() {
        let first_inner = &*first.inner_ref();
        if let (TulispValue::Unquote { value }, _) = first_inner {
            items += 1;
            if depth == 1 {
                result.append(
                    &mut compile_expr(ctx, value).map_err(|e| e.with_trace(first.clone()))?,
                );
            } else {
                result.append(&mut compile_back_quote_operand(
                    ctx,
                    value,
                    depth - 1,
                    true,
                )?);
                result.push(Instruction::WrapUnquote);
            }
        } else if let (TulispValue::Splice { value }, _) = first_inner {
            if depth == 1 {
                if items > 0 {
                    result.push(Instruction::List(items));
                    pieces += 1;
                    items = 0;
                }
                result.append(
                    &mut compile_expr(ctx, value).map_err(|e| e.with_trace(first.clone()))?,
                );
                pieces += 1;
            } else {
                // depth > 1: splice at this level is just data —
                // wrap as a Splice value and treat as one element.
                items += 1;
                result.append(&mut compile_back_quote_operand(
                    ctx,
                    value,
                    depth - 1,
                    true,
                )?);
                result.push(Instruction::WrapSplice);
            }
        } else if let (TulispValue::Backquote { value }, _) = first_inner {
            items += 1;
            result.append(&mut compile_back_quote(ctx, value, depth + 1)?);
            result.push(Instruction::WrapBackquote);
        } else {
            items += 1;
            result.append(&mut compile_back_quote(ctx, &first, depth)?);
        }
    }
    // A template that loops back is an error here.
    let rest = elements.tail().map_err(|e| e.with_trace(value.clone()))?;
    if rest.null() {
        if items > 0 {
            result.push(Instruction::List(items));
            pieces += 1;
        }
    } else {
        if let (TulispValue::Splice { value }, _) = &*rest.inner_ref() {
            // A `,@` in the dotted tail, `(a . ,@x)`, splices nothing
            // and stays, and `x` is walked at the same depth as `a`.
            result.append(&mut compile_back_quote_operand(ctx, value, depth, true)?);
            result.push(Instruction::WrapSplice);
        } else {
            result.append(&mut compile_back_quote(ctx, &rest, depth)?);
        }
        // Cons each element since the last splice onto the tail.
        result.extend(std::iter::repeat_n(Instruction::Cons, items));
        pieces += 1;
    }
    if pieces > 1 {
        result.push(Instruction::Append(pieces));
    }
    Ok(result)
}

/// Compiles `X`, the one operand of a `,X` or `,@X` kept as data, or
/// of a `'X`, at `depth`, as `eval_back_quote_operand` walks it: a
/// `,@Y` for `depth` 1 gives the one element of the value of `Y`, or
/// nil for an empty list when `empty_is_nil`.
fn compile_back_quote_operand(
    ctx: &mut TulispContext,
    x: &TulispObject,
    depth: u32,
    empty_is_nil: bool,
) -> Result<Vec<Instruction>, Error> {
    if depth == 1
        && let (TulispValue::Splice { value }, _) = &*x.inner_ref()
    {
        let mut result = compile_expr(ctx, value).map_err(|e| e.with_trace(x.clone()))?;
        result.push(Instruction::SoleElement { empty_is_nil });
        return Ok(result);
    }
    compile_back_quote(ctx, x, depth)
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
                Ok(vec![Instruction::Push(expr.clone())])
            } else {
                Ok(vec![])
            }
        }
        (TulispValue::Nil, _) | (TulispValue::T, _) => {
            if compiler.keep_result {
                // Push the parsed object itself, not a fresh
                // `false.into()` / `true.into()`. Otherwise a `nil`
                // / `t` argument loses its source span and error
                // backtraces miss the trace line for it (TW path
                // keeps the span, so VM and TW would diverge).
                Ok(vec![Instruction::Push(expr.clone())])
            } else {
                Ok(vec![])
            }
        }
        (TulispValue::String { .. }, _) | (TulispValue::Any(_), _) => {
            if compiler.keep_result {
                Ok(vec![Instruction::Push(expr.clone())])
            } else {
                Ok(vec![])
            }
        }
        // A function value evaluates to itself.
        (TulispValue::Lambda { .. }, _)
        | (TulispValue::Func(_), _)
        | (TulispValue::Defun { .. }, _)
        | (TulispValue::CompiledDefun { .. }, _)
        | (TulispValue::Macro(_), _)
        | (TulispValue::Defmacro { .. }, _) => {
            if compiler.keep_result {
                Ok(vec![Instruction::Push(expr.clone())])
            } else {
                Ok(vec![])
            }
        }
        (TulispValue::Bounce, _) => Ok(vec![]),

        (TulispValue::Backquote { value }, _) => {
            // The unquotes run even when the list is not used.
            let keep_result = compiler.keep_result;
            compiler.keep_result = true;
            let result = compile_back_quote(ctx, value, 1);
            if let Some(compiler) = ctx.compiler.as_mut() {
                compiler.keep_result = keep_result;
            }
            let mut result = result.map_err(|e| e.with_trace(expr.clone()))?;
            if !keep_result {
                result.push(Instruction::Pop);
            }
            Ok(result)
        }
        (TulispValue::Quote { value }, _) | (TulispValue::Sharpquote { value }, _) => {
            if compiler.keep_result {
                Ok(vec![Instruction::Push(value.clone())])
            } else {
                Ok(vec![])
            }
        }
        (TulispValue::List { .. }, _) => {
            drop(expr_ref);
            // Wrap the form's compiled bytecode with `PushTrace` /
            // `PopTrace` markers. `assemble` lifts these into a
            // side-table at compile time so the runtime pays
            // nothing for them on the happy path; on the error
            // path, `run_impl` looks up which ranges contain the
            // failing PC and applies their forms via `with_trace`.
            // Same shape TW's `eval_basic` produces.
            let mut inner = compile_form(ctx, expr).map_err(|e| e.with_trace(expr.clone()))?;
            if inner.is_empty() {
                return Ok(inner);
            }
            let mut wrapped = Vec::with_capacity(inner.len() + 2);
            wrapped.push(Instruction::PushTrace(expr.clone()));
            wrapped.append(&mut inner);
            wrapped.push(Instruction::PopTrace);
            Ok(wrapped)
        }
        (TulispValue::Symbol { .. }, _) | (TulispValue::LexicalBinding { .. }, _) => {
            if !compiler.keep_result {
                return Ok(vec![]);
            }
            Ok(vec![if expr.keywordp() {
                Instruction::Push(expr.clone())
            } else {
                Instruction::Load(expr.clone())
            }])
        }
        (TulispValue::Unquote { .. }, _) => Err(Error::new(
            crate::ErrorKind::SyntaxError,
            "Unquote without backquote".to_string(),
        )),
        (TulispValue::Splice { .. }, _) => Err(Error::new(
            crate::ErrorKind::SyntaxError,
            "Splice without backquote".to_string(),
        )),
    }
}

#[cfg(test)]
mod tests {
    use crate::TulispContext;

    // A form that fails to compile must not leave `keep_result`
    // false, or every later program would evaluate to nil. The
    // expected value is a Rust literal on purpose: a Lisp one would
    // compile to nil as well.
    #[test]
    fn a_compile_error_leaves_the_context_usable() {
        let ctx = &mut TulispContext::new();
        assert!(ctx.eval_string("(dolist 5) 1").is_err());
        assert_eq!(ctx.eval_string("(+ 1 2)").unwrap().to_string(), "3");
    }
}
