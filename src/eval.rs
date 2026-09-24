use crate::object::wrappers::generic::SharedMut;
use crate::{
    TulispObject, TulispValue,
    context::{FrameGuard, TulispContext},
    error::Error,
    list,
};

/// Whether `obj` is a `(lambda ...)` list.
pub(crate) fn is_lambda_list(ctx: &TulispContext, obj: &TulispObject) -> bool {
    obj.consp()
        && obj
            .car_and_then(|car| Ok(car.eq(&ctx.keywords.lambda)))
            .unwrap_or(false)
}

/// Turn the evaluated first argument of `funcall` / `apply` into
/// the function to call. A symbol resolves to the function bound to
/// it, one lookup as in Emacs. A `(lambda ...)` list is compiled into
/// a function that sees global and special variables, on each call. A
/// macro or a special form is rejected here, as in Emacs. Anything else
/// is returned as is, and `funcall` rejects it if it is not callable.
pub(crate) fn resolve_function(
    ctx: &mut TulispContext,
    func: &TulispObject,
) -> Result<TulispObject, Error> {
    let resolved = if func.symbolp() {
        func.get()?
    } else if is_lambda_list(ctx, func) {
        ctx.eval(func)?
    } else {
        func.clone()
    };
    // A macro or a special form is not a function, as in Emacs.
    if matches!(
        &resolved.inner_ref().0,
        TulispValue::Macro(_)
            | TulispValue::Defmacro { .. }
            | TulispValue::SpecialForm
            | TulispValue::Special { .. }
    ) {
        return Err(Error::invalid_argument(format!("invalid function: {func}")));
    }
    Ok(resolved)
}

/// The arguments of `apply`: all but the last, then the elements of
/// the last, which must be a proper list.
pub(crate) fn spread_apply_args(mut args: Vec<TulispObject>) -> Result<Vec<TulispObject>, Error> {
    let Some(final_list) = args.pop() else {
        return Ok(args);
    };
    if !final_list.listp() {
        return Err(Error::type_mismatch(format!(
            "apply: last argument must be a list, got: {final_list}"
        )));
    }
    let mut items = final_list.base_iter();
    args.extend(items.by_ref());
    let tail = items.tail()?;
    if !tail.null() {
        return Err(Error::type_mismatch(format!(
            "apply: last argument must be a proper list, got non-nil tail: {tail}"
        )));
    }
    Ok(args)
}

pub fn macroexpand(ctx: &mut TulispContext, inp: TulispObject) -> Result<TulispObject, Error> {
    macroexpand_depth(ctx, inp, 0, 0)
}

/// The error for code nested deeper than LIMIT, `max-nesting-depth`.
fn nesting_exceeded(limit: u32) -> Error {
    Error::lisp_error(format!("Lisp nesting exceeds max-nesting-depth ({limit})"))
}

/// Expands FORM once when its head names a macro. `None` when it does
/// not.
fn macroexpand_1(
    ctx: &mut TulispContext,
    form: &TulispObject,
) -> Result<Option<TulispObject>, Error> {
    if !form.consp() {
        return Ok(None);
    }
    let head = form.car()?;
    // A head that is a macro value rather than a symbol expands too.
    let value = head.get().unwrap_or_else(|_| head.clone());
    let inner = value.inner_ref();
    let expansion = match &inner.0 {
        TulispValue::Macro(func) => {
            let func = func.clone();
            drop(inner);
            func(ctx, &form.cdr()?)
        }
        TulispValue::Defmacro { lambda, compiled } => {
            let (lambda, compiled) = (lambda.clone(), compiled.clone());
            drop(inner);
            expand_lisp_macro(ctx, &head, &lambda, &compiled, &form.cdr()?)
        }
        _ => return Ok(None),
    };
    expansion.map(Some).map_err(|e| e.with_trace(form.clone()))
}

/// Recurses on each macro expansion and each element's car (the cdr
/// chain is walked iteratively), so `depth` bounds the native
/// recursion — deeply nested input raises a catchable error instead of
/// overflowing the stack. `quote_depth` is the backquote depth, 0 being
/// code: inside a backquote only what an unquote runs is expanded. A
/// list in which nothing expands is returned as it is.
fn macroexpand_depth(
    ctx: &mut TulispContext,
    inp: TulispObject,
    depth: u32,
    quote_depth: u32,
) -> Result<TulispObject, Error> {
    let limit = ctx.max_nesting_depth();
    if depth > limit {
        return Err(nesting_exceeded(limit));
    }
    if !inp.consp() {
        return macroexpand_operand(ctx, inp, depth, quote_depth, false);
    }
    if quote_depth == 0
        && let Some(expansion) = macroexpand_1(ctx, &inp)?
    {
        return Ok(with_call_span(
            macroexpand_depth(ctx, expansion, depth + 1, 0)?,
            &inp,
        ));
    }
    // The copy starts at the first element that changes.
    let mut copy: Option<crate::cons::ListBuilder> = None;
    let mut items = inp.base_iter();
    let mut count = 0;
    for item in items.by_ref() {
        let expanded = macroexpand_depth(ctx, item.clone(), depth + 1, quote_depth)?;
        if copy.is_none() && !expanded.eq_ptr(&item) {
            copy = Some(builder_with_first(&inp, count));
        }
        if let Some(copy) = copy.as_mut() {
            copy.push(expanded);
        }
        count += 1;
    }
    let tail = items.tail()?;
    if !tail.null() {
        let expanded = macroexpand_operand(ctx, tail.clone(), depth, quote_depth, true)?;
        if copy.is_none() && !expanded.eq_ptr(&tail) {
            copy = Some(builder_with_first(&inp, count));
        }
        if let Some(copy) = copy.as_mut() {
            copy.append(expanded)?;
        }
    }
    Ok(match copy {
        Some(copy) => copy.build().with_span(inp.span()),
        None => inp,
    })
}

/// A list builder holding the first COUNT elements of LIST.
fn builder_with_first(list: &TulispObject, count: usize) -> crate::cons::ListBuilder {
    let mut copy = crate::cons::ListBuilder::new();
    for item in list.base_iter().take(count) {
        copy.push(item);
    }
    copy
}

/// Expands what `obj`, a non-list at backquote depth QUOTE_DEPTH, wraps
/// that may hold code; see [`wrapped_operand`]. `obj` itself when
/// nothing in it expands.
fn macroexpand_operand(
    ctx: &mut TulispContext,
    obj: TulispObject,
    depth: u32,
    quote_depth: u32,
    in_tail: bool,
) -> Result<TulispObject, Error> {
    let Some(operand) = wrapped_operand(&obj, quote_depth, in_tail) else {
        return Ok(obj);
    };
    let expanded = macroexpand_depth(ctx, operand.value.clone(), depth + 1, operand.depth)?;
    Ok(if expanded.eq_ptr(&operand.value) {
        obj
    } else {
        operand.rewrap(expanded)
    })
}

/// Runs F on each top-level form of FORMS, in order. Each form's macros
/// are expanded just before, by the macros defined so far. A form that
/// expands to a `progn` is entered: ON_PROGN gets its forms, then they
/// are handled one at a time. F's flag is true for the form whose value is
/// the program's; for a last `progn` with no forms, F gets nil.
pub(crate) fn for_each_top_level_form(
    ctx: &mut TulispContext,
    forms: &TulispObject,
    f: &mut dyn FnMut(&mut TulispContext, &TulispObject, bool) -> Result<(), Error>,
    on_progn: &mut dyn FnMut(&mut TulispContext, &TulispObject),
) -> Result<(), Error> {
    walk_top_level_forms(ctx, forms, true, 0, None, f, on_progn)
}

/// The walk of [`for_each_top_level_form`]. LAST says whether FORMS
/// hold the program's value. DEPTH counts the macro expansions and the
/// `progn`s the walk went through to reach FORMS; past
/// `max-nesting-depth` it is an error. CALL is the `progn`, or the
/// macro call, that FORMS came from: an error in an atom or in a form
/// without a source location points at it.
fn walk_top_level_forms(
    ctx: &mut TulispContext,
    forms: &TulispObject,
    last: bool,
    depth: u32,
    call: Option<&TulispObject>,
    f: &mut dyn FnMut(&mut TulispContext, &TulispObject, bool) -> Result<(), Error>,
    on_progn: &mut dyn FnMut(&mut TulispContext, &TulispObject),
) -> Result<(), Error> {
    if last && forms.null() {
        return f(ctx, &TulispObject::nil(), true);
    }
    let mut forms = forms.base_iter().peekable();
    while let Some(form) = forms.next() {
        let is_last = last && forms.peek().is_none();
        let limit = ctx.max_nesting_depth();
        // A symbol's span is not its own: one symbol is shared by every
        // place it is read. Other atoms are treated the same way.
        let site = call
            .filter(|_| !form.consp() || form.span().is_none())
            .unwrap_or(&form)
            .clone();
        let at_site = |e: Error| {
            if site.eq_ptr(&form) {
                e
            } else {
                e.with_trace(site.clone())
            }
        };
        let mut expanded = form.clone();
        let mut depth = depth;
        while let Some(expansion) = macroexpand_1(ctx, &expanded).map_err(at_site)? {
            depth += 1;
            if depth > limit {
                return Err(nesting_exceeded(limit).with_trace(site));
            }
            expanded = expansion;
        }
        if expanded.consp() && expanded.car()?.eq(&ctx.keywords.progn) {
            let body = expanded.cdr()?;
            crate::lists::length(&body)
                .map_err(|e| e.with_trace(with_call_span(expanded.clone(), &site)))?;
            depth += 1;
            if depth > limit {
                return Err(nesting_exceeded(limit).with_trace(site));
            }
            on_progn(ctx, &body);
            walk_top_level_forms(ctx, &body, is_last, depth, Some(&site), f, on_progn)?;
        } else {
            let mut expanded = macroexpand(ctx, expanded).map_err(at_site)?;
            // An atom from a `progn` runs in a `progn` that has the site's
            // location.
            if !expanded.consp() && !site.eq_ptr(&form) && site.span().is_some() {
                expanded = list!(ctx.keywords.progn.clone(), expanded)?.with_span(site.span());
            }
            let expanded = with_call_span(expanded, &site);
            f(ctx, &expanded, is_last)?;
        }
    }
    Ok(())
}

/// Expands a call to a macro defined in Lisp: runs its body, compiled
/// to a VM lambda on first use, on ARGS, the call's argument forms.
/// The compiled lambda is kept once a call to it succeeded, unless its
/// compile met a call to a name with no value yet.
fn expand_lisp_macro(
    ctx: &mut TulispContext,
    name: &TulispObject,
    lambda: &TulispObject,
    compiled: &SharedMut<Option<TulispObject>>,
    args: &TulispObject,
) -> Result<TulispObject, Error> {
    let args = crate::cons::collect_list(args, Ok)?;
    let cached = compiled.borrow().clone();
    if let Some(function) = cached {
        return crate::bytecode::call_function(ctx, &function, args);
    }
    let (function, complete) = compile_macro_body(ctx, name, lambda, compiled)?;
    let expansion = crate::bytecode::call_function(ctx, &function, args)?;
    if complete {
        *compiled.borrow_mut() = Some(function);
    }
    Ok(expansion)
}

/// Compiles a macro's `(lambda PARAMS . BODY)` form in the VM. A macro
/// used in its own body would expand without end, so it is refused. The
/// flag is false when the body calls a name with no value yet: a macro
/// defined later would then expand there, so the body is not kept.
fn compile_macro_body(
    ctx: &mut TulispContext,
    name: &TulispObject,
    lambda: &TulispObject,
    compiled: &SharedMut<Option<TulispObject>>,
) -> Result<(TulispObject, bool), Error> {
    let key = compiled.addr_as_usize();
    if ctx.compiling_macros.contains(&key) {
        return Err(Error::lisp_error(format!(
            "macro {name} is used in its own body"
        )));
    }
    let before = ctx.compiler.as_ref().unwrap().unbound_calls;
    let mut compiling = CompilingMacro::new(ctx, key)?;
    let function = compiling.eval(lambda)?;
    let complete = compiling.compiler.as_ref().unwrap().unbound_calls == before;
    Ok((function, complete))
}

/// A macro whose body is compiling: it is in `compiling_macros`, and
/// counts a frame toward the depth limit, until this is dropped, on any
/// path including a panic. Macros whose bodies use each other compile
/// one inside another, so the frame bounds a long chain of them.
struct CompilingMacro<'a>(FrameGuard<'a>);

impl<'a> CompilingMacro<'a> {
    fn new(ctx: &'a mut TulispContext, key: usize) -> Result<Self, Error> {
        let mut frame = ctx.enter_frame()?;
        frame.compiling_macros.push(key);
        Ok(CompilingMacro(frame))
    }
}

impl std::ops::Deref for CompilingMacro<'_> {
    type Target = TulispContext;

    fn deref(&self) -> &TulispContext {
        &self.0
    }
}

impl std::ops::DerefMut for CompilingMacro<'_> {
    fn deref_mut(&mut self) -> &mut TulispContext {
        &mut self.0
    }
}

impl Drop for CompilingMacro<'_> {
    fn drop(&mut self) {
        self.0.compiling_macros.pop();
    }
}

/// Gives a fully expanded list the span of the macro call it replaces,
/// so error traces point at the call. The span goes on a copy of the
/// list's first cell, so a list the macro shares is not changed.
fn with_call_span(expansion: TulispObject, call: &TulispObject) -> TulispObject {
    if expansion.consp() && expansion.span().is_none() && call.span().is_some() {
        expansion.clone_inner().into_ref(call.span())
    } else {
        expansion
    }
}

/// The `X` of a `` `X ``, `,X`, `,@X` or `'X` that a walker
/// looking for variables goes into, with the backquote depth to walk
/// it at. See [`wrapped_operand`].
pub(crate) struct WrappedOperand {
    pub(crate) value: TulispObject,
    pub(crate) depth: u32,
    wrap: fn(TulispObject) -> TulispValue,
    span: Option<crate::object::Span>,
}

impl WrappedOperand {
    /// Walks `X` with `walk`, and wraps the result as `X` was wrapped.
    pub(crate) fn map(
        self,
        walk: impl FnOnce(TulispObject, u32) -> Result<TulispObject, Error>,
    ) -> Result<TulispObject, Error> {
        let walked = walk(self.value.clone(), self.depth)?;
        Ok(self.rewrap(walked))
    }

    /// Wraps `value` as `X` was wrapped.
    pub(crate) fn rewrap(&self, value: TulispObject) -> TulispObject {
        (self.wrap)(value).into_ref(self.span)
    }
}

/// The operand of `obj` that may hold variables, when `obj` is at
/// backquote depth `depth`, 0 being code. A backquote adds a level
/// and `,` or `,@` takes one away. `'X` is data in code, but inside a
/// backquote it is part of the template, and an unquote in it still
/// runs. `None` when `obj` wraps nothing or wraps data.
///
/// `in_tail` is for the dotted tail of a list. Emacs reads
/// `(a . ,@X)` as `(a \,@ X)`, so inside a backquote that `X` is at
/// the depth of `a`.
pub(crate) fn wrapped_operand(
    obj: &TulispObject,
    depth: u32,
    in_tail: bool,
) -> Option<WrappedOperand> {
    type Wrap = fn(TulispObject) -> TulispValue;
    let inner = obj.inner_ref();
    let (value, depth, wrap): (&TulispObject, u32, Wrap) = match &inner.0 {
        TulispValue::Backquote { value } => {
            (value, depth + 1, |value| TulispValue::Backquote { value })
        }
        TulispValue::Unquote { value } => (value, depth.saturating_sub(1), |value| {
            TulispValue::Unquote { value }
        }),
        TulispValue::Splice { value } if in_tail && depth > 0 => {
            (value, depth, |value| TulispValue::Splice { value })
        }
        TulispValue::Splice { value } => (value, depth.saturating_sub(1), |value| {
            TulispValue::Splice { value }
        }),
        TulispValue::Quote { value } if depth > 0 => {
            (value, depth, |value| TulispValue::Quote { value })
        }
        TulispValue::Quote { .. }
        | TulispValue::Sharpquote { .. }
        | TulispValue::Nil
        | TulispValue::T
        | TulispValue::Symbol { .. }
        | TulispValue::LexicalBinding { .. }
        | TulispValue::Number { .. }
        | TulispValue::String { .. }
        | TulispValue::List { .. }
        | TulispValue::Any(_)
        | TulispValue::SpecialForm
        | TulispValue::Defun { .. }
        | TulispValue::Special { .. }
        | TulispValue::Macro(_)
        | TulispValue::Defmacro { .. }
        | TulispValue::CompiledDefun { .. }
        | TulispValue::Bounce => return None,
    };
    Some(WrappedOperand {
        value: value.clone(),
        depth,
        wrap,
        span: obj.span(),
    })
}

/// How the walkers over code (the rewrite of lexical variables and
/// the search for a closure's free variables) read a list at code
/// level: which of its elements are forms, and which are names.
pub(crate) enum FormShape {
    /// `(quote X)`: nothing in it runs.
    Quote,
    /// `(lambda PARAMS BODY...)`.
    Lambda,
    /// `(defun NAME PARAMS BODY...)` or `(defmacro NAME PARAMS BODY...)`.
    Defun,
    /// `(defvar NAME [VALUE [DOC]])`.
    Defvar,
    /// `(let VARLIST BODY...)` or `(let* VARLIST BODY...)`.
    Let,
    /// `(condition-case VAR BODYFORM HANDLERS...)`.
    ConditionCase,
    /// `(cond CLAUSES...)`: each clause is a list of forms.
    Cond,
    /// `(Bounce FUNCTION ARGS...)`, a tail call `mark_tail_calls`
    /// marked.
    TailCall,
    /// `(FUNCTION ARGS...)`: a call, or a special form whose arguments
    /// are all forms. A symbol FUNCTION names a function, as in Emacs,
    /// and never a variable.
    Call,
}

impl FormShape {
    /// The shape of `form`, a list at code level.
    pub(crate) fn of(form: &TulispObject) -> FormShape {
        let Ok(head) = form.car() else {
            return FormShape::Call;
        };
        if head.is_bounce() {
            return FormShape::TailCall;
        }
        let head = head.inner_ref();
        match head.0.symbol_name() {
            Some("quote") => FormShape::Quote,
            Some("lambda") => FormShape::Lambda,
            Some("defun" | "defmacro") => FormShape::Defun,
            Some("defvar") => FormShape::Defvar,
            Some("let" | "let*") => FormShape::Let,
            Some("condition-case") => FormShape::ConditionCase,
            Some("cond") => FormShape::Cond,
            _ => FormShape::Call,
        }
    }

    /// How many elements at the start of `form`, a list at code
    /// level, are its head or names it binds or defines rather than
    /// forms. The rest of a `Lambda`, `Defun`, `Defvar`, `TailCall` or
    /// `Call` are forms. A `Call`'s head is a name when it is a symbol.
    pub(crate) fn names_at_start(form: &TulispObject) -> usize {
        match FormShape::of(form) {
            FormShape::Defun => 3,
            FormShape::Lambda | FormShape::Defvar | FormShape::TailCall => 2,
            FormShape::Call if form.car().is_ok_and(|head| head.is_symbol_variant()) => 1,
            FormShape::Call => 0,
            FormShape::Quote | FormShape::Let | FormShape::ConditionCase | FormShape::Cond => 1,
        }
    }
}

/// Walk `body` and replace each occurrence of a symbol listed in
/// `mappings` with its mapped replacement (typically a freshly-created
/// `LexicalBinding`). Only substitutes at code positions — literals
/// inside `'x`, `(quote x)`, or backquote-but-not-unquote positions
/// are preserved so data literals (e.g. alist keys) are not corrupted,
/// and the head of a call names a function, so it is kept too.
pub(crate) fn substitute_lexical(
    body: TulispObject,
    mappings: &[(TulispObject, TulispObject)],
) -> Result<TulispObject, Error> {
    substitute_lexical_inner(body, mappings, 0)
}

/// Like [`substitute_lexical`], for `forms`, a list of forms such as
/// the body of a `let` or a function.
pub(crate) fn substitute_lexical_body(
    forms: TulispObject,
    mappings: &[(TulispObject, TulispObject)],
) -> Result<TulispObject, Error> {
    if mappings.is_empty() {
        return Ok(forms);
    }
    walk_tail_substitute(forms, 0, mappings, 0)
}

/// Walk a list, substituting each element. The first `preserve`
/// cells are copied verbatim; everything past that is recursively
/// substituted (including any improper-list tail).
fn walk_tail_substitute(
    body: TulispObject,
    preserve: usize,
    mappings: &[(TulispObject, TulispObject)],
    quote_depth: u32,
) -> Result<TulispObject, Error> {
    let span = body.span();
    let mut builder = crate::cons::ListBuilder::new();
    let mut items = body.base_iter();
    let mut count: usize = 0;
    for car in items.by_ref() {
        let new_car = if count < preserve {
            car
        } else {
            substitute_lexical_inner(car, mappings, quote_depth)?
        };
        builder.push(new_car);
        count += 1;
    }
    // An improper-list tail, or the error of a list that loops back.
    let tail = items.tail()?;
    if !tail.null() {
        let new_tail = if count < preserve {
            tail
        } else {
            match wrapped_operand(&tail, quote_depth, true) {
                Some(operand) => {
                    operand.map(|value, depth| substitute_lexical_inner(value, mappings, depth))?
                }
                None => substitute_lexical_inner(tail, mappings, quote_depth)?,
            }
        };
        builder.append(new_tail)?;
    }
    Ok(builder.build().with_span(span))
}

/// Rewrites `body`, a list at code level, by its [`FormShape`].
///
/// Binding-introducing forms have a parameter / varlist section that
/// *declares* names rather than referencing them. Walking into those
/// positions and substituting turns them into `LexicalBinding`s, which
/// the inner form's compiler (`compile_fn_lambda`,
/// `compile_fn_let_star`, …) then wraps again in a fresh
/// `LexicalBinding` — producing a double wrap. Substitute only at
/// value-expression positions and at the body, leaving binder names
/// alone.
fn substitute_form(
    body: &TulispObject,
    mappings: &[(TulispObject, TulispObject)],
    quote_depth: u32,
) -> Result<TulispObject, Error> {
    match FormShape::of(body) {
        // At code level, `(quote X)` written as a list form is
        // data-only — don't substitute inside X (alist key etc.).
        FormShape::Quote => Ok(body.clone()),
        // (let VARLIST BODY...) | (let* VARLIST BODY...)
        // VARLIST is a list of either bare symbols (uninitialized
        // vars) or `(var init...)` pairs. Skip the var name; descend
        // into the init expressions and the body forms.
        FormShape::Let => {
            let head = body.car()?;
            let rest = body.cdr()?;
            let varlist = rest.car()?;
            let body_forms = rest.cdr()?;

            let new_varlist = if varlist.consp() {
                let varlist_span = varlist.span();
                let mut vl_builder = crate::cons::ListBuilder::new();
                let mut varitems = varlist.base_iter();
                for varitem in varitems.by_ref() {
                    let new_varitem = if varitem.consp() {
                        // (var init...) — preserve var, substitute init.
                        walk_tail_substitute(varitem, 1, mappings, quote_depth)?
                    } else {
                        // bare symbol
                        varitem
                    };
                    vl_builder.push(new_varitem);
                }
                varitems.take_error()?;
                vl_builder.build().with_span(varlist_span)
            } else {
                varlist
            };

            let body_span = body.span();
            let mut builder = crate::cons::ListBuilder::new();
            builder.push(head);
            builder.push(new_varlist);
            // Substitute the body forms.
            let mut forms = body_forms.base_iter();
            for form in forms.by_ref() {
                builder.push(substitute_lexical_inner(form, mappings, quote_depth)?);
            }
            let tail = forms.tail()?;
            if !tail.null() {
                builder.append(substitute_lexical_inner(tail, mappings, quote_depth)?)?;
            }
            Ok(builder.build().with_span(body_span))
        }
        // (condition-case VAR BODYFORM HANDLERS...)
        // Leave VAR and each handler's condition alone; substitute
        // BODYFORM and the handler bodies.
        FormShape::ConditionCase => {
            let span = body.span();
            let mut builder = crate::cons::ListBuilder::new();
            let mut items = body.base_iter();
            for (index, item) in items.by_ref().enumerate() {
                let item = if index < 2 {
                    item
                } else if index == 2 {
                    substitute_lexical_inner(item, mappings, quote_depth)?
                } else if item.consp() {
                    walk_tail_substitute(item, 1, mappings, quote_depth)?
                } else {
                    item
                };
                builder.push(item);
            }
            let tail = items.tail()?;
            if !tail.null() {
                builder.append(tail)?;
            }
            Ok(builder.build().with_span(span))
        }
        // (cond CLAUSES...): every element of a clause is a form, its
        // first one too.
        FormShape::Cond => {
            let span = body.span();
            let mut builder = crate::cons::ListBuilder::new();
            let mut items = body.base_iter();
            if let Some(head) = items.next() {
                builder.push(head);
            }
            for clause in items.by_ref() {
                builder.push(if clause.consp() {
                    walk_tail_substitute(clause, 0, mappings, quote_depth)?
                } else {
                    clause
                });
            }
            let tail = items.tail()?;
            if !tail.null() {
                builder.append(tail)?;
            }
            Ok(builder.build().with_span(span))
        }
        // Preserve the head and the names; substitute the rest.
        FormShape::Lambda
        | FormShape::Defun
        | FormShape::Defvar
        | FormShape::TailCall
        | FormShape::Call => {
            let preserve = FormShape::names_at_start(body);
            walk_tail_substitute(body.clone(), preserve, mappings, quote_depth)
        }
    }
}

fn substitute_lexical_inner(
    body: TulispObject,
    mappings: &[(TulispObject, TulispObject)],
    quote_depth: u32,
) -> Result<TulispObject, Error> {
    if mappings.is_empty() {
        return Ok(body);
    }
    let inner_ref = body.inner_ref();
    let span = body.span();
    let res = match &inner_ref.0 {
        TulispValue::Symbol { .. } | TulispValue::LexicalBinding { .. } => {
            drop(inner_ref);
            if quote_depth > 0 {
                body
            } else {
                // Iterate in reverse so the most recently-pushed
                // mapping wins. `let*` and similar incremental binders
                // append to `mappings`, so successive shadows of the
                // same name need last-write-wins lookup; defun /
                // lambda mappings have no duplicates so the order
                // doesn't matter for them.
                for (from, to) in mappings.iter().rev() {
                    if body.eq(from) {
                        return Ok(to.clone().with_span(span));
                    }
                }
                body
            }
        }
        TulispValue::List { .. } => {
            drop(inner_ref);
            if quote_depth > 0 {
                // Inside a backquote a list is a template: only what
                // an unquote in it runs is code.
                walk_tail_substitute(body, 0, mappings, quote_depth)?
            } else {
                substitute_form(&body, mappings, quote_depth)?
            }
        }
        // Outside a backquote, `'x` is a literal symbol (e.g. an alist
        // key), not a variable use, and rewriting it to a
        // `LexicalBinding` would corrupt the data. `wrapped_operand`
        // says what to walk.
        _ => {
            drop(inner_ref);
            match wrapped_operand(&body, quote_depth, false) {
                Some(operand) => {
                    operand.map(|value, depth| substitute_lexical_inner(value, mappings, depth))?
                }
                None => body,
            }
        }
    };
    Ok(res)
}

#[cfg(test)]
mod tests {
    use crate::test_utils::{
        eval_assert, eval_assert_equal, eval_assert_error, eval_assert_error_line, eval_assert_not,
    };
    use crate::{Error, TulispContext, TulispObject, TulispValue, list};

    // A call to a Rust function is checked when it compiles, in tail
    // position too.
    #[test]
    fn a_tail_call_to_a_rust_defun_checks_arity() {
        let ctx = &mut TulispContext::new();
        ctx.defun("helper", |a: i64, b: i64| a + b);
        let err = ctx
            .eval_string("(defun caller (x) (helper x))")
            .unwrap_err();
        assert!(err.to_string().contains("Too few arguments"), "{err}");
        ctx.defun("helper", |a: i64| a);
        let err = ctx
            .eval_string("(defun caller2 (x y) (helper x y))")
            .unwrap_err();
        assert!(err.to_string().contains("Too many arguments"), "{err}");
    }

    // A wrong count for a Lisp defun, a lambda and a macro is an error.
    #[test]
    fn a_wrong_count_is_an_error() {
        use std::sync::Arc;
        use std::sync::atomic::{AtomicI64, Ordering};
        let bumps = Arc::new(AtomicI64::new(0));
        let counter = bumps.clone();
        let ctx = &mut TulispContext::new();
        ctx.defun("bump", move |x: i64| {
            counter.fetch_add(1, Ordering::Relaxed);
            x
        });
        ctx.eval_string("(defun one (x) x) (defmacro mac (x) x)")
            .unwrap();
        for program in [
            "(one (bump 1) (bump 2))",
            "(one)",
            "((lambda (x y) x) (bump 1))",
            "(mac (bump 1) (bump 2))",
        ] {
            assert!(ctx.eval_string(program).is_err(), "{program}");
        }
        bumps.store(0, Ordering::Relaxed);
        assert_eq!(ctx.eval_string("(one (bump 5))").unwrap().to_string(), "5");
        assert_eq!(bumps.load(Ordering::Relaxed), 1);
    }

    #[test]
    fn an_error_in_a_macro_expansion_is_traced_to_the_call() {
        let ctx = &mut TulispContext::new();
        eval_assert_error(
            ctx,
            "(when t\n  (+ 1 \"a\"))",
            r#"ERR TypeMismatch: Expected number, got: "a"
<eval_string>:2.3-2.11:  at (+ 1 "a")
<eval_string>:1.1-2.12:  at (if t (progn (+ 1 "a")))
"#,
        );
    }

    // A form with a dotted tail can only come from a macro. Emacs
    // rejects it; nothing may silently drop the tail.
    #[test]
    fn improper_forms_are_rejected() {
        let ctx = &mut TulispContext::new();
        for (name, form) in [
            ("call", "'(list 1 . 2)"),
            ("defun-call", "'(+ 1 . 2)"),
            ("progn", "'(progn 1 . 2)"),
            ("lambda", "'(funcall (lambda (a . b) a) 1)"),
            // The tail sits right where the last parameter ends.
            ("exact-arity call", "'(f 1 . 2)"),
            ("exact-arity lambda call", "'((lambda (a) a) 1 . 2)"),
            ("no-argument call", "'(g . 2)"),
        ] {
            let program = format!("(defun f (a) a) (defun g () 42) (defmacro m () {form}) (m)");
            assert!(ctx.eval_string(&program).is_err(), "{name}: {program}");
        }
    }
    // `macroexpand` on a deeply nested structure raises a catchable
    // error instead of overflowing the stack. The structure is built
    // at runtime (a chain of `(when t …)`) to get past the parser's own
    // cap; run on an 8 MiB thread since the cap (256 in debug) exceeds
    // the harness's ~2 MiB ceiling. 1000 deep trips the cap but stays
    // shallow enough to drop without overflowing.
    #[test]
    fn macroexpand_deep_structure_errors_without_overflowing() {
        std::thread::Builder::new()
            .stack_size(8 * 1024 * 1024)
            .spawn(|| {
                let mut ctx = TulispContext::new();
                let prog = "(let ((x 1)) \
                            (dotimes (i 1000) (setq x (list 'when t x))) \
                            (condition-case nil (progn (macroexpand x) nil) (error 'caught)))";
                let r = ctx.eval_string(prog).expect("should error, not overflow");
                assert_eq!(r.to_string(), "caught");
            })
            .unwrap()
            .join()
            .unwrap();
    }

    #[test]
    fn backquote_fills_in_unquotes_and_splices() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(let ((vv '(12 20 30))) `(,(car vv) ,@(cdr vv) ,(cdr vv)))",
            "'(12 20 30 (20 30))",
        );

        eval_assert_equal(
            ctx,
            r#"
        (let ((a 10))
          (eq 'a (cdr `(a . a))))
        "#,
            r#"t"#,
        );

        eval_assert_equal(
            ctx,
            r#"
        (let ((a 10))
          (cdr `(a . ,a)))
        "#,
            r#"10"#,
        );

        eval_assert_equal(
            ctx,
            r#"`(1 2 '(+ 10 20)  ',(+ 10 20)  (quote ,(+ 20 20)))"#,
            r#"'(1 2 '(+ 10 20) '30 (quote 40))"#,
        );

        eval_assert_error(
            ctx,
            r#"`(1 2 ,,(+ 10 20))"#,
            r#"ERR SyntaxError: Unquote without backquote
<eval_string>:1.7-1.7:  at ,,(+ 10 20)
<eval_string>:1.1-1.1:  at `(1 2 ,,(+ 10 20))
"#,
        );

        // Nested backquote: `,,x` (depth 2 → 1 → 0) evaluates `x` at the
        // outer backquote level; `,y` at depth 2 reduces to depth 1 and
        // is preserved for the inner backquote to resolve later.
        // Matches Emacs (verified with `emacs --batch`).
        eval_assert_equal(
            ctx,
            r#"
        (let ((x 1) (y 2))
          `(a `(b ,,x ,y) c))
        "#,
            r#"'(a `(b ,1 ,y) c)"#,
        );

        // Single-comma at depth 2 stays as data (no eval) — both `,x`
        // and `,y` reduce to depth 1, preserved for the inner backquote.
        eval_assert_equal(
            ctx,
            r#"
        (let ((x 1) (y 2))
          `(a `(b ,x ,y) c))
        "#,
            r#"'(a `(b ,x ,y) c)"#,
        );

        // Dotted-tail double-comma resolves at the outer level.
        eval_assert_equal(
            ctx,
            r#"
        (let ((x 1))
          `(a `(b . ,,x)))
        "#,
            r#"'(a `(b . ,1))"#,
        );

        // `,x` inside `(quote ...)` inside outer backquote: the quote's
        // content is walked, `,x` evaluates at the outer level, and the
        // result wraps in a Quote. Matches Emacs.
        eval_assert_equal(
            ctx,
            r#"
        (let ((x 1))
          `(a (quote (,x)) c))
        "#,
            r#"'(a (quote (1)) c)"#,
        );

        // Outer `'` makes everything inside data: nothing evaluates, no
        // matter how deeply nested the backquote / unquote forms are.
        // `(let ((x 5)) '...)` shows the let-bound `x` is *not* picked
        // up by `,,x` inside the quote.
        eval_assert_equal(
            ctx,
            r#"
        (let ((x 5))
          '(`(,,x)))
        "#,
            r#"'(`(,,x))"#,
        );

        // Nested-backquote double-comma evaluating a `CompiledDefun`
        // (anonymous lambdas compile to bytecode): native compilation
        // emits `Funcall` for `(funcall f)`, no re-entry into `ctx.vm`.
        eval_assert_equal(
            ctx,
            r#"
        (setq f (lambda () 42))
        ``(,,(funcall f))
        "#,
            r#"'`(,42)"#,
        );

        // Same case through a runtime `(eval ...)`. The form is quoted
        // so the outer compile does not see the inner backquotes;
        // `eval` compiles and runs it while the outer program runs.
        eval_assert_equal(
            ctx,
            r#"
        (setq f (lambda () 42))
        (eval '``(,,(funcall f)))
        "#,
            r#"'`(,42)"#,
        );

        // The same through a named function.
        eval_assert_equal(
            ctx,
            r#"
        (defun f () 42)
        (eval '``(,,(f)))
        "#,
            r#"'`(,42)"#,
        );
    }

    #[test]
    fn backquote_splices_into_a_nested_unquote() {
        let ctx = &mut TulispContext::new();
        // Emacs reads `,,@x` as `(\, (\,@ x))`, so the value of `x`
        // becomes the arguments of the inner `,`.
        eval_assert_equal(ctx, "(let ((x (list 'y))) `(a `(b ,,@x)))", "'(a `(b ,y))");
        eval_assert_equal(
            ctx,
            "(let ((x (list 'y))) `(a `(b ,@,@x)))",
            "'(a `(b ,@y))",
        );
        eval_assert_equal(
            ctx,
            "(let ((x (list 'y))) `(a `(b . ,,@x)))",
            "'(a `(b . ,y))",
        );
        eval_assert_equal(
            ctx,
            "(let ((x (list 1))) `(a `(c `(d ,,,@x))))",
            "'(a `(c `(d ,,1)))",
        );
        eval_assert_equal(ctx, "(let ((x (list 1))) `(a ',@x))", "'(a '1)");
        // From no elements Emacs builds `(\,)`, which gives nil, and
        // `(\,@)`, which splices nothing: `,nil` and `,@nil` here.
        eval_assert_equal(ctx, "`(a `(b ,,@nil))", "'(a `(b ,nil))");
        eval_assert_equal(ctx, "(eval (cadr `(a `(b ,,@nil))))", "'(b nil)");
        eval_assert_equal(ctx, "`(a `(b ,@,@nil))", "'(a `(b ,@nil))");
        eval_assert_equal(ctx, "(eval (cadr `(a `(b ,@,@nil))))", "'(b)");
        eval_assert_equal(ctx, "(eval (cadr `(a `(b . ,,@nil))))", "'(b)");
        // From more elements Emacs builds `(\, 1 2)`, which its inner
        // backquote rejects with "Multiple args to , are not
        // supported". An unquote here holds one value, so the outer
        // backquote rejects it. A quote of no elements, `(quote)`, is
        // an error to evaluate in Emacs, and is rejected here too.
        eval_assert_error_line(
            ctx,
            "(let ((x (list 1 2))) `(a `(b ,,@x)))",
            "ERR TypeMismatch: Expected a list of at most one element, got: (1 2)",
        );
        eval_assert_error_line(
            ctx,
            "`(a `(b ,@,@(list 1 2)))",
            "ERR TypeMismatch: Expected a list of at most one element, got: (1 2)",
        );
        eval_assert_error_line(
            ctx,
            "`(a ',@nil)",
            "ERR TypeMismatch: Expected a list of one element, got: nil",
        );
    }

    #[test]
    fn backquote_keeps_a_dotted_tail() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(let ((x 3)) `(a b . ,x))", "'(a b . 3)");
        eval_assert_equal(ctx, "`(a b . c)", "'(a b . c)");
        eval_assert_equal(ctx, "(list 1 `(a b . c) 2)", "'(1 (a b . c) 2)");
        eval_assert_equal(
            ctx,
            "(let ((x (list 1 2)) (y 3)) `(,@x . ,y))",
            "'(1 2 . 3)",
        );
        eval_assert_equal(
            ctx,
            "(let ((x (list 1 2))) `(a ,@x b . c))",
            "'(a 1 2 b . c)",
        );
        eval_assert_equal(ctx, "`(p ,@(list 7 8) q . r)", "'(p 7 8 q . r)");
        eval_assert_equal(ctx, "`(,@(list) . c)", "'c");
        // A `,@` in the dotted tail splices nothing and stays as data.
        // Emacs reads `(a . ,@x)` as `(a \,@ x)`, and keeps the `\,@`
        // symbol as data too.
        eval_assert_equal(
            ctx,
            "(let ((x (list 1 2))) (format \"%S\" `(a . ,@x)))",
            "\"(a . ,@x)\"",
        );
        // What follows it is walked at the same depth as `a`: Emacs
        // gives `(a \,@ 1)` for the first, where the tail here is one
        // splice value, `,@1`, that prints as `(a . ,@1)`.
        eval_assert_equal(ctx, "(let ((x 1)) `(a . ,@,x))", "'(a . ,@1)");
        // The splice of an empty list keeps its `,@`, as Emacs's
        // `(\,@)` does.
        eval_assert_equal(ctx, "`(a . ,@,@nil)", "'(a . ,@nil)");
        eval_assert_equal(ctx, "(let ((x 1)) `(a . ,@(b ,x)))", "'(a . ,@(b 1))");
        // The inner `,b` belongs to the inner backquote, not the `let`.
        eval_assert_equal(
            ctx,
            "(setq b 7) (let ((r (let ((b 1)) `(a `(c . ,@(d ,b)))))) (eval (cadr r)))",
            "'(c . ,@(d 7))",
        );
        eval_assert_equal(ctx, "(let ((x 1)) `(a `(b . ,@,x)))", "'(a `(b . ,@,x))");
        eval_assert_equal(ctx, "(let ((x 1)) `(a `(b . ,@,,x)))", "'(a `(b . ,@,1))");
        eval_assert_equal(ctx, "(let ((x (list 'y))) `(a . ,@,@x))", "'(a . ,@y)");
        // A quote or a backquote in the dotted tail is walked like the
        // rest of the template. Emacs reads `(a . '(b ,x))` as
        // `(a quote (b ,x))`; here the tail stays one quote value.
        eval_assert_equal(ctx, "(let ((x 1)) `(a . '(b ,x)))", "'(a . '(b 1))");
        eval_assert_equal(ctx, "(let ((x 1)) `(a . ',x))", "'(a . '1)");
        eval_assert_equal(ctx, "(let ((x 1)) `(a . `(b ,,x)))", "'(a . `(b ,1))");
        eval_assert_equal(
            ctx,
            "(let ((x 1)) `(a `(b . `(c ,,,x ,,x))))",
            "'(a `(b . `(c ,,1 ,,x)))",
        );
        // A dotted unquote shares the value as the tail, as in Emacs,
        // so it may loop.
        eval_assert(
            ctx,
            "(let ((l (list 1 2 3))) (setcdr (cddr l) l) (eq (cdr `(a . ,l)) l))",
        );
    }

    #[test]
    fn backquote_shares_the_last_splice() {
        let ctx = &mut TulispContext::new();
        // Like the last argument of `append`, it is shared and may be
        // dotted, as in Emacs.
        eval_assert_equal(ctx, "(let ((l (cons 1 2))) `(a ,@l))", "'(a 1 . 2)");
        eval_assert_equal(ctx, "(let ((l (cons 1 2))) `(,@l))", "'(1 . 2)");
        eval_assert_equal(ctx, "(let ((x 5)) `(a ,@x))", "'(a . 5)");
        eval_assert(ctx, "(let ((l (list 1 2))) (eq (cdr `(a ,@l)) l))");
        eval_assert(ctx, "(let ((l (list 1 2))) (eq `(,@l) l))");
        // Every other splice is copied.
        eval_assert_not(ctx, "(let ((l (list 1 2))) (eq (cdr `(a ,@l b)) l))");
        eval_assert_equal(ctx, "(let ((l nil)) `(a ,@l b ,@l))", "'(a b)");
        // Only its cells are copied: its elements are shared, as with
        // `append`, and the copy is made after every element of the
        // template is evaluated.
        eval_assert(
            ctx,
            "(let ((x (list (list 1)))) (eq (car x) (car `(,@x b))))",
        );
        eval_assert_equal(
            ctx,
            "(let ((l (list 1 2))) `(,@l ,(setcdr l nil)))",
            "'(1 nil)",
        );
    }

    #[test]
    fn backquote_evaluates_every_element_before_joining() {
        let ctx = &mut TulispContext::new();
        // Each splice but the last is copied only after the whole
        // template has run, as by `append`.
        eval_assert_equal(
            ctx,
            "(let ((l (list 1 2)) (m (list 3))) `(,@l ,@m ,(setcdr l nil)))",
            "'(1 3 nil)",
        );
        eval_assert_equal(
            ctx,
            "(let ((l (list 1 2))) `(,@l b ,@l ,(setcdr l nil)))",
            "'(1 b 1 nil)",
        );
        eval_assert_equal(
            ctx,
            "(let ((l (list 1 2))) `(,@l b . ,(setcdr l nil)))",
            "'(1 b)",
        );
        // A bad splice is found only after every element has run.
        eval_assert_error_line(
            ctx,
            "(let ((x 5) (y nil)) `(a ,@x ,@y ,(error \"boom\")))",
            "ERR LispError: boom",
        );
    }

    #[test]
    fn backquote_runs_its_unquotes_when_unused() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(let ((y 0)) `(,(setq y 1)) y)", "1");
        eval_assert_equal(ctx, "(let ((y 0)) `(a ,@(setq y (list 2))) y)", "'(2)");
        eval_assert_equal(ctx, "(let ((y 0)) `(a . ,(setq y 3)) y)", "3");
        eval_assert_equal(ctx, "(defun f (y) `(,(setq y 4)) y) (f 0)", "4");
    }

    #[test]
    fn backquote_of_a_lone_splice_is_its_value() {
        let ctx = &mut TulispContext::new();
        eval_assert(ctx, "(let ((x (list 1 2))) (eq `,@x x))");
        eval_assert_equal(ctx, "`,@5", "5");
        eval_assert_equal(ctx, "(let ((x 1)) `(a `,@,x))", "'(a `,@1)");
        // An error in its operand traces the whole `,@X`.
        eval_assert_error(
            ctx,
            "`,@(let 5)",
            "ERR TypeMismatch: Expected list, got: 5\n\
             <eval_string>:1.4-1.10:  at (let 5)\n\
             <eval_string>:1.2-1.3:  at ,@(let 5)\n\
             <eval_string>:1.1-1.1:  at `,@(let 5)\n",
        );
    }

    #[test]
    fn backquote_splices_any_expression() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "`(,@nil)", "nil");
        eval_assert_equal(ctx, "`(a ,@nil b)", "'(a b)");
        eval_assert_equal(ctx, "`(a ,@'(1 2) b)", "'(a 1 2 b)");
        eval_assert_equal(ctx, "`(a ,@'(1 2))", "'(a 1 2)");
        eval_assert_equal(ctx, "`(a ,@5)", "'(a . 5)");
        eval_assert_equal(ctx, "`((,@nil))", "'(nil)");
        eval_assert_equal(
            ctx,
            "(defun f () (list 1 2)) `(a ,@(f) b ,@(f))",
            "'(a 1 2 b 1 2)",
        );
        eval_assert(
            ctx,
            "(setq g (list 1 2)) (defun h () g) (eq (cdr `(a ,@(h))) g)",
        );
        eval_assert_error_line(ctx, "`(a ,@5 b)", "ERR TypeMismatch: Expected list, got: 5");
    }

    #[test]
    fn backquote_rejects_splicing_a_circular_list() {
        let ctx = &mut TulispContext::new();
        eval_assert_error_line(
            ctx,
            "(let ((l (list 1 2 3))) (setcdr (cddr l) l) `(a ,@l b))",
            "ERR OutOfRange: Circular list",
        );
    }

    #[test]
    fn backquote_rejects_splicing_a_dotted_list_before_more_elements() {
        let ctx = &mut TulispContext::new();
        eval_assert_error_line(
            ctx,
            "(let ((l (cons 1 2))) `(,@l 3))",
            "ERR TypeMismatch: Expected list, got: 2",
        );
        eval_assert_error_line(
            ctx,
            "(let ((l (cons 1 2))) `(a ,@l . b))",
            "ERR TypeMismatch: Expected list, got: 2",
        );
        eval_assert_error_line(
            ctx,
            "(let ((x 5)) `(a ,@x b))",
            "ERR TypeMismatch: Expected list, got: 5",
        );
        eval_assert_error_line(
            ctx,
            "(let ((x 5)) `(,@x b))",
            "ERR TypeMismatch: Expected list, got: 5",
        );
    }

    #[test]
    fn backquote_rejects_a_circular_template() {
        let ctx = &mut TulispContext::new();
        // Only a Rust macro can hand over a template that loops.
        ctx.defmacro("circular-template", |_, _| {
            let items = list!(1.into(), 2.into(), 3.into())?;
            items.cddr()?.set_cdr(items.clone())?;
            Ok(TulispValue::Backquote { value: items }.into_ref(None))
        });
        eval_assert_error(
            ctx,
            "(circular-template)",
            "ERR OutOfRange: Circular list\n",
        );
    }

    #[test]
    fn backquote_in_a_lexical_scope() {
        let ctx = &mut TulispContext::new();
        // Quote inside backquote: 'a stays literal, ,b is substituted.
        eval_assert_equal(ctx, "(let ((b 42)) `('a ,b))", "'('a 42)");
        // An unquote under a quote runs too, as in Emacs.
        eval_assert_equal(ctx, "(let ((x 1)) `(a ',x 'x))", "'(a '1 'x)");
        eval_assert_equal(ctx, "(let ((x 1)) `(a '(b ,x)))", "'(a '(b 1))");
        eval_assert_equal(ctx, "(defun f (x) `(a ',x)) (f 2)", "'(a '2)");

        // Regression: literal symbol in a backquote alist key position
        // must NOT be rewritten even when it shares a name with a
        // lambda param. With the bug present, `k` in `(k . literal)`
        // would be substituted to a LexicalBinding wrapper, so
        // `(assoc 'k entry)` would miss.
        eval_assert_equal(
            ctx,
            r#"
        (defun make-entry (k v)
          `((key . ,k) (value . ,v) (k . literal-k)))
        (let ((entry (make-entry 'foo 42)))
          (list (cdr (assoc 'key entry))
                (cdr (assoc 'value entry))
                (cdr (assoc 'k entry))))
        "#,
            "'(foo 42 literal-k)",
        );

        // Regression: a backquote whose unquoted data contains literal
        // symbols matching enclosing lambda params must stay usable as
        // a lambda form. The bug turned inner literal keys into
        // LexicalBindings, which then errored when the stored form was
        // re-evaluated by funcall.
        eval_assert_equal(
            ctx,
            r#"
        (defun make-resetter (items)
          `(lambda ()
             (dolist (item (quote ,items))
               (cdr (assoc 'value item)))))
        (let ((form (make-resetter '(((value . 1)) ((value . 2))))))
          (funcall (eval form))
          'ok)
        "#,
            "'ok",
        );
    }

    #[test]
    fn a_closure_keeps_the_variables_of_its_backquote() {
        let ctx = &mut TulispContext::new();
        // A variable used only under a quote in the template is still
        // captured, as in Emacs.
        eval_assert_equal(
            ctx,
            "(let ((f (let ((x 2)) (lambda () `(a ',x))))) (funcall f))",
            "'(a '2)",
        );
        eval_assert_equal(
            ctx,
            "(defun make-quoted (x) (lambda () `(a ',x))) (funcall (make-quoted 3))",
            "'(a '3)",
        );
        eval_assert_equal(
            ctx,
            "(defun make-both (x) (lambda () `(a ,x ',x))) (funcall (make-both 3))",
            "'(a 3 '3)",
        );
        eval_assert_equal(
            ctx,
            "(let (fs)
               (dolist (k '(1 2))
                 (setq fs (cons (lambda (al) `(assq ',k ',al)) fs)))
               (mapcar (lambda (f) (funcall f 9)) fs))",
            "'((assq '2 '9) (assq '1 '9))",
        );
        eval_assert_equal(
            ctx,
            "(let ((f (let ((x 3)) (lambda () `(a . '(b ,x)))))) (funcall f))",
            "'(a . '(b 3))",
        );
        // Emacs gives `(a \,@ (let ((x 1)) 3))`, where the tail here is
        // one splice value.
        eval_assert_equal(
            ctx,
            "(let ((f (let ((x 3)) (lambda () `(a . ,@(let ((x 1)) ,x)))))) (funcall f))",
            "'(a . ,@(let ((x 1)) 3))",
        );
    }

    #[test]
    fn a_closure_keeps_a_quote_outside_a_backquote_as_data() {
        let ctx = &mut TulispContext::new();
        // Outside a backquote, a quote is data, so the `'g` here is the
        // symbol, which names the function, and not the variable.
        ctx.eval_string("(defun g () 'global)").unwrap();
        eval_assert_equal(
            ctx,
            "(let ((h (let ((g 'shadow)) (lambda () (funcall 'g))))) (funcall h))",
            "'global",
        );
        eval_assert_equal(
            ctx,
            "(defun call-g (g) (funcall 'g)) (call-g 'shadow)",
            "'global",
        );
    }

    #[test]
    fn let_rejects_a_circular_body() {
        let ctx = &mut TulispContext::new();
        // `(let ((y 1)) (setq y 2) y (setq y 2) y ...)`
        eval_assert_error_line(
            ctx,
            "(let ((body (list '(setq y 2) 'y)))
               (setcdr (cdr body) body)
               (eval (cons 'let (cons '((y 1)) body))))",
            "ERR OutOfRange: Circular list",
        );
        // `(let ((y 1)) (lambda (z) y y ...))`
        eval_assert_error_line(
            ctx,
            "(let ((body (list 'y)))
               (setcdr body body)
               (eval (list 'let '((y 1)) (cons 'lambda (cons '(z) body)))))",
            "ERR OutOfRange: Circular list",
        );
    }

    // The head of a call names a function, as in Emacs: a lexical
    // variable of the same name does not hide it, in the body that
    // binds it or in a closure over it.
    #[test]
    fn a_call_head_is_not_a_lexical_variable() {
        for (program, expected) in [
            ("(let ((list 3)) (list list))", "'(3)"),
            ("(defun f (list) (list list)) (f 1)", "'(1)"),
            ("(funcall (lambda (list) (list list 2)) 1)", "'(1 2)"),
            ("(let ((list 1)) (funcall (lambda () (list list))))", "'(1)"),
            (
                "(defun f (list) (mapcar (lambda (x) (list x)) list)) (f '(1 2))",
                "'((1) (2))",
            ),
            ("(defun f (if) (if if 1 2)) (f nil)", "2"),
            ("(defun f (setq) (setq setq 2) setq) (f 1)", "2"),
            ("(defun f (while) (while nil) while) (f 1)", "1"),
            ("(defun f (progn) (progn progn)) (f 1)", "1"),
            (
                "(defun f (car) (let ((s 0)) (dolist (x '(1 2)) (setq s (+ s x))) s)) (f 1)",
                "3",
            ),
            ("(defun f (cons) (setq cons (cons 1 cons))) (f nil)", "'(1)"),
            ("(defun f (cond) (cond (cond 1) (t 2))) (f t)", "1"),
            ("(defun g (x) x) (defun f (g) (g g)) (f 1)", "1"),
            ("(defun f (f) (if (> f 0) (f (- f 1)) 0)) (f 3)", "0"),
            // The head of a `cond` clause is a condition, not a call.
            ("(let ((x 5)) (funcall (lambda () (cond (x x)))))", "5"),
            ("(funcall (let ((x 5)) (lambda () (cond (x 1)))))", "1"),
            // A variable name a macro puts at the head of a call still
            // names the function.
            (
                "(progn (defmacro m (x) (list x x)) (let ((list 1)) (m list)))",
                "'(1)",
            ),
        ] {
            eval_assert_equal(&mut TulispContext::new(), program, expected);
        }
        // A function held in a variable is called with `funcall`.
        eval_assert_error_line(
            &mut TulispContext::new(),
            "(let ((g (lambda () 1))) (g))",
            "ERR Uninitialized: Variable definition is void: g",
        );
    }

    // A body is a list of forms: one whose first form is a symbol
    // named like a special form is still read form by form.
    #[test]
    fn a_body_is_read_form_by_form() {
        for (program, expected) in [
            ("(funcall (lambda (let) let) 5)", "5"),
            ("(let ((let 2)) let)", "2"),
            (
                "(let ((x 4)) (funcall (lambda (let) (funcall (lambda () let))) x))",
                "4",
            ),
            (
                "(car (condition-case let (error \"x\") (error let)))",
                "'error",
            ),
        ] {
            eval_assert_equal(&mut TulispContext::new(), program, expected);
        }
    }

    // The name and parameters of a nested `defun`, and the name of a
    // nested `defvar`, are not variables of the scopes around them.
    #[test]
    fn a_nested_definition_names_no_variable() {
        for (program, expected) in [
            ("(let ((f 1)) (defun f () 2)) (f)", "2"),
            ("(let ((x 1)) (defun f (&optional x) x)) (f 5)", "5"),
            ("(let ((z 1)) (defvar z 3)) z", "3"),
            ("(let ((z 1)) (defvar z 3) z)", "1"),
        ] {
            eval_assert_equal(&mut TulispContext::new(), program, expected);
        }
    }

    #[test]
    fn substitute_lexical_skips_binders() {
        let ctx = &mut TulispContext::new();
        // `substitute_lexical` leaves the parameter / varname positions of
        // `lambda` / `let` / `let*` alone (`dolist` and `dotimes` expand to
        // `let`): the inner form's compiler wraps those names in a
        // `LexicalBinding` itself, and a `debug_assert!` in
        // `TulispObject::lexical_binding` panics on a double wrap.
        //
        // Each shape has an outer binder (defun param or let-bound var)
        // whose name is reused as a binder *inside* the body, where the
        // outer substitution must not write into the inner binder's
        // declaration.

        // 1. defun param `x` reused as a let* var.
        eval_assert_equal(
            ctx,
            r#"
        (defun shadow-let (x)
          (let* ((x (+ x 1)))
            x))
        (shadow-let 10)
        "#,
            "11",
        );

        // 2. defun param `&optional sep` shadowed by `(let* ((sep (or sep "")))…)`.
        //    This is the `mapconcat` shape from the prelude.
        eval_assert_equal(
            ctx,
            r#"
        (defun joiner (xs &optional sep)
          (let* ((sep (or sep "-")))
            (mapconcat (lambda (x) (format "%S" x)) xs sep)))
        (list (joiner '(a b c)) (joiner '(a b c) "/"))
        "#,
            r##"'("a-b-c" "a/b/c")"##,
        );

        // 3. defun param `x` reused as a nested lambda's param.
        eval_assert_equal(
            ctx,
            r#"
        (defun shadow-lambda (x)
          (let ((fn (lambda (x) (* x 10))))
            (funcall fn 5)))
        (shadow-lambda 99)
        "#,
            "50",
        );

        // 4. defun param `x` reused as a `dolist` var.
        eval_assert_equal(
            ctx,
            r#"
        (defun shadow-dolist (x)
          (let ((acc 0))
            (dolist (x '(1 2 3))
              (setq acc (+ acc x)))
            acc))
        (shadow-dolist 99)
        "#,
            "6",
        );

        // 5. defun param `i` reused as a `dotimes` var.
        eval_assert_equal(
            ctx,
            r#"
        (defun shadow-dotimes (i)
          (let ((acc 0))
            (dotimes (i 4)
              (setq acc (+ acc i)))
            acc))
        (shadow-dotimes 99)
        "#,
            "6",
        );

        // 6. let* binder `x` referenced inside its own init expression
        //    (the prior x), then shadowed for the body.
        eval_assert_equal(
            ctx,
            r#"
        (let* ((x 1)
               (x (+ x 10))
               (x (* x 2)))
          x)
        "#,
            "22",
        );
    }

    #[test]
    fn defmacro_expands_and_checks_arity() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(defmacro num ()  4) (macroexpand '(num))", "4");
        eval_assert_equal(
            ctx,
            r##"
        (defmacro inc (var)
          "Have a docstring"
          (list 'setq var (list '+ 1 var)))

        (macroexpand '(inc x))
        "##,
            "'(setq x (+ 1 x))",
        );
        eval_assert_equal(
            ctx,
            "(defmacro inc (var)  (list 'setq var (list '+ 1 var))) (let ((x 4)) (inc x))",
            "5",
        );
        eval_assert_error(
            ctx,
            "(defmacro inc (var)  (list 'setq var (list '+ 1 var))) (let ((x 4)) (inc))",
            r#"ERR ArityMismatch: Too few arguments
<eval_string>:1.69-1.73:  at (inc)
"#,
        );
        eval_assert_error(
            ctx,
            "(defmacro inc (var)  (list 'setq var (list '+ 1 var))) (let ((x 4)) (inc 4 5))",
            r#"ERR ArityMismatch: Too many arguments
<eval_string>:1.69-1.77:  at (inc 4 5)
"#,
        );
    }

    #[test]
    fn macroexpand_runs_a_macro_body() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            r#"
        (defmacro make (alist)
          (setq make-args alist)
          t)

        (eval (list 'make `(,(cons 'a 1) ,(cons 'b 2))))

        make-args
        "#,
            r#"'((a . 1) (b . 2))"#,
        );

        eval_assert_equal(
            ctx,
            r#"
        (macroexpand '(make ((a . 1) (b . 2) (c . 3))))

        make-args
        "#,
            r#"'((a . 1) (b . 2) (c . 3))"#,
        );

        eval_assert_equal(
            ctx,
            r#"
        (make ((a . 1) (b . 2) (c . 3) (d . 4)))

        make-args
        "#,
            r#"'((a . 1) (b . 2) (c . 3) (d . 4))"#,
        );
    }

    // A lambda a macro's body makes is a function value, not the list.
    #[test]
    fn a_macro_body_makes_a_function_of_a_lambda() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            r#"(defmacro vm-m () (lambda () 5)) (format "%s" (vm-m))"#,
            r#""CompiledDefun""#,
        );
    }

    // A macro's body may use a macro defined after it.
    #[test]
    fn a_macro_body_may_use_a_macro_defined_later() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(defmacro lz-outer () (list 'quote (lz-inner))) (defmacro lz-inner () 7) (lz-outer)",
            "7",
        );
    }

    // The body compiles once: a macro used in it expands once, however
    // often the outer macro expands.
    #[test]
    fn a_macro_body_compiles_once() -> Result<(), Error> {
        use std::sync::Arc;
        use std::sync::atomic::{AtomicUsize, Ordering};
        let ctx = &mut TulispContext::new();
        ctx.eval_string("(defmacro once-outer () (list 'quote (once-counted)))")?;
        let count = Arc::new(AtomicUsize::new(0));
        let counter = count.clone();
        ctx.defmacro("once-counted", move |_, _| {
            counter.fetch_add(1, Ordering::Relaxed);
            Ok(TulispObject::from(1))
        });
        for _ in 0..3 {
            assert_eq!(ctx.eval_string("(once-outer)")?.to_string(), "1");
        }
        assert_eq!(count.load(Ordering::Relaxed), 1);
        Ok(())
    }

    // A macro used in the body keeps the expansion it had when the body
    // compiled; redefining the outer macro takes the new one.
    #[test]
    fn a_compiled_macro_body_keeps_its_inner_macros() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();
        ctx.eval_string("(defmacro kp-outer () (list 'quote (kp-inner)))")?;
        ctx.eval_string("(defmacro kp-inner () ''a)")?;
        eval_assert_equal(ctx, "(kp-outer)", "'a");
        eval_assert_equal(ctx, "(defmacro kp-inner () ''b) (kp-outer)", "'a");
        eval_assert_equal(
            ctx,
            "(defmacro kp-outer () (list 'quote (kp-inner))) (kp-outer)",
            "'b",
        );
        Ok(())
    }

    // A first expansion that fails leaves the macro usable once its
    // helper is defined.
    #[test]
    fn a_failed_first_expansion_can_succeed_later() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();
        ctx.eval_string("(defmacro ff-a (x) (list 'quote (ff-b x)))")?;
        assert!(ctx.eval_string("(ff-a 5)").is_err());
        ctx.eval_string("(defmacro ff-b (x) x)")?;
        assert_eq!(ctx.eval_string("(ff-a 5)")?.to_string(), "5");
        Ok(())
    }

    #[test]
    fn macro_docstrings_and_empty_bodies() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, r#"(defmacro ds1 () "doc" "real") (ds1)"#, r#""real""#);
        eval_assert_equal(ctx, r#"(defmacro ds2 () "doc") (ds2)"#, "nil");
        eval_assert_equal(ctx, "(defmacro ds3 ()) (ds3)", "nil");
    }

    #[test]
    fn macro_optional_and_rest_parameters() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(defmacro op (a &optional b &rest c) (list 'quote (list a b c)))
             (list (op 1) (op 1 2 3 4))",
            "'((1 nil nil) (1 2 (3 4)))",
        );
    }

    // An error raised or a compile error in the body is traced to the
    // macro call; one in a backquote-built expansion too.
    #[test]
    fn macro_errors_are_traced_to_the_call() {
        let ctx = &mut TulispContext::new();
        for (program, needle) in [
            ("(defmacro er1 () (car 1)) (er1)", "at (er1)"),
            ("(defmacro er2 () (if)) (er2)", "at (er2)"),
            (
                "(defmacro m (x) `(progn ,x)) (m (car 5))",
                "1.33-1.39:  at (car 5)",
            ),
        ] {
            let err = ctx.eval_string(program).unwrap_err().format(ctx);
            assert!(err.contains(needle), "{program}: {err}");
        }
    }

    // A function body is expanded before its variables are resolved
    // and its tail calls marked, the code inside an unquote too.
    #[test]
    fn a_function_body_is_expanded_before_it_compiles() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(defun h (x) (dolist (x '(1 2) x) x)) (h 5)", "5");
        eval_assert_equal(ctx, "(let ((a 1)) (when-let ((a (+ a 1))) a))", "2");
        eval_assert_equal(
            ctx,
            "(defmacro get-x () 'x) (defun gx (x) (get-x)) (gx 4)",
            "4",
        );
        eval_assert_equal(
            ctx,
            "(defun lp (n) (when (> n 0) (lp (- n 1)))) (lp 100000)",
            "nil",
        );
        eval_assert_equal(
            ctx,
            "(defun bx (x) `(v ,(get-x) ,@(when x (list x)))) (bx 4)",
            "'(v 4 4)",
        );
    }

    // Each top-level form expands with the macros defined by the forms
    // before it, one a macro's expansion defined too.
    #[test]
    fn a_macro_a_macro_defines_expands_in_later_forms() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(defmacro def-get-y () '(defmacro get-y () 'y))
             (def-get-y)
             (defun gy (y) (get-y))
             (gy 4)",
            "4",
        );
        eval_assert_equal(
            ctx,
            "(defmacro two-defs () '(progn (defmacro sm2 () 3) (defun sf2 () (sm2))))
             (two-defs)
             (sf2)",
            "3",
        );
    }

    // A top-level `progn` is entered one form at a time and keeps the
    // value `progn` gives.
    #[test]
    fn a_top_level_progn_keeps_its_value() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(progn)", "nil");
        eval_assert_equal(ctx, "", "nil");
        eval_assert_equal(ctx, "(defmacro nothing () '(progn)) 1 (nothing)", "nil");
        eval_assert_equal(ctx, "(progn 1 (progn 2 3))", "3");
        eval_assert_equal(ctx, "(progn 1 (progn 2 (progn)))", "nil");
    }

    // A macro that keeps expanding to a `progn` that uses it again is
    // an error, not a stack overflow.
    #[test]
    fn a_macro_expanding_to_itself_in_a_progn_is_an_error() {
        let ctx = &mut TulispContext::new();
        ctx.set_max_eval_depth(10);
        eval_assert_error_line(
            ctx,
            "(defmacro inf () '(progn 1 (inf))) (inf)",
            "ERR LispError: Lisp nesting exceeds max-nesting-depth (40)",
        );
        // One expansion to `progn`s nested past the limit.
        eval_assert_error_line(
            ctx,
            "(defmacro deep ()
               (let ((x 1)) (dotimes (_ 50) (setq x (list 'progn x))) x))
             (deep)",
            "ERR LispError: Lisp nesting exceeds max-nesting-depth (40)",
        );
    }

    // An error in a form of an entered `progn` points at the `progn`,
    // or at the macro call that produced it.
    #[test]
    fn an_error_in_an_entered_progn_is_traced_to_it() {
        let ctx = &mut TulispContext::new();
        for program in [
            "(defmacro m (a) `(progn (defun zz () 1) (car ,a)))\n(m 5)",
            "(defmacro m () (list 'progn (list 'car 5)))\n(m)",
            "(defmacro dp () (cons 'progn 7))\n(dp)",
            "(defmacro ua () (list 'progn 1 'unbound-zz))\n(ua)",
            "\n(progn 1 unbound-zz)",
        ] {
            let err = ctx.eval_string(program).unwrap_err().format(ctx);
            assert!(err.contains("<eval_string>:2.1-2."), "{program}: {err}");
        }
    }

    // `macroexpand` expands the code an unquote or a splice runs, and
    // leaves the rest of a backquote's template alone.
    #[test]
    fn macroexpand_expands_inside_unquotes() {
        let ctx = &mut TulispContext::new();
        let got = ctx
            .eval_string("(macroexpand '`(when ,@(when x (list y))))")
            .unwrap();
        assert_eq!(got.to_string(), "`(when ,@(if x (progn (list y))))");
    }

    // A macro that returns a list it keeps gives each call's
    // expansion that call's location, and leaves the list alone.
    #[test]
    fn a_shared_expansion_is_traced_to_each_call() {
        let ctx = &mut TulispContext::new();
        ctx.eval_string("(defvar shared (list 'car 5)) (defmacro sm () shared)")
            .unwrap();
        for (program, needle) in [
            ("\n(sm)", "<eval_string>:2.1-2.4"),
            ("\n\n\n(sm)", "<eval_string>:4.1-4.4"),
        ] {
            let err = ctx.eval_string(program).unwrap_err().format(ctx);
            assert!(err.contains(needle), "{program}: {err}");
        }
    }

    // A quoted `(lambda ...)` list is a function.
    #[test]
    fn a_quoted_lambda_list_is_a_function() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(funcall '(lambda (x) (* x 2)) 3)", "6");
        eval_assert_equal(ctx, "(mapcar '(lambda (x) (1+ x)) '(1 2))", "'(2 3)");
        eval_assert_equal(ctx, "(let ((f '(lambda () 5))) (funcall f))", "5");
        eval_assert_equal(ctx, "(funcall '(lambda (x)) 1)", "nil");
        eval_assert_equal(ctx, "(funcall '(lambda (x) \"doc\" (* x 3)) 2)", "6");
        eval_assert_error_line(
            ctx,
            "(let ((y 1)) (funcall '(lambda () y)))",
            "ERR Uninitialized: Variable definition is void: y",
        );
    }

    // A macro defined at run time is there for a later program.
    #[test]
    fn a_macro_defined_through_eval() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(eval '(defmacro em () 3)) t", "t");
        eval_assert_equal(ctx, "(em)", "3");
    }

    // A funcall of a function in a variable is not a call to a name
    // with no value: the body is kept, with the macros it used then.
    #[test]
    fn a_body_calling_a_local_function_is_kept() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();
        ctx.eval_string(
            "(defmacro lf-outer ()
               (let ((g (lambda (y) y))) (funcall g (list 'quote (lf-inner)))))",
        )?;
        ctx.eval_string("(defmacro lf-inner () ''old)")?;
        assert_eq!(ctx.eval_string("(lf-outer)")?.to_string(), "old");
        ctx.eval_string("(defmacro lf-inner () ''new)")?;
        assert_eq!(ctx.eval_string("(lf-outer)")?.to_string(), "old");
        Ok(())
    }

    // A body whose first compile met a call to a name with no value yet
    // is not kept, so it takes that name once it is defined.
    #[test]
    fn a_body_compiled_before_its_helper_exists_is_not_kept() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();
        ctx.eval_string("(defmacro fz (x) (if x (list 'quote (fz-inner x)) ''none))")?;
        assert_eq!(ctx.eval_string("(fz nil)")?.to_string(), "none");
        ctx.eval_string("(defmacro fz-inner (x) (list 'car x))")?;
        assert_eq!(ctx.eval_string("(fz (a b))")?.to_string(), "a");
        Ok(())
    }

    // A macro used in its own body, however deep and even in code that
    // never runs, is refused at its first expansion, which compiles the
    // body. The guard keys on the macro, so a cycle through a second
    // macro is refused too.
    #[test]
    fn a_macro_used_in_its_own_body_is_an_error() {
        std::thread::Builder::new()
            .stack_size(8 * 1024 * 1024)
            .spawn(|| {
                for (program, name) in [
                    ("(defmacro rm1 () (rm1)) (rm1)", "rm1"),
                    (
                        "(defmacro rm2 () (progn (progn (progn (progn (progn (rm2))))))) (rm2)",
                        "rm2",
                    ),
                    (
                        "(defmacro rm3 (x) (let ((f (lambda (y) (rm3 y)))) (list 'quote x))) (rm3 1)",
                        "rm3",
                    ),
                ] {
                    let ctx = &mut TulispContext::new();
                    eval_assert_error_line(
                        ctx,
                        program,
                        &format!("ERR LispError: macro {name} is used in its own body"),
                    );
                }
            })
            .unwrap()
            .join()
            .unwrap();
    }

    // A macro that keeps expanding itself while its body runs ends
    // with the depth limit.
    #[test]
    fn a_macro_expanding_itself_at_run_time_hits_the_depth_limit() {
        std::thread::Builder::new()
            .stack_size(8 * 1024 * 1024)
            .spawn(|| {
                let ctx = &mut TulispContext::new();
                let program = "(defmacro rr () (macroexpand '(rr))) (rr)";
                let err = ctx.eval_string(program).unwrap_err().format(ctx);
                assert!(err.contains("max-eval-depth"), "{err}");
            })
            .unwrap()
            .join()
            .unwrap();
    }

    // A panic while a body compiles does not leave its macro marked as
    // compiling.
    #[test]
    fn a_panic_while_a_body_compiles_is_recovered() -> Result<(), Error> {
        use std::sync::Arc;
        use std::sync::atomic::{AtomicBool, Ordering};
        let ctx = &mut TulispContext::new();
        ctx.eval_string("(defmacro pc-outer () (list 'quote (pc-host)))")?;
        let first = Arc::new(AtomicBool::new(true));
        let flag = first.clone();
        ctx.defmacro("pc-host", move |_, _| {
            if flag.swap(false, Ordering::Relaxed) {
                panic!("host macro panic");
            }
            Ok(TulispObject::from(1))
        });
        let caught = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            ctx.eval_string("(pc-outer)")
        }));
        assert!(caught.is_err());
        assert_eq!(ctx.eval_string("(pc-outer)")?.to_string(), "1");
        Ok(())
    }
}
