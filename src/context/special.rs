use std::sync::atomic::{AtomicBool, Ordering};

use crate::bytecode::Block;
use crate::object::wrappers::generic::Shared;
use crate::{
    Error, Param, ParamKind, PositionalParam, Rest, TulispContext, TulispConvertible, TulispObject,
};

/// An argument of a special form, passed unevaluated. See
/// [`defspecial`](TulispContext::defspecial).
///
/// A `Form` reads the lexical variables of the code around the call,
/// so it is valid only while the call runs. Evaluating it after the
/// special form returned is an error.
///
/// Only a special form takes a `Form`; a function cannot:
///
/// ```compile_fail
/// let mut ctx = tulisp::TulispContext::new();
/// ctx.defun("f", |form: tulisp::Form| form.source().clone());
/// ```
#[derive(Clone)]
pub struct Form {
    source: TulispObject,
    code: FormCode,
    live: Shared<AtomicBool>,
}

#[allow(dead_code)]
#[derive(Clone)]
enum FormCode {
    /// Compiled by the VM.
    Compiled(Block),
}

impl Form {
    /// Evaluates the form and returns its value. It may be called any
    /// number of times during the call.
    pub fn eval(&self, ctx: &mut TulispContext) -> Result<TulispObject, Error> {
        if !self.live.load(Ordering::Relaxed) {
            return Err(
                Error::lisp_error("a form ran after its special form returned")
                    .with_trace(self.source.clone()),
            );
        }
        match &self.code {
            FormCode::Compiled(block) => crate::bytecode::run_block(ctx, block, None),
        }
    }

    /// Evaluates the form and converts the value, as a
    /// [`defun`](TulispContext::defun) parameter of type `T` would be.
    pub fn eval_into<T: TulispConvertible>(&self, ctx: &mut TulispContext) -> Result<T, Error> {
        let value = self.eval(ctx)?;
        T::from_tulisp(ctx, &value)
    }

    /// The argument as written, with the call's lexical variables in
    /// it: [`ctx.eval`](TulispContext::eval) of it sees the same
    /// variables while the call runs.
    pub fn source(&self) -> &TulispObject {
        &self.source
    }
}

/// The forms of one special-form call. Dropping it, when the call
/// returns or unwinds, makes them invalid.
#[allow(dead_code)]
pub(crate) struct CallForms {
    live: Shared<AtomicBool>,
}

#[allow(dead_code)]
impl CallForms {
    pub(crate) fn new() -> Self {
        CallForms {
            live: Shared::new(AtomicBool::new(true)),
        }
    }

    pub(crate) fn compiled(&self, block: Block, source: TulispObject) -> Form {
        Form {
            source,
            code: FormCode::Compiled(block),
            live: self.live.clone(),
        }
    }
}

impl Drop for CallForms {
    fn drop(&mut self) {
        self.live.store(false, Ordering::Relaxed);
    }
}

/// Whether the argument at INDEX of a call to a special form whose
/// parameters are KINDS is passed unevaluated. An index past the
/// parameters belongs to the last one, a rest parameter.
#[allow(dead_code)]
pub(crate) fn takes_form(kinds: &[ParamKind], index: usize) -> bool {
    matches!(
        kinds.get(index).or(kinds.last()),
        Some(ParamKind::Form { .. } | ParamKind::RestForm)
    )
}

/// A special form's arguments: the evaluated ones and the forms, each
/// in order. Only [`SpecialParam`] implementations read it.
pub struct SpecialArgs<'a> {
    pub(crate) values: &'a [TulispObject],
    pub(crate) forms: std::vec::IntoIter<Form>,
}

/// A parameter of a special form registered with
/// [`defspecial`](TulispContext::defspecial): any [`Param`], whose
/// argument is evaluated before the call, or a [`Form`],
/// `Option<Form>` or [`Rest<Form>`], whose argument is not.
#[diagnostic::on_unimplemented(
    message = "`{Self}` is not a `defspecial` parameter",
    note = "a parameter is `TulispConvertible`, `Rest<T>`, `Plist<T>`, `Form`, `Option<Form>` or `Rest<Form>`"
)]
pub trait SpecialParam: Sized + 'static {
    const KIND: ParamKind;

    /// Takes this parameter's value from the front of ARGS. Arity has
    /// been checked, so a required position is present.
    fn take(ctx: &mut TulispContext, args: &mut SpecialArgs<'_>) -> Result<Self, Error>;
}

impl<T: Param> SpecialParam for T {
    // A `Param` reads an evaluated argument, so it cannot claim an
    // unevaluated one.
    const KIND: ParamKind = match T::KIND {
        kind @ (ParamKind::Positional { .. } | ParamKind::Rest | ParamKind::Plist) => kind,
        ParamKind::Form { .. } | ParamKind::RestForm => {
            panic!("a `Param` cannot take its argument unevaluated; use `Form`")
        }
    };

    fn take(ctx: &mut TulispContext, args: &mut SpecialArgs<'_>) -> Result<Self, Error> {
        T::take(ctx, &mut args.values)
    }
}

impl SpecialParam for Form {
    const KIND: ParamKind = ParamKind::Form { required: true };

    fn take(_ctx: &mut TulispContext, args: &mut SpecialArgs<'_>) -> Result<Self, Error> {
        args.forms.next().ok_or_else(Error::too_few_arguments)
    }
}

impl SpecialParam for Option<Form> {
    const KIND: ParamKind = ParamKind::Form { required: false };

    fn take(_ctx: &mut TulispContext, args: &mut SpecialArgs<'_>) -> Result<Self, Error> {
        Ok(args.forms.next())
    }
}

impl SpecialParam for Rest<Form> {
    const KIND: ParamKind = ParamKind::RestForm;

    fn take(_ctx: &mut TulispContext, args: &mut SpecialArgs<'_>) -> Result<Self, Error> {
        Ok(args.forms.by_ref().collect())
    }
}

/// A [`SpecialParam`] that binds one argument position, so it may come
/// before another parameter.
pub trait SpecialPositionalParam: SpecialParam {}

impl<T: PositionalParam> SpecialPositionalParam for T {}
impl SpecialPositionalParam for Form {}
impl SpecialPositionalParam for Option<Form> {}

#[cfg(test)]
mod tests {
    use super::takes_form;
    use crate::ParamKind;

    #[test]
    fn takes_form_follows_the_parameter_kinds() {
        let kinds = [
            ParamKind::Positional { required: true },
            ParamKind::Form { required: true },
            ParamKind::RestForm,
        ];
        let got: Vec<bool> = (0..5).map(|i| takes_form(&kinds, i)).collect();
        assert_eq!(got, [false, true, true, true, true]);
        let eager_rest = [ParamKind::Form { required: true }, ParamKind::Rest];
        let got: Vec<bool> = (0..3).map(|i| takes_form(&eager_rest, i)).collect();
        assert_eq!(got, [true, false, false]);
    }
}
