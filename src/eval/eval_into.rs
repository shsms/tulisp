use crate::{
    TulispConvertible, TulispObject, TulispValue,
    context::TulispContext,
    error::Error,
    eval::{Eval, eval_form},
    number::Number,
};

/// A trait for evaluating a [`TulispObject`] and converting the result into a
/// specific Rust type.
///
/// This is useful when you want to evaluate an expression and immediately
/// obtain a typed value, without manually calling `eval` followed by a
/// conversion.
///
/// # Example
///
/// ```ignore
/// use tulisp::{TulispContext, Number};
/// use tulisp::eval::EvalInto;
///
/// let ctx = &mut TulispContext::new();
/// let expr = ctx.eval_string("(+ 1 2)")?;
/// let n: Number = expr.eval_into(ctx)?;
/// ```
pub(crate) trait EvalInto<T: TulispConvertible> {
    /// Evaluate `self` in the given context and convert the result into `T`.
    fn eval_into(&self, ctx: &mut TulispContext) -> Result<T, Error>;
}

impl EvalInto<Number> for TulispObject {
    #[inline(always)]
    fn eval_into(&self, ctx: &mut TulispContext) -> Result<Number, Error> {
        let inner = self.inner_ref();
        match &inner.0 {
            TulispValue::Number { value, .. } => Ok(*value),
            TulispValue::Symbol { value: sym, .. } => {
                if sym.is_constant() {
                    return Err(Error::type_mismatch(format!(
                        "Expected number, got: {}",
                        self
                    )));
                }
                sym.get_as_number().map_err(|e| e.with_trace(self.clone()))
            }
            TulispValue::LexicalBinding { value: sym, .. } => {
                sym.get_as_number().map_err(|e| e.with_trace(self.clone()))
            }
            TulispValue::List { .. } => eval_form::<Eval>(ctx, self)
                .map_err(|e| e.with_trace(self.clone()))?
                .as_number(),
            _ => Err(Error::type_mismatch(format!(
                "Expected number, got: {}",
                self
            ))),
        }
    }
}
