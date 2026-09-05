use crate::{TulispConvertible, TulispObject};

/// A variadic tail argument in a [`defun`](crate::TulispContext::defun) function.
///
/// Use `Rest<T>` as the last parameter of a function registered with
/// [`defun`](crate::TulispContext::defun) to accept zero or more trailing arguments,
/// all converted to `T`.  This mirrors Emacs Lisp's `&rest` parameter.
///
/// # Example
///
/// ```rust
/// use tulisp::{TulispContext, Rest};
///
/// let mut ctx = TulispContext::new();
/// ctx.defun("sum", |items: Rest<f64>| -> f64 { items.into_iter().sum() });
/// assert_eq!(ctx.eval_string("(sum 1.0 2.0 3.0)").unwrap().to_string(), "6.0");
/// ```
pub struct Rest<T> {
    values: Vec<T>,
}

impl From<Rest<TulispObject>> for TulispObject {
    fn from(val: Rest<TulispObject>) -> Self {
        val.values.into_iter().collect()
    }
}

impl<T> FromIterator<T> for Rest<T>
where
    T: TulispConvertible + 'static,
{
    // Every `T` collects into a `Vec`, `TulispObject` too: building
    // a list here would cost one cons cell per argument on every
    // call, and most callers only iterate.
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        Rest {
            values: iter.into_iter().collect(),
        }
    }
}

impl<T> IntoIterator for Rest<T>
where
    T: TulispConvertible,
{
    type Item = T;
    type IntoIter = std::vec::IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.values.into_iter()
    }
}
