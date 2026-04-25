use crate::{TulispConvertible, TulispObject, cons};

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
/// assert_eq!(ctx.eval_string("(sum 1.0 2.0 3.0)").unwrap().to_string(), "6");
/// ```
pub struct Rest<T> {
    values: RestEnum<T>,
}

enum RestEnum<T> {
    Typed(Vec<T>),
    Boxed(TulispObject, std::marker::PhantomData<T>),
}

impl From<Rest<TulispObject>> for TulispObject {
    fn from(val: Rest<TulispObject>) -> Self {
        match val.values {
            RestEnum::Typed(..) => {
                unreachable!()
            }
            RestEnum::Boxed(obj, _) => obj,
        }
    }
}

impl<T> FromIterator<T> for Rest<T>
where
    T: TulispConvertible + 'static,
{
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        if std::any::TypeId::of::<T>() == std::any::TypeId::of::<TulispObject>() {
            let obj: TulispObject = iter.into_iter().map(|t| t.into_tulisp()).collect();
            Rest {
                values: RestEnum::Boxed(obj, std::marker::PhantomData),
            }
        } else {
            let values: Vec<T> = iter.into_iter().collect();
            Rest {
                values: RestEnum::Typed(values),
            }
        }
    }
}

/// Iterator returned by [`Rest::into_iter`].
pub enum RestEnumIter<T> {
    Typed(std::vec::IntoIter<T>),
    Boxed(cons::BaseIter, std::marker::PhantomData<T>),
}

impl<T> Iterator for RestEnumIter<T>
where
    T: TulispConvertible,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            RestEnumIter::Typed(iter) => iter.next(),
            RestEnumIter::Boxed(base_iter, _) => base_iter
                .next()
                .and_then(|obj| TulispConvertible::from_tulisp(&obj).ok()),
        }
    }
}

impl<T> IntoIterator for Rest<T>
where
    T: TulispConvertible,
{
    type Item = T;
    type IntoIter = RestEnumIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        match self.values {
            RestEnum::Typed(vec) => RestEnumIter::Typed(vec.into_iter()),
            RestEnum::Boxed(obj, _) => {
                let base_iter = obj.base_iter();
                RestEnumIter::Boxed(base_iter, std::marker::PhantomData)
            }
        }
    }
}
