use crate::{Error, TulispContext, TulispConvertible, TulispObject};

/// The arguments of [`funcall`](TulispContext::funcall): a tuple with one
/// element per argument, or `()` for none.
pub trait FuncallArgs {
    /// The arguments in order, each converted.
    fn into_args(self, ctx: &mut TulispContext) -> Vec<TulispObject>;
}

macro_rules! impl_funcall_args {
    ($($t:ident $v:ident),*) => {
        impl<$($t: TulispConvertible),*> FuncallArgs for ($($t,)*) {
            #[allow(unused_variables)]
            fn into_args(self, ctx: &mut TulispContext) -> Vec<TulispObject> {
                let ($($v,)*) = self;
                vec![$($v.into_tulisp(ctx)),*]
            }
        }
    };
}

impl_funcall_args!();
impl_funcall_args!(A a);
impl_funcall_args!(A a, B b);
impl_funcall_args!(A a, B b, C c);
impl_funcall_args!(A a, B b, C c, D d);
impl_funcall_args!(A a, B b, C c, D d, E e);
impl_funcall_args!(A a, B b, C c, D d, E e, F f);
impl_funcall_args!(A a, B b, C c, D d, E e, F f, G g);
impl_funcall_args!(A a, B b, C c, D d, E e, F f, G g, H h);
impl_funcall_args!(A a, B b, C c, D d, E e, F f, G g, H h, I i);
impl_funcall_args!(A a, B b, C c, D d, E e, F f, G g, H h, I i, J j);
impl_funcall_args!(A a, B b, C c, D d, E e, F f, G g, H h, I i, J j, K k);
impl_funcall_args!(A a, B b, C c, D d, E e, F f, G g, H h, I i, J j, K k, L l);

/// The list whose elements [`apply`](TulispContext::apply) passes as its
/// last arguments: a Lisp list, or a `Vec` of convertible values.
pub trait SpreadArgs {
    /// Adds the elements to `args`, each converted.
    fn spread(self, ctx: &mut TulispContext, args: &mut Vec<TulispObject>) -> Result<(), Error>;
}

impl SpreadArgs for &TulispObject {
    fn spread(self, _ctx: &mut TulispContext, args: &mut Vec<TulispObject>) -> Result<(), Error> {
        args.extend(crate::cons::collect_list(self, Ok)?);
        Ok(())
    }
}

impl SpreadArgs for TulispObject {
    fn spread(self, ctx: &mut TulispContext, args: &mut Vec<TulispObject>) -> Result<(), Error> {
        (&self).spread(ctx, args)
    }
}

impl<T: TulispConvertible> SpreadArgs for Vec<T> {
    fn spread(self, ctx: &mut TulispContext, args: &mut Vec<TulispObject>) -> Result<(), Error> {
        args.extend(self.into_iter().map(|item| item.into_tulisp(ctx)));
        Ok(())
    }
}

/// The arguments of [`apply`](TulispContext::apply): a tuple whose last
/// element is a list to spread, as in `(a, b, rest)`, or a list on its own.
pub trait ApplyArgs {
    /// The arguments in order, each converted.
    fn into_args(self, ctx: &mut TulispContext) -> Result<Vec<TulispObject>, Error>;
}

impl<L: SpreadArgs> ApplyArgs for L {
    fn into_args(self, ctx: &mut TulispContext) -> Result<Vec<TulispObject>, Error> {
        let mut args = Vec::new();
        self.spread(ctx, &mut args)?;
        Ok(args)
    }
}

macro_rules! impl_apply_args {
    ($($t:ident $v:ident),*) => {
        impl<$($t: TulispConvertible,)* L: SpreadArgs> ApplyArgs for ($($t,)* L,) {
            fn into_args(self, ctx: &mut TulispContext) -> Result<Vec<TulispObject>, Error> {
                let ($($v,)* rest,) = self;
                let mut args = vec![$($v.into_tulisp(ctx)),*];
                rest.spread(ctx, &mut args)?;
                Ok(args)
            }
        }
    };
}

impl_apply_args!(A a);
impl_apply_args!(A a, B b);
impl_apply_args!(A a, B b, C c);
impl_apply_args!(A a, B b, C c, D d);
impl_apply_args!(A a, B b, C c, D d, E e);
impl_apply_args!(A a, B b, C c, D d, E e, F f);
impl_apply_args!(A a, B b, C c, D d, E e, F f, G g);
impl_apply_args!(A a, B b, C c, D d, E e, F f, G g, H h);
impl_apply_args!(A a, B b, C c, D d, E e, F f, G g, H h, I i);
impl_apply_args!(A a, B b, C c, D d, E e, F f, G g, H h, I i, J j);
impl_apply_args!(A a, B b, C c, D d, E e, F f, G g, H h, I i, J j, K k);

#[cfg(test)]
mod tests {
    use crate::{TulispContext, TulispObject};

    #[test]
    fn funcall_passes_one_argument_per_tuple_element() {
        let mut ctx = TulispContext::new();
        ctx.eval_string("(defun pair (a b) (list b a))").unwrap();
        let pair = ctx.intern("pair");
        let result = ctx.funcall(&pair, (1i64, "x".to_string())).unwrap();
        assert_eq!(result.to_string(), r#"("x" 1)"#);
        let list = ctx.intern("list");
        assert_eq!(ctx.funcall(&list, ()).unwrap().to_string(), "nil");
        let forms = ctx.eval_string("'(+ 1 2)").unwrap();
        let result = ctx.funcall(&list, (forms, vec![1i64, 2])).unwrap();
        assert_eq!(result.to_string(), "((+ 1 2) (1 2))");
        let err = ctx.funcall(&pair, (1i64,)).unwrap_err();
        assert!(err.to_string().contains("Too few arguments"), "{err}");
    }

    #[test]
    fn apply_spreads_its_last_argument() {
        let mut ctx = TulispContext::new();
        let plus = ctx.intern("+");
        assert_eq!(ctx.apply(&plus, vec![1.5f64]).unwrap().to_string(), "1.5");
        assert_eq!(
            ctx.apply(&plus, TulispObject::nil()).unwrap().to_string(),
            "0"
        );
    }

    #[test]
    fn apply_passes_its_arguments_without_evaluating_them() {
        let mut ctx = TulispContext::new();
        let list = ctx.intern("list");
        let forms = ctx.eval_string("'((+ 1 2) x)").unwrap();
        let result = ctx.apply(&list, ("a".to_string(), &forms)).unwrap();
        assert_eq!(result.to_string(), r#"("a" (+ 1 2) x)"#);
    }

    #[test]
    fn apply_calls_named_functions_and_lambdas() {
        let mut ctx = TulispContext::new();
        ctx.eval_string("(defun pair (a b) (list b a))").unwrap();
        let pair = ctx.intern("pair");
        let result = ctx.apply(&pair, (1i64, vec![2i64])).unwrap();
        assert_eq!(result.to_string(), "(2 1)");
        let lambda = ctx.eval_string("(lambda (a &rest b) (cons a b))").unwrap();
        let result = ctx.apply(&lambda, (1i64, vec![2i64, 3])).unwrap();
        assert_eq!(result.to_string(), "(1 2 3)");
    }

    #[test]
    fn apply_rejects_a_last_argument_that_is_not_a_list() {
        let mut ctx = TulispContext::new();
        let plus = ctx.intern("+");
        for source in ["5", "'(1 . 2)"] {
            let last = ctx.eval_string(source).unwrap();
            assert!(ctx.apply(&plus, (1i64, last)).is_err(), "{source}");
        }
    }
}
