use std::borrow::Cow;

use crate::object::wrappers::generic::SendSyncIfSync;
use crate::symbols::{ParamPosition, Signature, SignatureParam};
use crate::value::DefunArity;
use crate::{
    Error, Plist, Plistable, Rest, SpecialParam, TulispContext, TulispConvertible, TulispObject,
};

/// How a closure parameter takes its value from a call's arguments.
///
/// A hand-written [`Param`] uses `Positional`, `Rest` or `Plist`; the `Form`
/// kinds belong to [`defspecial`](TulispContext::defspecial)'s own parameters.
/// More kinds may come in later versions, so a `match` over it needs a `_` arm:
///
/// ```compile_fail
/// fn name(kind: tulisp::ParamKind) -> &'static str {
///     match kind {
///         tulisp::ParamKind::Positional { .. } => "positional",
///         tulisp::ParamKind::Rest => "rest",
///         tulisp::ParamKind::Plist => "plist",
///         tulisp::ParamKind::Form { .. } => "form",
///         tulisp::ParamKind::RestForm => "rest-form",
///     }
/// }
/// ```
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub enum ParamKind {
    /// One argument at this position; `required` is false for a
    /// parameter that may be absent.
    Positional { required: bool },
    /// Every remaining argument, as a [`Rest<T>`].
    Rest,
    /// Every remaining argument, as keyword/value pairs in a
    /// [`Plist<T>`].
    Plist,
    /// One argument at this position, passed unevaluated as a `Form`;
    /// `required` is false for `Option<Form>`.
    Form { required: bool },
    /// Every remaining argument, unevaluated, as a `Rest<Form>`.
    RestForm,
}

/// A parameter of a function registered with [`defun`](TulispContext::defun),
/// or of a macro registered with [`defmacro`](TulispContext::defmacro).
pub trait Param: Sized + 'static {
    const KIND: ParamKind;

    /// Takes this parameter's value from the front of `args`, leaving
    /// the arguments it did not consume. Arity has been checked by
    /// the caller, so a required position is always present.
    fn take(ctx: &mut TulispContext, args: &mut &[TulispObject]) -> Result<Self, Error>;

    /// The Lisp type an editor shows for this parameter. `None` unless
    /// overridden.
    fn type_name() -> Option<Cow<'static, str>> {
        None
    }

    /// The keys a `Plist` parameter reads, as a plist spells them, each with
    /// its leading `:`. Empty unless overridden: `Plist<T>` gives
    /// [`Plistable::plist_keys`] of `T`.
    fn declared_keys() -> Vec<Cow<'static, str>> {
        Vec::new()
    }
}

impl<T: TulispConvertible + 'static> Param for T {
    const KIND: ParamKind = ParamKind::Positional {
        required: T::REQUIRED,
    };

    fn take(ctx: &mut TulispContext, args: &mut &[TulispObject]) -> Result<Self, Error> {
        match args.split_first() {
            Some((value, rest)) => {
                *args = rest;
                T::from_tulisp(ctx, value)
            }
            None if T::REQUIRED => Err(Error::too_few_arguments()),
            None => T::from_absent(ctx),
        }
    }

    fn type_name() -> Option<Cow<'static, str>> {
        T::lisp_type()
    }
}

impl<T: TulispConvertible + 'static> Param for Rest<T> {
    const KIND: ParamKind = ParamKind::Rest;

    fn take(ctx: &mut TulispContext, args: &mut &[TulispObject]) -> Result<Self, Error> {
        std::mem::take(args)
            .iter()
            .map(|arg| T::from_tulisp(ctx, arg))
            .collect::<Result<Rest<T>, Error>>()
    }

    fn type_name() -> Option<Cow<'static, str>> {
        T::lisp_type()
    }
}

impl<T: Plistable + 'static> Param for Plist<T> {
    const KIND: ParamKind = ParamKind::Plist;

    fn take(ctx: &mut TulispContext, args: &mut &[TulispObject]) -> Result<Self, Error> {
        Plist::new(ctx, std::mem::take(args))
    }

    fn declared_keys() -> Vec<Cow<'static, str>> {
        T::plist_keys()
    }
}

/// A [`Param`] that binds one argument position, so it may come
/// before another parameter; [`Rest<T>`] and [`Plist<T>`] may not. A
/// hand-written [`Param`] needs this impl too to sit anywhere but last.
pub trait PositionalParam: Param {}

impl<T: TulispConvertible + 'static> PositionalParam for T {}

/// The value a [`defun`](TulispContext::defun) or
/// [`defmacro`](TulispContext::defmacro) closure returns.
pub trait Return: 'static {
    /// The Lisp value the call answers with, or the error it raises.
    fn into_result(self, ctx: &mut TulispContext) -> Result<TulispObject, Error>;
}

impl<T: TulispConvertible + 'static> Return for T {
    fn into_result(self, ctx: &mut TulispContext) -> Result<TulispObject, Error> {
        Ok(self.into_tulisp(ctx))
    }
}

impl Return for () {
    fn into_result(self, _ctx: &mut TulispContext) -> Result<TulispObject, Error> {
        Ok(TulispObject::nil())
    }
}

impl<T: Return> Return for Result<T, Error> {
    fn into_result(self, ctx: &mut TulispContext) -> Result<TulispObject, Error> {
        self.and_then(|value| value.into_result(ctx))
    }
}

/// The arity a parameter list declares: a required position is any
/// position up to the last required parameter; a `Rest` or `Plist`
/// parameter, always last, takes the remainder.
pub(crate) fn arity(kinds: &[ParamKind]) -> DefunArity {
    let mut required = 0;
    let mut positional = 0;
    let mut has_rest = false;
    for (index, kind) in kinds.iter().enumerate() {
        match kind {
            ParamKind::Positional { required: true } | ParamKind::Form { required: true } => {
                required = index + 1;
                positional = index + 1;
            }
            ParamKind::Positional { required: false } | ParamKind::Form { required: false } => {
                positional = index + 1
            }
            ParamKind::Rest | ParamKind::Plist | ParamKind::RestForm => has_rest = true,
        }
    }
    DefunArity {
        required,
        optional: positional - required,
        has_rest,
    }
}

/// What a signature shows of one parameter besides its position.
pub(crate) struct ParamDetail {
    /// The Lisp type the parameter converts from.
    pub(crate) type_name: Option<Cow<'static, str>>,
    /// The keys a `Plist` parameter reads.
    pub(crate) keys: Vec<Cow<'static, str>>,
}

impl ParamDetail {
    /// The detail of P, a [`Param`] or a [`SpecialParam`]: every `Param` is a
    /// `SpecialParam` too, with the same detail.
    pub(crate) fn of<P: SpecialParam>() -> Self {
        Self {
            type_name: P::special_type_name(),
            keys: P::special_declared_keys(),
        }
    }
}

/// The signature a parameter list declares, its positions counted as [`arity`]
/// counts them, with each parameter's detail.
pub(crate) fn signature(kinds: &[ParamKind], details: Vec<ParamDetail>) -> Signature {
    debug_assert_eq!(kinds.len(), details.len());
    let required = arity(kinds).required;
    let params = kinds
        .iter()
        .zip(details)
        .enumerate()
        .map(|(index, (kind, detail))| SignatureParam {
            name: None,
            position: match kind {
                ParamKind::Rest | ParamKind::RestForm => ParamPosition::Rest,
                ParamKind::Plist => ParamPosition::Keywords,
                ParamKind::Positional { .. } | ParamKind::Form { .. } if index < required => {
                    ParamPosition::Required
                }
                ParamKind::Positional { .. } | ParamKind::Form { .. } => ParamPosition::Optional,
            },
            type_name: detail.type_name,
            keys: detail.keys,
        })
        .collect();
    Signature { params }
}

/// A closure that [`defun`](TulispContext::defun) and
/// [`defmacro`](TulispContext::defmacro) can register: `Fn(P1, .., Pn) -> R` or
/// `Fn(&mut TulispContext, P1, .., Pn) -> R` for up to twelve parameters and a
/// [`Return`]; every parameter but the last is a [`PositionalParam`], the last
/// any [`Param`].
///
/// Code of an embedder can take one and pass it on to `defun`:
///
/// ```rust
/// use tulisp::{TulispCallable, TulispContext};
///
/// fn register<Args: 'static, Output: 'static, const CTX: bool>(
///     ctx: &mut TulispContext,
///     name: &str,
///     f: impl TulispCallable<Args, Output, CTX> + 'static,
/// ) {
///     ctx.defun(name, f);
/// }
///
/// let mut ctx = TulispContext::new();
/// register(&mut ctx, "twice", |x: i64| x * 2);
/// assert_eq!(ctx.eval_string("(twice 4)").unwrap().to_string(), "8");
/// ```
///
/// Only Tulisp implements it:
///
/// ```compile_fail
/// struct Mine;
/// impl tulisp::TulispCallable<(), i64, false> for Mine {
///     fn add_to_context(
///         self,
///         _: &mut tulisp::TulispContext,
///         _: &str,
///         _: tulisp::Token,
///     ) {
///     }
/// }
/// ```
#[diagnostic::on_unimplemented(
    message = "`defun` and `defmacro` cannot register this closure",
    note = "up to twelve parameters, each `TulispConvertible`; only the last may be `Rest<T>` or `Plist<T>`",
    note = "the return type must be `TulispConvertible`, `()`, or a `Result` of one",
    note = "a `TulispAny` type converts by value only when it is `Clone`; `Shared<T>` converts one that is not"
)]
pub trait TulispCallable<Args: 'static, Output: 'static, const CTX: bool> {
    #[doc(hidden)]
    fn add_to_context(self, ctx: &mut TulispContext, name: &str, _: Token);

    #[doc(hidden)]
    fn add_macro_to_context(self, ctx: &mut TulispContext, name: &str, _: Token);
}

/// Keeps [`TulispCallable`] and [`SpecialCallable`](crate::SpecialCallable) for
/// Tulisp to implement: their method takes one, and no other crate can name it.
pub struct Token(pub(crate) ());

/// The name of a definition, alone or with a docstring: `"name"`, or `("name",
/// "Docstring.")`. A `String`, or a reference to anything that gives a `&str`
/// with `as_ref`, such as `&String` or `&Box<str>`, works too. Every `Name` is
/// a [`FunctionName`] as well.
///
/// Only Tulisp implements it.
pub trait Name {
    #[doc(hidden)]
    fn parts(&self, _: Token) -> (&str, Option<&str>);
}

impl Name for String {
    fn parts(&self, _: Token) -> (&str, Option<&str>) {
        (self, None)
    }
}

impl<T: AsRef<str> + ?Sized> Name for &T {
    fn parts(&self, _: Token) -> (&str, Option<&str>) {
        ((*self).as_ref(), None)
    }
}

impl<T: AsRef<str> + ?Sized> Name for &mut T {
    fn parts(&self, _: Token) -> (&str, Option<&str>) {
        ((**self).as_ref(), None)
    }
}

impl<N: AsRef<str>, D: AsRef<str>> Name for (N, D) {
    fn parts(&self, _: Token) -> (&str, Option<&str>) {
        (self.0.as_ref(), Some(self.1.as_ref()))
    }
}

/// The name [`defun`](TulispContext::defun),
/// [`defspecial`](TulispContext::defspecial) and
/// [`defmacro`](TulispContext::defmacro) define, for a function whose
/// parameters are `Args`. It is any [`Name`], or the name, the parameters'
/// names and a docstring, in the order of a Lisp `defun`:
///
/// ```rust
/// use tulisp::TulispContext;
///
/// let mut ctx = TulispContext::new();
/// ctx.defun(
///     ("connect", ["host", "port"], "Connect to HOST."),
///     |host: String, port: Option<i64>| format!("{host}:{}", port.unwrap_or(80)),
/// );
/// let info = ctx.describe("connect").unwrap();
/// assert_eq!(info.doc.as_deref(), Some("Connect to HOST."));
/// assert_eq!(info.signature.unwrap().render("connect"), "(connect HOST &optional PORT)");
/// ```
///
/// The names are an array of `&str`, one for each parameter. A `&mut
/// TulispContext` parameter gets no name. The compiler checks the count:
///
/// ```compile_fail,E0277
/// let mut ctx = tulisp::TulispContext::new();
/// ctx.defun(("add", ["a"], "Add A and B."), |a: i64, b: i64| a + b);
/// ```
///
/// The docstring and the names stay with the name while it holds the function:
/// they go when the name is defined again, when `fset` gives it another value,
/// and when `fmakunbound` clears it.
///
/// Only Tulisp implements it.
#[diagnostic::on_unimplemented(
    message = "`{Self}` is not a name `defun`, `defspecial` or `defmacro` can take for this function",
    note = "a name is a `String` or a reference such as `&str`, a `(name, doc)` pair, or `(name, [names], doc)`",
    note = "with a `(name, [names], doc)` name, give one name for each parameter, not counting `&mut TulispContext`. At most twelve parameters can have names"
)]
pub trait FunctionName<Args> {
    #[doc(hidden)]
    fn parts(&self, _: Token) -> (&str, Option<&[&str]>, Option<&str>);
}

impl<Args, N: Name> FunctionName<Args> for N {
    fn parts(&self, token: Token) -> (&str, Option<&[&str]>, Option<&str>) {
        let (name, doc) = Name::parts(self, token);
        (name, None, doc)
    }
}

impl<Args, N: AsRef<str>, P: ParamNames<Args>, D: AsRef<str>> FunctionName<Args> for (N, P, D) {
    fn parts(&self, token: Token) -> (&str, Option<&[&str]>, Option<&str>) {
        (
            self.0.as_ref(),
            Some(self.1.names(token)),
            Some(self.2.as_ref()),
        )
    }
}

/// The names of a function's parameters, one for each of `Args`: an array of
/// `&str`, as many names as the function has parameters, not counting a `&mut
/// TulispContext` one.
///
/// Only Tulisp implements it.
#[diagnostic::on_unimplemented(
    message = "`{Self}` does not give one name for each parameter",
    label = "expected one name for each parameter in `{Args}`",
    note = "a `&mut TulispContext` parameter gets no name"
)]
pub trait ParamNames<Args> {
    #[doc(hidden)]
    fn names(&self, _: Token) -> &[&str];
}

/// The `Args` of a second `ParamNames` impl on each array of up to twelve
/// names. Each of those lengths also has a real impl. With two impls, the
/// array's length cannot pick `Args`, so the closure's parameter types pick it.
/// Most compilers then report a wrong count of names on the names; some report
/// it on the whole name. `do_not_recommend` keeps these impls out of the
/// error's list of impls.
struct ParamsDecoy;

macro_rules! count_params {
    () => { 0 };
    ($head:ident $($tail:ident)*) => { 1 + count_params!($($tail)*) };
}

macro_rules! impl_tulisp_callable {
    // What `defun` and `defmacro` both register: the arity, the signature, and
    // a closure that takes the parameters from the arguments and calls `$func`.
    // `$cx` is the name the closure binds the context to.
    (@parts $func:ident, $cx:ident, ($($call_ctx:tt)*), ($($p:ident),*), ($($last:ident)?)) => {{
        let kinds = [$(<$p as Param>::KIND,)* $(<$last as Param>::KIND,)?];
        let details = vec![$(ParamDetail::of::<$p>(),)* $(ParamDetail::of::<$last>(),)?];
        let call = move |$cx: &mut TulispContext, args: &[TulispObject]| {
            let mut args = args;
            $(let $p = <$p as Param>::take($cx, &mut args)?;)*
            $(let $last = <$last as Param>::take($cx, &mut args)?;)?
            ($func)($($call_ctx)* $($p,)* $($last)?).into_result($cx)
        };
        (arity(&kinds), signature(&kinds, details), call)
    }};
    // One impl per arity for closures with and without the context parameter.
    // Every parameter but the last binds one position.
    (@impl $ctx:literal, $cx:ident, ($($fn_ctx:tt)*), ($($call_ctx:tt)*), ($($p:ident),*), ($($last:ident)?)) => {
        #[allow(nonstandard_style)]
        impl<FnT, R, $($p,)* $($last,)?> TulispCallable<($($p,)* $($last,)?), R, $ctx> for FnT
        where
            FnT: Fn($($fn_ctx)* $($p,)* $($last)?) -> R + SendSyncIfSync + 'static,
            R: Return,
            $($p: PositionalParam,)*
            $($last: Param,)?
        {
            // `define_typed_defun` records the caller's location for TAGS.
            #[track_caller]
            #[allow(unused_mut, unused_variables)]
            fn add_to_context(self, ctx: &mut TulispContext, name: &str, _: Token) {
                let (arity, signature, call) = impl_tulisp_callable!(
                    @parts self, $cx, ($($call_ctx)*), ($($p),*), ($($last)?)
                );
                ctx.define_typed_defun(name, arity, signature, call);
            }

            #[track_caller]
            #[allow(unused_mut, unused_variables)]
            fn add_macro_to_context(self, ctx: &mut TulispContext, name: &str, _: Token) {
                let (arity, signature, call) = impl_tulisp_callable!(
                    @parts self, $cx, ($($call_ctx)*), ($($p),*), ($($last)?)
                );
                ctx.define_typed_macro(name, arity, signature, call);
            }
        }
    };
    // The names of the parameters, and the closures with and without the
    // context parameter.
    (($($p:ident),*), ($($last:ident)?)) => {
        impl<'a, $($p,)* $($last,)?> ParamNames<($($p,)* $($last,)?)>
            for [&'a str; count_params!($($p)* $($last)?)]
        {
            fn names(&self, _: Token) -> &[&str] {
                self
            }
        }
        #[diagnostic::do_not_recommend]
        impl<'a> ParamNames<ParamsDecoy> for [&'a str; count_params!($($p)* $($last)?)] {
            fn names(&self, _: Token) -> &[&str] {
                self
            }
        }
        impl_tulisp_callable!(@impl false, cx, (), (), ($($p),*), ($($last)?));
        impl_tulisp_callable!(@impl true, cx, (&mut TulispContext,), (cx,), ($($p),*), ($($last)?));
    };
}

impl_tulisp_callable!((), ());
impl_tulisp_callable!((), (A));
impl_tulisp_callable!((A), (B));
impl_tulisp_callable!((A, B), (C));
impl_tulisp_callable!((A, B, C), (D));
impl_tulisp_callable!((A, B, C, D), (E));
impl_tulisp_callable!((A, B, C, D, E), (F));
impl_tulisp_callable!((A, B, C, D, E, F), (G));
impl_tulisp_callable!((A, B, C, D, E, F, G), (H));
impl_tulisp_callable!((A, B, C, D, E, F, G, H), (I));
impl_tulisp_callable!((A, B, C, D, E, F, G, H, I), (J));
impl_tulisp_callable!((A, B, C, D, E, F, G, H, I, J), (K));
impl_tulisp_callable!((A, B, C, D, E, F, G, H, I, J, K), (L));

#[cfg(test)]
mod tests {
    use super::{Param, ParamKind, arity};
    use crate::test_utils::{eval_assert, eval_assert_equal, eval_assert_error};
    use crate::value::DefunArity;
    use crate::{Error, Plist, Rest, TulispContext, TulispObject, list};

    #[test]
    fn a_macro_takes_its_arguments_as_written() {
        let ctx = &mut TulispContext::new();
        ctx.defmacro(
            "quoted",
            |ctx: &mut TulispContext, form: TulispObject, other: Option<TulispObject>| {
                list!(,ctx.intern("quote") ,list!(,form ,other)?)
            },
        );
        eval_assert_equal(ctx, "(quoted (+ 1 2))", "'((+ 1 2) nil)");
        eval_assert_equal(ctx, "(quoted (+ 1 2) x)", "'((+ 1 2) x)");
        // As in Lisp, a written nil is no argument: the option is `None`.
        ctx.defmacro(
            "given",
            |_form: TulispObject, other: Option<TulispObject>| other.is_some(),
        );
        eval_assert_equal(
            ctx,
            "(list (given 1) (given 1 nil) (given 1 x))",
            "'(nil nil t)",
        );
        // Another type converts the form as written.
        ctx.defmacro(
            "times",
            |ctx: &mut TulispContext, count: i64, body: Rest<TulispObject>| {
                let mut forms = Vec::new();
                for _ in 0..count {
                    forms.extend(body.iter().cloned());
                }
                list!(,ctx.intern("progn") ,@forms)
            },
        );
        eval_assert_equal(ctx, "(let ((x 0)) (times 3 (setq x (1+ x))) x)", "3");
        eval_assert_error(
            ctx,
            "(times (+ 1 2) nil)",
            "ERR TypeMismatch: Expected integer: (+ 1 2)\n\
             <eval_string>:1.8-1.14:  at (+ 1 2)\n\
             <eval_string>:1.1-1.19:  at (times (+ 1 2) nil)\n",
        );
    }

    #[test]
    fn a_macro_call_with_a_wrong_count_is_an_error() {
        let ctx = &mut TulispContext::new();
        ctx.defmacro("one", |form: TulispObject| form);
        eval_assert_error(
            ctx,
            "(one)",
            "ERR ArityMismatch: Too few arguments\n\
             <eval_string>:1.1-1.5:  at (one)\n",
        );
        eval_assert_error(
            ctx,
            "(one 1 2)",
            "ERR ArityMismatch: Too many arguments\n\
             <eval_string>:1.1-1.9:  at (one 1 2)\n",
        );
        // So is an argument list that does not end in nil.
        let err = ctx.eval_string("(one . 1)").unwrap_err();
        assert!(!err.to_string().contains("Arity"), "{err}");
    }

    #[test]
    fn arity_counts_forms_as_positions() {
        let a = arity(&[
            ParamKind::Positional { required: true },
            ParamKind::Form { required: true },
            ParamKind::Form { required: false },
            ParamKind::RestForm,
        ]);
        assert_eq!((a.required, a.optional, a.has_rest), (2, 1, true));
    }

    #[test]
    fn a_missing_required_position_is_an_error() {
        let mut ctx = TulispContext::new();
        let mut none: &[TulispObject] = &[];
        let err = <i64 as Param>::take(&mut ctx, &mut none).unwrap_err();
        assert!(err.to_string().contains("Too few arguments"), "{err}");
    }

    #[test]
    fn a_parameter_kind_comes_from_its_type() {
        assert!(matches!(
            <i64 as Param>::KIND,
            ParamKind::Positional { required: true }
        ));
        assert!(matches!(<Rest<i64> as Param>::KIND, ParamKind::Rest));
    }

    #[test]
    fn test_add_functions_only_rest() -> Result<(), crate::Error> {
        let ctx = &mut TulispContext::new();
        ctx.defun("sum", |items: Rest<f64>| -> f64 { items.into_iter().sum() });

        // The empty sum is a float zero of either sign; compare with `=`.
        eval_assert(ctx, "(and (floatp (sum)) (= (sum) 0))");
        eval_assert_equal(ctx, "(sum (- 4 0.5) (+ 3 4) 5 10)", "25.5");
        eval_assert_error(
            ctx,
            r#"(sum "hh" 10)"#,
            r#"ERR TypeMismatch: Expected number, got: "hh"
<eval_string>:1.1-1.13:  at (sum "hh" 10)
"#,
        );

        ctx.defun(
            "cats",
            |items: Rest<String>| -> Result<TulispObject, crate::Error> {
                for item in items {
                    if item == "cat" {
                        return Ok("meow".into());
                    }
                    if item == "stop" {
                        return Ok(false.into());
                    }
                }
                Err(crate::Error::invalid_argument("No cats found"))
            },
        );

        eval_assert_equal(ctx, r#"(let ((a "stop"))(cats "dog" a "cat"))"#, "nil");
        eval_assert_error(
            ctx,
            r#"(cats 1 2 3)"#,
            r#"ERR TypeMismatch: Expected string, got: 1
<eval_string>:1.1-1.12:  at (cats 1 2 3)
"#,
        );
        eval_assert_error(
            ctx,
            r#"(let ((horse "horse"))(cats horse))"#,
            r#"ERR InvalidArgument: No cats found
<eval_string>:1.23-1.34:  at (cats horse)
<eval_string>:1.1-1.35:  at (let ((horse "horse")) (cats horse))
"#,
        );

        Ok(())
    }

    #[test]
    fn test_add_functions_only_args() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();

        ctx.defun("add_round", |a: f64, b: f64| -> i64 {
            (a + b).round() as i64
        });
        eval_assert_equal(ctx, "(let ((a 3.5) (b 4.2))(add_round a b))", "8");
        eval_assert_error(
            ctx,
            "(add_round 2)",
            r#"ERR ArityMismatch: Too few arguments
<eval_string>:1.1-1.13:  at (add_round 2)
"#,
        );

        ctx.defun("greet", |name: String| format!("Hello, {}!", name));
        eval_assert_equal(ctx, r#"(greet "Alice")"#, r#""Hello, Alice!""#);
        eval_assert_error(
            ctx,
            r#"(greet "Alice" "Peter")"#,
            r#"ERR ArityMismatch: Too many arguments
<eval_string>:1.1-1.23:  at (greet "Alice" "Peter")
"#,
        );

        Ok(())
    }

    #[test]
    fn test_add_functions_args_and_rest() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();

        ctx.defun(
            "make_sentence",
            |subject: String, verb: String, objects: Rest<String>| -> String {
                let objs = objects.into_iter().collect::<Vec<_>>().join(" ");
                format!("{} {} {}", subject, verb, objs)
            },
        );

        eval_assert_equal(
            ctx,
            r#"(make_sentence "The cat" "chased" "the mouse" "and" "the dog")"#,
            r#""The cat chased the mouse and the dog""#,
        );

        eval_assert_equal(ctx, r#"(make_sentence "Birds" "fly")"#, r#""Birds fly ""#);

        Ok(())
    }

    #[test]
    fn test_add_functions_args_and_optional() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();

        ctx.defun("power", |base: f64, exponent: Option<f64>| -> f64 {
            let exp = exponent.unwrap_or(2.0);
            base.powf(exp)
        });

        eval_assert_equal(ctx, "(power 3)", "9.0");
        eval_assert_equal(ctx, "(power 2 3)", "8.0");
        eval_assert_error(
            ctx,
            "(power 2 3 4)",
            r#"ERR ArityMismatch: Too many arguments
<eval_string>:1.1-1.13:  at (power 2 3 4)
"#,
        );
        eval_assert_error(
            ctx,
            "(power)",
            r#"ERR ArityMismatch: Too few arguments
<eval_string>:1.1-1.7:  at (power)
"#,
        );
        Ok(())
    }

    #[test]
    fn test_add_functions_args_optional_and_rest() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();

        ctx.defun(
            "build_url",
            |base: String, path: Option<String>, query_params: Rest<String>| -> String {
                let mut url = base;
                if let Some(p) = path {
                    url.push('/');
                    url.push_str(&p);
                }
                let joined = query_params.into_iter().collect::<Vec<_>>().join("&");
                if !joined.is_empty() {
                    url.push('?');
                    url.push_str(&joined);
                }
                url
            },
        );

        eval_assert_equal(
            ctx,
            r#"(build_url "http://example.com" "search" "q=rust" "page=1")"#,
            r#""http://example.com/search?q=rust&page=1""#,
        );

        eval_assert_equal(
            ctx,
            r#"(build_url "http://example.com")"#,
            r#""http://example.com""#,
        );

        eval_assert_equal(
            ctx,
            r#"(build_url "http://example.com" "about")"#,
            r#""http://example.com/about""#,
        );

        Ok(())
    }

    crate::AsList! {
        struct Cfg {
            a: i64,
            b: Option<i64>,
        }
    }

    crate::AsList! {
        struct Opt {
            a: Option<i64>,
            b: Option<i64>,
        }
    }

    fn arity_of(kinds: &[ParamKind]) -> (usize, usize, bool) {
        let DefunArity {
            required,
            optional,
            has_rest,
        } = arity(kinds);
        (required, optional, has_rest)
    }

    #[test]
    fn arity_comes_from_the_parameter_types() {
        assert_eq!(arity_of(&[]), (0, 0, false));
        assert_eq!(
            arity_of(&[<i64 as Param>::KIND, <Option<i64> as Param>::KIND]),
            (1, 1, false)
        );
        assert_eq!(
            arity_of(&[<Option<i64> as Param>::KIND, <i64 as Param>::KIND]),
            (2, 0, false)
        );
        assert_eq!(
            arity_of(&[<i64 as Param>::KIND, <Rest<i64> as Param>::KIND]),
            (1, 0, true)
        );
        assert!(matches!(<Plist<Cfg> as Param>::KIND, ParamKind::Plist));
    }

    #[test]
    fn an_absent_or_nil_optional_position_is_none() {
        let mut ctx = TulispContext::new();
        ctx.defun("opt", |a: i64, b: Option<i64>| -> i64 {
            a + b.unwrap_or(10)
        });
        eval_assert_equal(&mut ctx, "(opt 1 2)", "3");
        eval_assert_equal(&mut ctx, "(opt 1)", "11");
        eval_assert_equal(&mut ctx, "(opt 1 nil)", "11");
    }

    #[test]
    fn an_option_before_a_required_parameter_still_takes_a_position() {
        let mut ctx = TulispContext::new();
        ctx.defun("mid", |a: Option<i64>, b: i64| -> i64 {
            a.unwrap_or(0) + b
        });
        eval_assert_equal(&mut ctx, "(mid nil 2)", "2");
        eval_assert_equal(&mut ctx, "(mid 1 2)", "3");
        eval_assert_error(
            &mut ctx,
            "(mid 2)",
            r#"ERR ArityMismatch: Too few arguments
<eval_string>:1.1-1.7:  at (mid 2)
"#,
        );
    }

    #[test]
    fn a_keyword_tail_binds_the_remaining_arguments() {
        let mut ctx = TulispContext::new();
        ctx.defun("cfg", |scale: i64, c: Plist<Cfg>| -> i64 {
            scale * (c.a + c.b.unwrap_or(0))
        });
        eval_assert_equal(&mut ctx, "(cfg 2 :a 3)", "6");
        eval_assert_equal(&mut ctx, "(cfg 2 :a 3 :b 4)", "14");
    }

    #[test]
    fn a_keyword_tail_with_no_arguments_left_binds_an_empty_plist() {
        let mut ctx = TulispContext::new();
        ctx.defun("scaled", |scale: i64, c: Plist<Opt>| -> i64 {
            scale * (c.a.unwrap_or(1) + c.b.unwrap_or(1))
        });
        eval_assert_equal(&mut ctx, "(scaled 2)", "4");
        eval_assert_equal(&mut ctx, "(scaled 2 :a 3)", "8");
    }

    #[test]
    fn an_absent_optional_position_before_a_keyword_tail_binds_an_empty_plist() {
        let mut ctx = TulispContext::new();
        ctx.defun("g", |a: Option<i64>, p: Plist<Opt>| -> i64 {
            a.unwrap_or(-1) + p.a.unwrap_or(-5)
        });
        eval_assert_equal(&mut ctx, "(g)", "-6");
        eval_assert_equal(&mut ctx, "(g nil :a 3)", "2");
    }

    #[test]
    fn returns_fold_unit_and_result() {
        let mut ctx = TulispContext::new();
        ctx.defun("nothing", |_a: i64| {});
        eval_assert_equal(&mut ctx, "(nothing 1)", "nil");
        ctx.defun("checked-nothing", |a: i64| -> Result<(), Error> {
            if a < 0 {
                Err(Error::invalid_argument("negative".to_string()))
            } else {
                Ok(())
            }
        });
        eval_assert_equal(&mut ctx, "(checked-nothing 1)", "nil");
        ctx.defun("checked", |a: i64| -> Result<i64, Error> {
            if a < 0 {
                Err(Error::invalid_argument("negative".to_string()))
            } else {
                Ok(a)
            }
        });
        eval_assert_equal(&mut ctx, "(checked 1)", "1");
        eval_assert_error(
            &mut ctx,
            "(checked -1)",
            r#"ERR InvalidArgument: negative
<eval_string>:1.1-1.12:  at (checked -1)
"#,
        );
        ctx.defun(
            "with-ctx",
            |ctx: &mut TulispContext, name: String| -> TulispObject { ctx.intern(&name) },
        );
        eval_assert_equal(&mut ctx, "(with-ctx \"foo\")", "'foo");
    }

    #[test]
    fn a_keyword_tail_binds_after_the_context_parameter() {
        let mut ctx = TulispContext::new();
        ctx.defun(
            "tagged",
            |ctx: &mut TulispContext, name: String, c: Plist<Cfg>| -> TulispObject {
                ctx.intern(&format!("{name}{}", c.a))
            },
        );
        eval_assert_equal(&mut ctx, "(tagged \"n\" :a 3)", "'n3");
    }

    #[test]
    fn twelve_parameters_are_accepted() {
        let mut ctx = TulispContext::new();
        ctx.defun(
            "twelve",
            |a: i64,
             b: i64,
             c: i64,
             d: i64,
             e: i64,
             f: i64,
             g: i64,
             h: i64,
             i: i64,
             j: i64,
             k: i64,
             l: i64|
             -> i64 { a + b + c + d + e + f + g + h + i + j + k + l },
        );
        eval_assert_equal(&mut ctx, "(twelve 1 1 1 1 1 1 1 1 1 1 1 1)", "12");
    }
}
