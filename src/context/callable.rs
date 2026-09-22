use crate::object::wrappers::generic::SyncSend;
use crate::value::DefunArity;
use crate::{Error, Plist, Plistable, Rest, TulispContext, TulispConvertible, TulispObject};

/// How a closure parameter takes its value from a call's arguments.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ParamKind {
    /// One argument at this position; `required` is false for a
    /// parameter that may be absent.
    Positional { required: bool },
    /// Every remaining argument, as a [`Rest<T>`].
    Rest,
    /// Every remaining argument, as keyword/value pairs in a
    /// [`Plist<T>`].
    Plist,
}

/// A parameter of a function registered with
/// [`defun`](TulispContext::defun).
pub trait Param: Sized + 'static {
    const KIND: ParamKind;

    /// Takes this parameter's value from the front of `args`, leaving
    /// the arguments it did not consume. Arity has been checked by
    /// the caller, so a required position is always present.
    fn take(ctx: &mut TulispContext, args: &mut &[TulispObject]) -> Result<Self, Error>;
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
}

impl<T: TulispConvertible + 'static> Param for Rest<T> {
    const KIND: ParamKind = ParamKind::Rest;

    fn take(ctx: &mut TulispContext, args: &mut &[TulispObject]) -> Result<Self, Error> {
        std::mem::take(args)
            .iter()
            .map(|arg| T::from_tulisp(ctx, arg))
            .collect::<Result<Rest<T>, Error>>()
    }
}

impl<T: Plistable + 'static> Param for Plist<T> {
    const KIND: ParamKind = ParamKind::Plist;

    fn take(ctx: &mut TulispContext, args: &mut &[TulispObject]) -> Result<Self, Error> {
        Plist::new(ctx, std::mem::take(args))
    }
}

/// A [`Param`] that binds one argument position, so it may come
/// before another parameter; [`Rest<T>`] and [`Plist<T>`] may not. A
/// hand-written [`Param`] needs this impl too to sit anywhere but last.
pub trait PositionalParam: Param {}

impl<T: TulispConvertible + 'static> PositionalParam for T {}

/// The value a [`defun`](TulispContext::defun) closure returns.
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
            ParamKind::Positional { required: true } => {
                required = index + 1;
                positional = index + 1;
            }
            ParamKind::Positional { required: false } => positional = index + 1,
            ParamKind::Rest | ParamKind::Plist => has_rest = true,
        }
    }
    DefunArity {
        required,
        optional: positional - required,
        has_rest,
    }
}

/// A closure that [`defun`](TulispContext::defun) can register:
/// `Fn(P1, .., Pn) -> R` or `Fn(&mut TulispContext, P1, .., Pn) -> R`
/// for up to twelve parameters and a [`Return`]; every parameter but
/// the last is a [`PositionalParam`], the last any [`Param`].
#[diagnostic::on_unimplemented(
    message = "`defun` cannot register this closure",
    note = "up to twelve parameters, each `TulispConvertible`; only the last may be `Rest<T>` or `Plist<T>`",
    note = "the return type must be `TulispConvertible`, `()`, or a `Result` of one",
    note = "a `TulispAny` type converts by value only when it is `Clone`; `Shared<T>` converts one that is not"
)]
pub trait TulispCallable<Args: 'static, Output: 'static, const CTX: bool> {
    fn add_to_context(self, ctx: &mut TulispContext, name: &str);
}

macro_rules! impl_tulisp_callable {
    // One impl per arity for closures with and without the context
    // parameter; `$cx` is the name the closure binds it to. Every
    // parameter but the last binds one position.
    (@impl $ctx:literal, $cx:ident, ($($fn_ctx:tt)*), ($($call_ctx:tt)*), ($($p:ident),*), ($($last:ident)?)) => {
        #[allow(nonstandard_style)]
        impl<FnT, R, $($p,)* $($last,)?> TulispCallable<($($p,)* $($last,)?), R, $ctx> for FnT
        where
            FnT: Fn($($fn_ctx)* $($p,)* $($last)?) -> R + SyncSend + 'static,
            R: Return,
            $($p: PositionalParam,)*
            $($last: Param,)?
        {
            // `define_typed_defun` records the caller's location for TAGS.
            #[track_caller]
            #[allow(unused_mut, unused_variables)]
            fn add_to_context(self, ctx: &mut TulispContext, name: &str) {
                let arity = arity(&[$(<$p as Param>::KIND,)* $(<$last as Param>::KIND,)?]);
                ctx.define_typed_defun(name, arity, move |$cx, args| {
                    let mut args = args;
                    $(let $p = <$p as Param>::take($cx, &mut args)?;)*
                    $(let $last = <$last as Param>::take($cx, &mut args)?;)?
                    (self)($($call_ctx)* $($p,)* $($last)?).into_result($cx)
                });
            }
        }
    };
    (($($p:ident),*), ($($last:ident)?)) => {
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
    use crate::{Error, Plist, Rest, TulispContext, TulispObject};

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
