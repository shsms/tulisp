use std::sync::atomic::{AtomicBool, Ordering};

use crate::bytecode::Block;
use crate::object::wrappers::generic::{Shared, SyncSend};
use crate::{
    Error, Param, ParamKind, PositionalParam, Rest, Return, TulispContext, TulispConvertible,
    TulispObject,
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
    block: Block,
    live: Shared<AtomicBool>,
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
        crate::bytecode::run_block(ctx, &self.block, None)
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
pub(crate) struct CallForms {
    live: Shared<AtomicBool>,
}

impl CallForms {
    pub(crate) fn new() -> Self {
        CallForms {
            live: Shared::new(AtomicBool::new(true)),
        }
    }

    pub(crate) fn form(&self, block: Block, source: TulispObject) -> Form {
        Form {
            source,
            block,
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
///
/// `defspecial` refuses a hand-written [`Param`] whose kind is
/// `ParamKind::Form` or `ParamKind::RestForm` when the program is
/// built (`cargo check` does not report it):
///
/// ```compile_fail
/// use tulisp::{Error, Param, ParamKind, TulispContext, TulispObject};
///
/// struct Raw(TulispObject);
/// impl Param for Raw {
///     const KIND: ParamKind = ParamKind::Form { required: true };
///     fn take(_: &mut TulispContext, args: &mut &[TulispObject]) -> Result<Self, Error> {
///         let (first, rest) = args.split_first().ok_or_else(Error::too_few_arguments)?;
///         *args = rest;
///         Ok(Raw(first.clone()))
///     }
/// }
///
/// let mut ctx = TulispContext::new();
/// ctx.defspecial("raw", |raw: Raw| raw.0);
/// ```
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

/// A closure that [`defspecial`](TulispContext::defspecial) can
/// register: `Fn(P1, .., Pn) -> R` or
/// `Fn(&mut TulispContext, P1, .., Pn) -> R` for up to twelve
/// parameters and a [`Return`]; every parameter but the last is a
/// [`SpecialPositionalParam`], the last any [`SpecialParam`].
#[diagnostic::on_unimplemented(
    message = "`defspecial` cannot register this closure",
    note = "up to twelve parameters, each `TulispConvertible` or `Form`; only the last may be `Rest<T>`, `Plist<T>` or `Rest<Form>`",
    note = "the return type must be `TulispConvertible`, `()`, or a `Result` of one"
)]
pub trait SpecialCallable<Args: 'static, Output: 'static, const CTX: bool> {
    fn add_to_context(self, ctx: &mut TulispContext, name: &str);
}

macro_rules! impl_special_callable {
    // One impl per arity for closures with and without the context
    // parameter; `$cx` is the name the closure binds it to. Every
    // parameter but the last binds one position.
    (@impl $ctx:literal, $cx:ident, ($($fn_ctx:tt)*), ($($call_ctx:tt)*), ($($p:ident),*), ($($last:ident)?)) => {
        #[allow(nonstandard_style)]
        impl<FnT, R, $($p,)* $($last,)?> SpecialCallable<($($p,)* $($last,)?), R, $ctx> for FnT
        where
            FnT: Fn($($fn_ctx)* $($p,)* $($last)?) -> R + SyncSend + 'static,
            R: Return,
            $($p: SpecialPositionalParam,)*
            $($last: SpecialParam,)?
        {
            // `define_special` records the caller's location for TAGS.
            #[track_caller]
            #[allow(unused_mut, unused_variables)]
            fn add_to_context(self, ctx: &mut TulispContext, name: &str) {
                let kinds = vec![$(<$p as SpecialParam>::KIND,)* $(<$last as SpecialParam>::KIND,)?];
                ctx.define_special(name, kinds, move |$cx, values, forms| {
                    let mut args = SpecialArgs { values, forms: forms.into_iter() };
                    $(let $p = <$p as SpecialParam>::take($cx, &mut args)?;)*
                    $(let $last = <$last as SpecialParam>::take($cx, &mut args)?;)?
                    (self)($($call_ctx)* $($p,)* $($last)?).into_result($cx)
                });
            }
        }
    };
    (($($p:ident),*), ($($last:ident)?)) => {
        impl_special_callable!(@impl false, cx, (), (), ($($p),*), ($($last)?));
        impl_special_callable!(@impl true, cx, (&mut TulispContext,), (cx,), ($($p),*), ($($last)?));
    };
}

impl_special_callable!((), ());
impl_special_callable!((), (A));
impl_special_callable!((A), (B));
impl_special_callable!((A, B), (C));
impl_special_callable!((A, B, C), (D));
impl_special_callable!((A, B, C, D), (E));
impl_special_callable!((A, B, C, D, E), (F));
impl_special_callable!((A, B, C, D, E, F), (G));
impl_special_callable!((A, B, C, D, E, F, G), (H));
impl_special_callable!((A, B, C, D, E, F, G, H), (I));
impl_special_callable!((A, B, C, D, E, F, G, H, I), (J));
impl_special_callable!((A, B, C, D, E, F, G, H, I, J), (K));
impl_special_callable!((A, B, C, D, E, F, G, H, I, J, K), (L));

#[cfg(test)]
mod tests {
    use super::takes_form;
    use crate::ParamKind;
    use crate::test_utils::{eval_assert_equal, eval_assert_error_line, eval_assert_not};
    use crate::{Error, Form, Rest, TulispContext, TulispObject};
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::{Arc, Mutex};

    fn eval_to_string(ctx: &mut TulispContext, program: &str) -> String {
        match ctx.eval_string(program) {
            Ok(value) => value.to_string(),
            Err(err) => err.format(ctx),
        }
    }

    fn with_forms() -> TulispContext {
        let mut ctx = TulispContext::new();
        ctx.defspecial("never", |_form: Form| ());
        ctx.defspecial(
            "twice",
            |ctx: &mut TulispContext, form: Form| -> Result<TulispObject, Error> {
                form.eval(ctx)?;
                form.eval(ctx)
            },
        );
        ctx.defspecial(
            "my-or",
            |ctx: &mut TulispContext, forms: Rest<Form>| -> Result<TulispObject, Error> {
                for form in forms {
                    let value = form.eval(ctx)?;
                    if value.is_truthy() {
                        return Ok(value);
                    }
                }
                Ok(TulispObject::nil())
            },
        );
        ctx.defspecial("my-progn", |ctx: &mut TulispContext, body: Rest<Form>| {
            body.eval_progn(ctx)
        });
        ctx.defspecial(
            "maybe",
            |ctx: &mut TulispContext, form: Option<Form>| -> Result<TulispObject, Error> {
                match form {
                    Some(form) => form.eval(ctx),
                    None => Ok(ctx.intern("absent")),
                }
            },
        );
        ctx.defspecial("source-of", |form: Form| form.source().clone());
        ctx.defspecial(
            "eval-source",
            |ctx: &mut TulispContext, form: Form| -> Result<TulispObject, Error> {
                let source = form.source().clone();
                ctx.eval(&source)
            },
        );
        ctx
    }

    #[test]
    fn a_form_runs_as_often_as_the_closure_asks() {
        let ctx = &mut with_forms();
        let program = "(defvar n 0) (setq n 0)
                       (never (setq n (+ n 1)))
                       (twice (setq n (+ n 1)))
                       n";
        eval_assert_equal(ctx, program, "2");
    }

    #[test]
    fn eager_arguments_run_in_order_before_the_call() {
        let ctx = &mut with_forms();
        ctx.defspecial(
            "eager-first",
            |ctx: &mut TulispContext, a: i64, form: Form, b: i64| -> Result<TulispObject, Error> {
                let log = ctx.intern("log").get()?;
                let value = form.eval(ctx)?;
                Ok([TulispObject::from(a), b.into(), log, value]
                    .into_iter()
                    .collect())
            },
        );
        let program = "(defvar log nil) (setq log nil)
                       (eager-first (progn (setq log (cons 'a log)) 1)
                                    (setq log (cons 'form log))
                                    (progn (setq log (cons 'b log)) 2))";
        eval_assert_equal(ctx, program, "'(1 2 (b a) (form b a))");
    }

    #[test]
    fn an_optional_form() {
        let ctx = &mut with_forms();
        eval_assert_equal(ctx, "(list (maybe) (maybe (+ 1 2)))", "'(absent 3)");
    }

    #[test]
    fn rest_forms_run_one_by_one() {
        let ctx = &mut with_forms();
        let program = "(defvar n 0) (setq n 0)
                       (list (my-or nil (progn (setq n (+ n 1)) 7) (setq n 100)) n)";
        eval_assert_equal(ctx, program, "'(7 1)");
        eval_assert_equal(ctx, "(list (my-progn) (my-progn 1 2 3))", "'(nil 3)");
    }

    #[test]
    fn a_form_source_holds_the_call_variables() {
        let ctx = &mut with_forms();
        eval_assert_equal(ctx, "(source-of (+ 1 2))", "'(+ 1 2)");
        eval_assert_equal(ctx, "(let ((x 5)) (eval-source x))", "5");
        let program = "(defun make-source (x) (lambda () (eval-source x)))
                       (list (funcall (make-source 1)) (funcall (make-source 2)))";
        eval_assert_equal(ctx, program, "'(1 2)");
        // The variables inside a backquote in the source too.
        let program = "(defun make-quoted (x) (lambda () (eval-source `(a ,x ,@(list x)))))
                       (funcall (make-quoted 7))";
        eval_assert_equal(ctx, program, "'(a 7 7)");
        let program = "(defun make-dotted (x) (lambda () (eval-source `(a . ,x))))
                       (funcall (make-dotted 7))";
        eval_assert_equal(ctx, program, "'(a . 7)");
    }

    // The closure's own code after the forms runs when they error or
    // throw.
    #[test]
    fn teardown_runs_after_an_error_or_a_throw() {
        let ctx = &mut with_forms();
        let cleanups = Arc::new(AtomicUsize::new(0));
        let seen = cleanups.clone();
        ctx.defspecial(
            "with-cleanup",
            move |ctx: &mut TulispContext, body: Rest<Form>| -> Result<TulispObject, Error> {
                let result = body.eval_progn(ctx);
                seen.fetch_add(1, Ordering::Relaxed);
                result
            },
        );
        let program = "(list (condition-case nil (with-cleanup (error \"boom\")) (error 'caught))
                             (catch 'tag (with-cleanup (throw 'tag 1))))";
        eval_assert_equal(ctx, program, "'(caught 1)");
        assert_eq!(cleanups.load(Ordering::Relaxed), 2);
    }

    // An error leaving a form does not disturb the values the outer
    // run has on its stack.
    #[test]
    fn an_error_in_a_form_leaves_the_outer_stack_intact() {
        let ctx = &mut with_forms();
        let program = "(list 1 (condition-case nil (my-progn (list 7 (car 5))) (error 'caught)) 3)";
        eval_assert_equal(ctx, program, "'(1 caught 3)");
    }

    #[test]
    fn a_form_kept_past_its_call_is_an_error() {
        let ctx = &mut with_forms();
        let kept: Arc<Mutex<Option<Form>>> = Arc::default();
        let store = kept.clone();
        ctx.defspecial("keep-form", move |form: Form| {
            *store.lock().unwrap() = Some(form);
        });
        ctx.defun(
            "run-kept",
            move |ctx: &mut TulispContext| -> Result<TulispObject, Error> {
                let form = kept.lock().unwrap().clone().unwrap();
                form.eval(ctx)
            },
        );
        let program = "(let ((x 1)) (keep-form (+ x 1))) (run-kept)";
        let got = ctx.eval_string(program).unwrap_err().format(ctx);
        assert!(
            got.contains("a form ran after its special form returned"),
            "{got}"
        );
    }

    #[test]
    fn closures_in_and_around_forms() {
        let ctx = &mut with_forms();
        eval_assert_equal(ctx, "(let ((y 3)) (funcall (my-progn (lambda () y))))", "3");
        let program = "(defun make (x) (lambda () (my-progn x)))
                       (let ((a (make 1)) (b (make 2))) (list (funcall a) (funcall b)))";
        eval_assert_equal(ctx, program, "'(1 2)");
    }

    #[test]
    fn recursion_through_a_form() {
        let ctx = &mut with_forms();
        let program = "(defun count-down (n)
                         (my-progn (if (= n 0) 'done (count-down (- n 1)))))
                       (count-down 5)";
        eval_assert_equal(ctx, program, "'done");
    }

    // A compile error inside a form is raised only when the form runs.
    #[test]
    fn a_compile_error_in_a_form_is_raised_when_it_runs() {
        let ctx = &mut with_forms();
        eval_assert_equal(ctx, "(progn (never (if)) 'fine)", "'fine");
        assert!(ctx.eval_string("(twice (if))").is_err());
    }

    #[test]
    fn arity_is_checked_where_the_form_is_used() {
        let ctx = &mut with_forms();
        eval_assert_error_line(ctx, "(twice)", "ERR ArityMismatch: Too few arguments");
        eval_assert_error_line(ctx, "(twice 1 2)", "ERR ArityMismatch: Too many arguments");
    }

    // A special form defined after the code that calls it compiled is
    // reached through the call fallback, which refuses it.
    #[test]
    fn a_special_form_defined_late_is_an_error() {
        let ctx = &mut TulispContext::new();
        ctx.eval_string("(defun g () (late-form 1))").unwrap();
        ctx.defspecial("late-form", |form: Form| form.source().clone());
        assert!(eval_to_string(ctx, "(g)").contains("invalid function: late-form"));
    }

    #[test]
    fn a_call_compiles_to_special_call() {
        let ctx = &mut with_forms();
        let listing = crate::test_utils::listing(ctx, "(my-or nil 1)");
        assert!(listing.contains("specialcall my-or 0 2"), "{listing}");
        assert!(!listing.contains("rustcall"), "{listing}");
    }

    #[cfg(feature = "etags")]
    #[test]
    fn a_special_form_gets_a_tags_entry() {
        let ctx = &mut TulispContext::new();
        ctx.defspecial("tagged-form", |form: Form| form.source().clone());
        assert!(
            ctx.tags_table
                .get(file!())
                .is_some_and(|tags| tags.contains_key("tagged-form"))
        );
    }

    // A special form is not a function, as in Emacs.
    #[test]
    fn a_special_form_is_not_a_function() {
        let ctx = &mut TulispContext::new();
        ctx.defspecial("quote-it", |form: Form| form.source().clone());
        let err = "ERR InvalidArgument: invalid function: quote-it";
        eval_assert_error_line(ctx, "(funcall 'quote-it 1)", err);
        eval_assert_error_line(ctx, "(apply 'quote-it '(1))", err);
        eval_assert_not(ctx, "(functionp 'quote-it)");
        let sym = ctx.intern("quote-it");
        let err = ctx.funcall(&sym, (1,)).unwrap_err();
        assert!(err.format(ctx).contains("invalid function: quote-it"));
    }

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

    // A special form gets its evaluated arguments before the call, and
    // runs its forms when the closure asks for them.
    #[test]
    fn a_special_form_runs_its_forms_when_asked() {
        let ctx = &mut TulispContext::new();
        ctx.defspecial(
            "add-twice",
            |ctx: &mut TulispContext,
             n: i64,
             form: crate::Form|
             -> Result<crate::TulispObject, crate::Error> {
                form.eval(ctx)?;
                Ok((n + form.eval_into::<i64>(ctx)?).into())
            },
        );
        let program = "(defvar k 0) (setq k 0) (list (add-twice (+ 1 2) (setq k (+ k 1))) k)";
        let got = ctx.eval_string(program).unwrap();
        assert_eq!(got.to_string(), "(5 2)");
    }
}
