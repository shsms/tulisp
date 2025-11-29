use crate::{Error, TulispContext, TulispObject, destruct_eval_bind};

pub trait TulispCallable<
    Args: 'static,
    Output: 'static,
    const NEEDS_CONTEXT: bool,
    const NUM_ARGS: usize,
    const NUM_OPTIONAL: usize,
    const HAS_REST: bool,
    const FALLIBLE: bool,
>
{
    fn add_to_context(self, ctx: &mut TulispContext, name: &str);
}

impl<OutT, FnT> TulispCallable<(), OutT, true, 0, 0, false, true> for FnT
where
    FnT: for<'a> Fn(&'a mut TulispContext) -> Result<OutT, Error> + 'static,
    OutT: Into<TulispObject> + 'static,
{
    fn add_to_context(self, ctx: &mut TulispContext, name: &str) {
        ctx.add_special_form(name, move |ctx, _args| {
            let res = (self)(ctx);
            res.map(|o| o.into())
        });
    }
}

impl<OutT, FnT> TulispCallable<(), OutT, true, 0, 0, false, false> for FnT
where
    FnT: for<'a> Fn(&'a mut TulispContext) -> OutT + 'static,
    OutT: Into<TulispObject> + 'static,
{
    fn add_to_context(self, ctx: &mut TulispContext, name: &str) {
        ctx.add_special_form(name, move |ctx, _args| {
            let res = (self)(ctx);
            Ok(res.into())
        });
    }
}

impl<OutT, FnT> TulispCallable<(), OutT, false, 0, 0, false, true> for FnT
where
    FnT: for<'a> Fn() -> Result<OutT, Error> + 'static,
    OutT: Into<TulispObject> + 'static,
{
    fn add_to_context(self, ctx: &mut TulispContext, name: &str) {
        ctx.add_special_form(name, move |_ctx, _args| {
            let res = (self)();
            res.map(|o| o.into())
        });
    }
}

impl<OutT, FnT> TulispCallable<(), OutT, false, 0, 0, false, false> for FnT
where
    FnT: for<'a> Fn() -> OutT + 'static,
    OutT: Into<TulispObject> + 'static,
{
    fn add_to_context(self, ctx: &mut TulispContext, name: &str) {
        ctx.add_special_form(name, move |_ctx, _args| {
            let res = (self)();
            Ok(res.into())
        });
    }
}

// Implementations for functions with one parameter
impl<P1, OutT, FnT> TulispCallable<(P1,), OutT, true, 1, 0, false, true> for FnT
where
    FnT: for<'a> Fn(&'a mut TulispContext, P1) -> Result<OutT, Error> + 'static,
    P1: TryFrom<TulispObject, Error = Error> + 'static,
    OutT: Into<TulispObject> + 'static,
{
    fn add_to_context(self, ctx: &mut TulispContext, name: &str) {
        ctx.add_special_form(name, move |ctx, args| {
            destruct_eval_bind!(ctx, (arg1) = args);
            let res = (self)(ctx, arg1.try_into()?);
            res.map(|o| o.into())
        });
    }
}

impl<P1, OutT, FnT> TulispCallable<(P1,), OutT, true, 1, 0, false, false> for FnT
where
    FnT: for<'a> Fn(&'a mut TulispContext, P1) -> OutT + 'static,
    P1: TryFrom<TulispObject, Error = Error> + 'static,
    OutT: Into<TulispObject> + 'static,
{
    fn add_to_context(self, ctx: &mut TulispContext, name: &str) {
        ctx.add_special_form(name, move |ctx, args| {
            destruct_eval_bind!(ctx, (arg1) = args);
            let res = (self)(ctx, arg1.try_into()?);
            Ok(res.into())
        });
    }
}

impl<P1, OutT, FnT> TulispCallable<(P1,), OutT, false, 1, 0, false, true> for FnT
where
    FnT: for<'a> Fn(P1) -> Result<OutT, Error> + 'static,
    P1: TryFrom<TulispObject, Error = Error> + 'static,
    OutT: Into<TulispObject> + 'static,
{
    fn add_to_context(self, ctx: &mut TulispContext, name: &str) {
        ctx.add_special_form(name, move |ctx, args| {
            destruct_eval_bind!(ctx, (arg1) = args);
            let res = (self)(arg1.try_into()?);
            res.map(|o| o.into())
        });
    }
}

impl<P1, OutT, FnT> TulispCallable<(P1,), OutT, false, 1, 0, false, false> for FnT
where
    FnT: for<'a> Fn(P1) -> OutT + 'static,
    P1: TryFrom<TulispObject, Error = Error> + 'static,
    OutT: Into<TulispObject> + 'static,
{
    fn add_to_context(self, ctx: &mut TulispContext, name: &str) {
        ctx.add_special_form(name, move |ctx, args| {
            destruct_eval_bind!(ctx, (arg1) = args);
            let res = (self)(arg1.try_into()?);
            Ok(res.into())
        });
    }
}

// Implementations for functions with two parameters
impl<P1, P2, OutT, FnT> TulispCallable<(P1, P2), OutT, true, 2, 0, false, true> for FnT
where
    FnT: for<'a> Fn(&'a mut TulispContext, P1, P2) -> Result<OutT, Error> + 'static,
    P1: TryFrom<TulispObject, Error = Error> + 'static,
    P2: TryFrom<TulispObject, Error = Error> + 'static,
    OutT: Into<TulispObject> + 'static,
{
    fn add_to_context(self, ctx: &mut TulispContext, name: &str) {
        ctx.add_special_form(name, move |ctx, args| {
            destruct_eval_bind!(ctx, (arg1 arg2) = args);
            let res = (self)(ctx, arg1.try_into()?, arg2.try_into()?);
            res.map(|o| o.into())
        });
    }
}

impl<P1, P2, OutT, FnT> TulispCallable<(P1, P2), OutT, true, 2, 0, false, false> for FnT
where
    FnT: for<'a> Fn(&'a mut TulispContext, P1, P2) -> OutT + 'static,
    P1: TryFrom<TulispObject, Error = Error> + 'static,
    P2: TryFrom<TulispObject, Error = Error> + 'static,
    OutT: Into<TulispObject> + 'static,
{
    fn add_to_context(self, ctx: &mut TulispContext, name: &str) {
        ctx.add_special_form(name, move |ctx, args| {
            destruct_eval_bind!(ctx, (arg1 arg2) = args);
            let res = (self)(ctx, arg1.try_into()?, arg2.try_into()?);
            Ok(res.into())
        });
    }
}

impl<P1, P2, OutT, FnT> TulispCallable<(P1, P2), OutT, false, 2, 0, false, true> for FnT
where
    FnT: for<'a> Fn(P1, P2) -> Result<OutT, Error> + 'static,
    P1: TryFrom<TulispObject, Error = Error> + 'static,
    P2: TryFrom<TulispObject, Error = Error> + 'static,
    OutT: Into<TulispObject> + 'static,
{
    fn add_to_context(self, ctx: &mut TulispContext, name: &str) {
        ctx.add_special_form(name, move |ctx, args| {
            destruct_eval_bind!(ctx, (arg1 arg2) = args);
            let res = (self)(arg1.try_into()?, arg2.try_into()?);
            res.map(|o| o.into())
        });
    }
}

impl<P1, P2, OutT, FnT> TulispCallable<(P1, P2), OutT, false, 2, 0, false, false> for FnT
where
    FnT: for<'a> Fn(P1, P2) -> OutT + 'static,
    P1: TryFrom<TulispObject, Error = Error> + 'static,
    P2: TryFrom<TulispObject, Error = Error> + 'static,
    OutT: Into<TulispObject> + 'static,
{
    fn add_to_context(self, ctx: &mut TulispContext, name: &str) {
        ctx.add_special_form(name, move |ctx, args| {
            destruct_eval_bind!(ctx, (arg1 arg2) = args);
            let res = (self)(arg1.try_into()?, arg2.try_into()?);
            Ok(res.into())
        });
    }
}
