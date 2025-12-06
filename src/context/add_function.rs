use crate::{Error, Rest, TulispContext, TulispObject, destruct_bind, destruct_eval_bind};

pub trait TulispCallable<
    Args: 'static,
    Output: 'static,
    const NEEDS_CONTEXT: bool,
    const NUM_ARGS: usize,
    const NUM_OPTIONAL: usize,
    const HAS_REST: bool,
    const HAS_RETURN: bool,
    const FALLIBLE: bool,
>
{
    fn add_to_context(self, ctx: &mut TulispContext, name: &str);
}

macro_rules! impl_tulisp_callable {
    (
        args: $args_count:literal: ($($arg: ident),*),
        opts: $opts_count:literal: ($($opt: ident),*) $(,)?
    ) => {

        // Without context, infallible, no rest
        #[allow(nonstandard_style)]
        impl<OutT, FnT, $($arg,)* $($opt,)*>
        TulispCallable<($($arg,)* $($opt,)*) , OutT, false, $args_count, $opts_count, false, true, false> for FnT
        where
        FnT: Fn($($arg,)* $(Option<$opt>),*) -> OutT + 'static,
        $($arg: TryFrom<TulispObject, Error = Error> + 'static,)*
        $($opt: TryFrom<TulispObject, Error = Error> + 'static,)*
        OutT: Into<TulispObject> + 'static,
        {
            fn add_to_context(
                self,
                ctx: &mut TulispContext,
                name: &str,
            ) {
                ctx.add_special_form(
                    name,
                    move |_ctx, _args| {
                        impl_tulisp_callable!(@deb _ctx, $($arg)* &optional $($opt)*, _args);
                        let res = (self)($($arg.try_into()?,)* $(if $opt.null() {None} else {Some($opt.try_into()?)}),*);
                        Ok(res.into())
                    }
                );
            }
        }

        // Without context, no return, no rest
        #[allow(nonstandard_style)]
        impl<FnT, $($arg,)* $($opt,)*>
        TulispCallable<($($arg,)* $($opt,)*) , (), false, $args_count, $opts_count, false, false, false> for FnT
        where
        FnT: Fn($($arg,)* $(Option<$opt>),*) + 'static,
        $($arg: TryFrom<TulispObject, Error = Error> + 'static,)*
        $($opt: TryFrom<TulispObject, Error = Error> + 'static,)*
        {
            fn add_to_context(
                self,
                ctx: &mut TulispContext,
                name: &str,
            ) {
                ctx.add_special_form(
                    name,
                    move |_ctx, _args| {
                        impl_tulisp_callable!(@deb _ctx, $($arg)* &optional $($opt)*, _args);
                        (self)($($arg.try_into()?,)* $(if $opt.null() {None} else {Some($opt.try_into()?)}),*);
                        Ok(TulispObject::nil())
                    }
                );
            }
        }

        // With context, infallible, no rest
        #[allow(nonstandard_style)]
        impl<OutT, FnT, $($arg,)* $($opt,)*>
        TulispCallable<($($arg,)* $($opt,)*) , OutT, true, $args_count, $opts_count, false, true,false> for FnT
        where
        FnT: Fn(&mut TulispContext, $($arg,)* $(Option<$opt>),*) -> OutT + 'static,
        $($arg: TryFrom<TulispObject, Error = Error> + 'static,)*
        $($opt: TryFrom<TulispObject, Error = Error> + 'static,)*
        OutT: Into<TulispObject> + 'static,
        {
            fn add_to_context(
                self,
                ctx: &mut TulispContext,
                name: &str,
            ) {
                ctx.add_special_form(
                    name,
                    move |ctx, _args| {
                        impl_tulisp_callable!(@deb ctx, $($arg)* &optional $($opt)*, _args);
                        let res = (self)(ctx, $($arg.try_into()?,)* $(if $opt.null() {None} else {Some($opt.try_into()?)}),*);
                        Ok(res.into())
                    }
                );
            }
        }

        // With context, no return, no rest
        #[allow(nonstandard_style)]
        impl<FnT, $($arg,)* $($opt,)*>
        TulispCallable<($($arg,)* $($opt,)*), (), true, $args_count, $opts_count, false, false,false> for FnT
        where
        FnT: Fn(&mut TulispContext, $($arg,)* $(Option<$opt>),*) + 'static,
        $($arg: TryFrom<TulispObject, Error = Error> + 'static,)*
        $($opt: TryFrom<TulispObject, Error = Error> + 'static,)*
        {
            fn add_to_context(
                self,
                ctx: &mut TulispContext,
                name: &str,
            ) {
                ctx.add_special_form(
                    name,
                    move |ctx, _args| {
                        impl_tulisp_callable!(@deb ctx, $($arg)* &optional $($opt)*, _args);
                        (self)(ctx, $($arg.try_into()?,)* $(if $opt.null() {None} else {Some($opt.try_into()?)}),*);
                        Ok(TulispObject::nil())
                    }
                );
            }
        }

        // Without context, fallible, no rest
        #[allow(nonstandard_style)]
        impl<OutT, FnT, $($arg,)* $($opt,)*>
        TulispCallable<($($arg,)* $($opt,)*) , OutT, false, $args_count, $opts_count, false, true, true> for FnT
        where
        FnT: Fn($($arg,)* $(Option<$opt>),*) -> Result<OutT, Error> + 'static,
        $($arg: TryFrom<TulispObject, Error = Error> + 'static,)*
        $($opt: TryFrom<TulispObject, Error = Error> + 'static,)*
        OutT: Into<TulispObject> + 'static,
        {
            fn add_to_context(
                self,
                ctx: &mut TulispContext,
                name: &str,
            ) {
                ctx.add_special_form(
                    name,
                    move |_ctx, _args| {
                        impl_tulisp_callable!(@deb _ctx, $($arg)* &optional $($opt)*, _args);
                        let res = (self)($($arg.try_into()?,)* $(if $opt.null() {None} else {Some($opt.try_into()?)}),*)?;
                        Ok(res.into())
                    }
                );
            }
        }

        // With context, fallible, no rest
        #[allow(nonstandard_style)]
        impl<OutT, FnT, $($arg,)* $($opt,)*>
        TulispCallable<($($arg,)* $($opt,)*) , OutT, true, $args_count, $opts_count, false, true, true> for FnT
        where
        FnT: Fn(&mut TulispContext, $($arg,)* $(Option<$opt>),*) -> Result<OutT, Error> + 'static,
        $($arg: TryFrom<TulispObject, Error = Error> + 'static,)*
        $($opt: TryFrom<TulispObject, Error = Error> + 'static,)*
        OutT: Into<TulispObject> + 'static,
        {
            fn add_to_context(
                self,
                ctx: &mut TulispContext,
                name: &str,
            ) {
                ctx.add_special_form(
                    name,
                    move |ctx, _args| {
                        impl_tulisp_callable!(@deb ctx, $($arg)* &optional $($opt)*, _args);
                        let res = (self)(ctx, $($arg.try_into()?,)* $(if $opt.null() {None} else {Some($opt.try_into()?)}),*)?;
                        Ok(res.into())
                    }
                );
            }
        }

        // Without context, infallible, with rest
        #[allow(nonstandard_style)]
        impl<RestT, OutT, FnT, $($arg,)* $($opt,)*>
        TulispCallable<($($arg,)* $($opt,)* RestT,), OutT, false, $args_count, $opts_count, true, true, false> for FnT
        where
        FnT: Fn($($arg,)* $(Option<$opt>,)* Rest<RestT>) -> OutT + 'static,
        $($arg: TryFrom<TulispObject, Error = Error> + 'static,)*
        $($opt: TryFrom<TulispObject, Error = Error> + 'static,)*
        RestT: TryFrom<TulispObject, Error = Error> + Into<TulispObject> + 'static,
        OutT: Into<TulispObject> + 'static,
        {
            fn add_to_context(
                self,
                ctx: &mut TulispContext,
                name: &str,
            ) {
                ctx.add_special_form(
                    name,
                    move |_ctx, _args| {
                        impl_tulisp_callable!(@db $($arg)* &optional $($opt)* &rest rest, _args);
                        $(let $arg = _ctx.eval(&$arg)?;)*
                        $(let $opt = _ctx.eval(&$opt)?;)*
                        let rest = rest
                            .base_iter()
                            .map(|arg| _ctx.eval(&arg)?.try_into())
                            .collect::<Result<_, _>>()?;
                        let res = (self)(
                            $($arg.try_into()?,)*
                            $(if $opt.null() {None} else {Some($opt.try_into()?)},)*
                            rest
                        );
                        Ok(res.into())
                    }
                );
            }
        }

        // Without context, no return, with rest
        #[allow(nonstandard_style)]
        impl<RestT, FnT, $($arg,)* $($opt,)*>
        TulispCallable<($($arg,)* $($opt,)* RestT,), (), false, $args_count, $opts_count, true, false, false> for FnT
        where
        FnT: Fn($($arg,)* $(Option<$opt>,)* Rest<RestT>) + 'static,
        $($arg: TryFrom<TulispObject, Error = Error> + 'static,)*
        $($opt: TryFrom<TulispObject, Error = Error> + 'static,)*
        RestT: TryFrom<TulispObject, Error = Error> + Into<TulispObject> + 'static,
        {
            fn add_to_context(
                self,
                ctx: &mut TulispContext,
                name: &str,
            ) {
                ctx.add_special_form(
                    name,
                    move |_ctx, _args| {
                        impl_tulisp_callable!(@db $($arg)* &optional $($opt)* &rest rest, _args);
                        $(let $arg = _ctx.eval(&$arg)?;)*
                        $(let $opt = _ctx.eval(&$opt)?;)*
                        let rest = rest
                            .base_iter()
                            .map(|arg| _ctx.eval(&arg)?.try_into())
                            .collect::<Result<_, _>>()?;
                        (self)(
                            $($arg.try_into()?,)*
                            $(if $opt.null() {None} else {Some($opt.try_into()?)},)*
                            rest
                        );
                        Ok(TulispObject::nil())
                    }
                );
            }
        }

        // With context, infallible, with rest
        #[allow(nonstandard_style)]
        impl<RestT, OutT, FnT, $($arg,)* $($opt,)*>
        TulispCallable<($($arg,)* $($opt,)* RestT,), OutT, true, $args_count, $opts_count, true, true, false> for FnT
        where
        FnT: Fn(&mut TulispContext, $($arg,)* $(Option<$opt>,)* Rest<RestT>) -> OutT + 'static,
        $($arg: TryFrom<TulispObject, Error = Error> + 'static,)*
        $($opt: TryFrom<TulispObject, Error = Error> + 'static,)*
        RestT: TryFrom<TulispObject, Error = Error> + Into<TulispObject> + 'static,
        OutT: Into<TulispObject> + 'static,
        {
            fn add_to_context(
                self,
                ctx: &mut TulispContext,
                name: &str,
            ) {
                ctx.add_special_form(
                    name,
                    move |ctx, _args| {
                        impl_tulisp_callable!(@db $($arg)* &optional $($opt)* &rest rest, _args);
                        $(let $arg = ctx.eval(&$arg)?;)*
                        $(let $opt = ctx.eval(&$opt)?;)*
                        let rest = rest
                            .base_iter()
                            .map(|arg| ctx.eval(&arg)?.try_into())
                            .collect::<Result<_, _>>()?;
                        let res = (self)(
                            ctx,
                            $($arg.try_into()?,)*
                            $(if $opt.null() {None} else {Some($opt.try_into()?)},)*
                            rest
                        );
                        Ok(res.into())
                    }
                );
            }
        }

        // With context, no return, with rest
        #[allow(nonstandard_style)]
        impl<RestT, FnT, $($arg,)* $($opt,)*>
        TulispCallable<($($arg,)* $($opt,)* RestT,), (), true, $args_count, $opts_count, true, false, false> for FnT
        where
        FnT: Fn(&mut TulispContext, $($arg,)* $(Option<$opt>,)* Rest<RestT>) + 'static,
        $($arg: TryFrom<TulispObject, Error = Error> + 'static,)*
        $($opt: TryFrom<TulispObject, Error = Error> + 'static,)*
        RestT: TryFrom<TulispObject, Error = Error> + Into<TulispObject> + 'static,
        {
            fn add_to_context(
                self,
                ctx: &mut TulispContext,
                name: &str,
            ) {
                ctx.add_special_form(
                    name,
                    move |ctx, _args| {
                        impl_tulisp_callable!(@db $($arg)* &optional $($opt)* &rest rest, _args);
                        $(let $arg = ctx.eval(&$arg)?;)*
                        $(let $opt = ctx.eval(&$opt)?;)*
                        let rest = rest
                            .base_iter()
                            .map(|arg| ctx.eval(&arg)?.try_into())
                            .collect::<Result<_, _>>()?;
                        (self)(
                            ctx,
                            $($arg.try_into()?,)*
                            $(if $opt.null() {None} else {Some($opt.try_into()?)},)*
                            rest
                        );
                        Ok(TulispObject::nil())
                    }
                );
            }
        }

        // Without context, fallible, with rest
        #[allow(nonstandard_style)]
        impl<RestT, OutT, FnT, $($arg,)* $($opt,)*>
        TulispCallable<($($arg,)* $($opt,)* RestT,), OutT, false, $args_count, $opts_count, true, true, true> for FnT
        where
        FnT: Fn($($arg,)* $(Option<$opt>,)* Rest<RestT>) -> Result<OutT, Error> + 'static,
        $($arg: TryFrom<TulispObject, Error = Error> + 'static,)*
        $($opt: TryFrom<TulispObject, Error = Error> + 'static,)*
        RestT: TryFrom<TulispObject, Error = Error> + Into<TulispObject> + 'static,
        OutT: Into<TulispObject> + 'static,
        {
            fn add_to_context(
                self,
                ctx: &mut TulispContext,
                name: &str,
            ) {
                ctx.add_special_form(
                    name,
                    move |_ctx, _args| {
                        impl_tulisp_callable!(@db $($arg)* &optional $($opt)* &rest rest, _args);
                        $(let $arg = _ctx.eval(&$arg)?;)*
                        $(let $opt = _ctx.eval(&$opt)?;)*
                        let rest = rest
                            .base_iter()
                            .map(|arg| _ctx.eval(&arg)?.try_into())
                            .collect::<Result<_, _>>()?;
                        let res = (self)(
                            $($arg.try_into()?,)*
                            $(if $opt.null() {None} else {Some($opt.try_into()?)},)*
                            rest
                        )?;
                        Ok(res.into())
                    }
                );
            }
        }

        // With context, fallible, with rest
        #[allow(nonstandard_style)]
        impl<RestT, OutT, FnT, $($arg,)* $($opt,)*>
        TulispCallable<($($arg,)* $($opt,)* RestT,), OutT, true, $args_count, $opts_count, true, true, true> for FnT
        where
        FnT: Fn(&mut TulispContext, $($arg,)* $(Option<$opt>,)* Rest<RestT>) -> Result<OutT, Error> + 'static,
        $($arg: TryFrom<TulispObject, Error = Error> + 'static,)*
        $($opt: TryFrom<TulispObject, Error = Error> + 'static,)*
        RestT: TryFrom<TulispObject, Error = Error> + Into<TulispObject> + 'static,
        OutT: Into<TulispObject> + 'static,
        {
            fn add_to_context(
                self,
                ctx: &mut TulispContext,
                name: &str,
            ) {
                ctx.add_special_form(
                    name,
                    move |ctx, _args| {
                        impl_tulisp_callable!(@db $($arg)* &optional $($opt)* &rest rest, _args);
                        $(let $arg = ctx.eval(&$arg)?;)*
                        $(let $opt = ctx.eval(&$opt)?;)*
                        let rest = rest
                            .base_iter()
                            .map(|arg| ctx.eval(&arg)?.try_into())
                            .collect::<Result<_, _>>()?;
                        let res = (self)(
                            ctx,
                            $($arg.try_into()?,)*
                            $(if $opt.null() {None} else {Some($opt.try_into()?)},)*
                            rest
                        )?;
                        Ok(res.into())
                    }
                );
            }
        }
    };

    (@deb $ctx:ident, $($arg_name:ident)+ &optional, $args:ident) => {
        destruct_eval_bind!($ctx, ($($arg_name)+) = $args);
    };
    (@deb $ctx:ident, &optional $($opt_name:ident)+, $args:ident) => {
        destruct_eval_bind!($ctx, (&optional $($opt_name)+) = $args);
    };
    (@deb $ctx:ident, $($arg_name:ident)+ &optional $($opt_name:ident)+, $args:ident) => {
        destruct_eval_bind!($ctx, ($($arg_name)+ &optional $($opt_name)+) = $args);
    };
    (@deb $ctx:ident, &optional, $args:ident) => {};

    (@db $($arg_name:ident)+ &optional &rest $rest:ident, $args:ident) => {
        destruct_bind!(($($arg_name)+ &rest $rest) = $args);
    };
    (@db &optional $($opt_name:ident)+ &rest $rest:ident, $args:ident) => {
        destruct_bind!((&optional $($opt_name)+ &rest $rest) = $args);
    };
    (@db $($arg_name:ident)+ &optional $($opt_name:ident)+ &rest $rest:ident, $args:ident) => {
        destruct_bind!(($($arg_name)+ &optional $($opt_name)+ &rest $rest) = $args);
    };
    (@db &optional &rest $rest:ident, $args:ident) => {
        destruct_bind!((&rest $rest) = $args);
    };
}

#[cfg(feature = "big_functions")]
mod upto_10_args {
    use super::*;
    impl_tulisp_callable!(args: 0: (), opts: 10: (A, B, C, D, E, F, G, H, I, J),);
    impl_tulisp_callable!(args: 1: (A), opts: 9: (B, C, D, E, F, G, H, I, J),);
    impl_tulisp_callable!(args: 2: (A, B), opts: 8: (C, D, E, F, G, H, I, J),);
    impl_tulisp_callable!(args: 3: (A, B, C), opts: 7: (D, E, F, G, H, I, J),);
    impl_tulisp_callable!(args: 4: (A, B, C, D), opts: 6: (E, F, G, H, I, J),);
    impl_tulisp_callable!(args: 5: (A, B, C, D, E), opts: 5: (F, G, H, I, J),);
    impl_tulisp_callable!(args: 6: (A, B, C, D, E, F), opts: 4: (G, H, I, J),);
    impl_tulisp_callable!(args: 7: (A, B, C, D, E, F, G), opts: 3: (H, I, J),);
    impl_tulisp_callable!(args: 8: (A, B, C, D, E, F, G, H), opts: 2: (I, J),);
    impl_tulisp_callable!(args: 9: (A, B, C, D, E, F, G, H, I), opts: 1: (J),);
    impl_tulisp_callable!(args: 10: (A, B, C, D, E, F, G, H, I, J), opts: 0: (),);

    impl_tulisp_callable!(args: 0: (), opts: 9: (A, B, C, D, E, F, G, H, I),);
    impl_tulisp_callable!(args: 1: (A), opts: 8: (B, C, D, E, F, G, H, I),);
    impl_tulisp_callable!(args: 2: (A, B), opts: 7: (C, D, E, F, G, H, I),);
    impl_tulisp_callable!(args: 3: (A, B, C), opts: 6: (D, E, F, G, H, I),);
    impl_tulisp_callable!(args: 4: (A, B, C, D), opts: 5: (E, F, G, H, I),);
    impl_tulisp_callable!(args: 5: (A, B, C, D, E), opts: 4: (F, G, H, I),);
    impl_tulisp_callable!(args: 6: (A, B, C, D, E, F), opts: 3: (G, H, I),);
    impl_tulisp_callable!(args: 7: (A, B, C, D, E, F, G), opts: 2: (H, I),);
    impl_tulisp_callable!(args: 8: (A, B, C, D, E, F, G, H), opts: 1: (I),);
    impl_tulisp_callable!(args: 9: (A, B, C, D, E, F, G, H, I), opts: 0: (),);

    impl_tulisp_callable!(args: 0: (), opts: 8: (A, B, C, D, E, F, G, H),);
    impl_tulisp_callable!(args: 1: (A), opts: 7: (B, C, D, E, F, G, H),);
    impl_tulisp_callable!(args: 2: (A, B), opts: 6: (C, D, E, F, G, H),);
    impl_tulisp_callable!(args: 3: (A, B, C), opts: 5: (D, E, F, G, H),);
    impl_tulisp_callable!(args: 4: (A, B, C, D), opts: 4: (E, F, G, H),);
    impl_tulisp_callable!(args: 5: (A, B, C, D, E), opts: 3: (F, G, H),);
    impl_tulisp_callable!(args: 6: (A, B, C, D, E, F), opts: 2: (G, H),);
    impl_tulisp_callable!(args: 7: (A, B, C, D, E, F, G), opts: 1: (H),);
    impl_tulisp_callable!(args: 8: (A, B, C, D, E, F, G, H), opts: 0: (),);

    impl_tulisp_callable!(args: 0: (), opts: 7: (A, B, C, D, E, F, G),);
    impl_tulisp_callable!(args: 1: (A), opts: 6: (B, C, D, E, F, G),);
    impl_tulisp_callable!(args: 2: (A, B), opts: 5: (C, D, E, F, G),);
    impl_tulisp_callable!(args: 3: (A, B, C), opts: 4: (D, E, F, G),);
    impl_tulisp_callable!(args: 4: (A, B, C, D), opts: 3: (E, F, G),);
    impl_tulisp_callable!(args: 5: (A, B, C, D, E), opts: 2: (F, G),);
    impl_tulisp_callable!(args: 6: (A, B, C, D, E, F), opts: 1: (G),);
    impl_tulisp_callable!(args: 7: (A, B, C, D, E, F, G), opts: 0: (),);

    impl_tulisp_callable!(args: 0: (), opts: 6: (A, B, C, D, E, F),);
    impl_tulisp_callable!(args: 1: (A), opts: 5: (B, C, D, E, F),);
    impl_tulisp_callable!(args: 2: (A, B), opts: 4: (C, D, E, F),);
    impl_tulisp_callable!(args: 3: (A, B, C), opts: 3: (D, E, F),);
    impl_tulisp_callable!(args: 4: (A, B, C, D), opts: 2: (E, F),);
    impl_tulisp_callable!(args: 5: (A, B, C, D, E), opts: 1: (F),);
    impl_tulisp_callable!(args: 6: (A, B, C, D, E, F), opts: 0: (),);
}

mod upto_5_args {
    use super::*;

    impl_tulisp_callable!(args: 0: (), opts: 5: (A, B, C, D, E),);
    impl_tulisp_callable!(args: 1: (A), opts: 4: (B, C, D, E),);
    impl_tulisp_callable!(args: 2: (A, B), opts: 3: (C, D, E),);
    impl_tulisp_callable!(args: 3: (A, B, C), opts: 2: (D, E),);
    impl_tulisp_callable!(args: 4: (A, B, C, D), opts: 1: (E),);
    impl_tulisp_callable!(args: 5: (A, B, C, D, E), opts: 0: (),);

    impl_tulisp_callable!(args: 0: (), opts: 4: (A, B, C, D),);
    impl_tulisp_callable!(args: 1: (A), opts: 3: (B, C, D),);
    impl_tulisp_callable!(args: 2: (A, B), opts: 2: (C, D),);
    impl_tulisp_callable!(args: 3: (A, B, C), opts: 1: (D),);
    impl_tulisp_callable!(args: 4: (A, B, C, D), opts: 0: (),);

    impl_tulisp_callable!(args: 0: (), opts: 3: (A, B, C),);
    impl_tulisp_callable!(args: 1: (A), opts: 2: (B, C),);
    impl_tulisp_callable!(args: 2: (A, B), opts: 1: (C),);
    impl_tulisp_callable!(args: 3: (A, B, C), opts: 0: (),);

    impl_tulisp_callable!(args: 0: (), opts: 2: (A, B),);
    impl_tulisp_callable!(args: 1: (A), opts: 1: (B),);
    impl_tulisp_callable!(args: 2: (A, B), opts: 0: (),);

    impl_tulisp_callable!(args: 0: (), opts: 1: (A),);
    impl_tulisp_callable!(args: 1: (A), opts: 0: (),);

    impl_tulisp_callable!(args: 0: (), opts: 0: (),);
}

#[cfg(test)]
mod tests {
    use crate::test_utils::{eval_assert_equal, eval_assert_error};
    use crate::{Error, ErrorKind, Rest, TulispContext, TulispObject};

    #[test]
    fn test_add_functions_only_rest() -> Result<(), crate::Error> {
        let ctx = &mut TulispContext::new();
        ctx.add_function("sum", |items: Rest<f64>| -> f64 { items.into_iter().sum() });

        eval_assert_equal(ctx, "(sum)", "0.0");
        eval_assert_equal(ctx, "(sum (- 4 0.5) (+ 3 4) 5 10)", "25.5");
        eval_assert_error(
            ctx,
            r#"(sum "hh" 10)"#,
            r#"ERR TypeMismatch: Expected number, got: String { value: "hh" }
<eval_string>:1.7-1.10:  at "hh"
<eval_string>:1.1-1.14:  at (sum "hh" 10)
"#,
        );

        ctx.add_function(
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
                Err(crate::Error::new(
                    ErrorKind::InvalidArgument,
                    "No cats found".into(),
                ))
            },
        );

        eval_assert_equal(ctx, r#"(let ((a "stop"))(cats "dog" a "cat"))"#, "nil");
        eval_assert_error(
            ctx,
            r#"(let ((horse "horse"))(cats horse))"#,
            r#"ERR InvalidArgument: No cats found
<eval_string>:1.23-1.35:  at (cats horse)
<eval_string>:1.1-1.36:  at (let ((horse "horse")) (cats horse))
"#,
        );

        Ok(())
    }

    #[test]
    fn test_add_functions_only_args() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();

        ctx.add_function("add_round", |a: f64, b: f64| -> i64 {
            (a + b).round() as i64
        });
        eval_assert_equal(ctx, "(let ((a 3.5) (b 4.2))(add_round a b))", "8");

        ctx.add_function("greet", |name: String| format!("Hello, {}!", name));
        eval_assert_equal(ctx, r#"(greet "Alice")"#, r#""Hello, Alice!""#);
        eval_assert_error(
            ctx,
            r#"(greet "Alice" "Peter")"#,
            r#"ERR TypeMismatch: Too many arguments
<eval_string>:1.1-1.24:  at (greet "Alice" "Peter")
"#,
        );

        Ok(())
    }

    #[test]
    fn test_add_functions_args_and_rest() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();

        ctx.add_function(
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

        ctx.add_function("power", |base: f64, exponent: Option<f64>| -> f64 {
            let exp = exponent.unwrap_or(2.0);
            base.powf(exp)
        });

        eval_assert_equal(ctx, "(power 3)", "9.0");
        eval_assert_equal(ctx, "(power 2 3)", "8.0");
        eval_assert_error(
            ctx,
            "(power 2 3 4)",
            r#"ERR TypeMismatch: Too many arguments
<eval_string>:1.1-1.14:  at (power 2 3 4)
"#,
        );
        Ok(())
    }

    #[test]
    fn test_add_functions_args_optional_and_rest() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();

        ctx.add_function(
            "build_url",
            |base: String, path: Option<String>, query_params: Rest<String>| -> String {
                let mut url = base;
                if let Some(p) = path {
                    url.push('/');
                    url.push_str(&p);
                }
                let joined = query_params.into_iter().collect::<Vec<_>>().join("&");
                if !joined.is_empty() {
                    url.push_str("?");
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
}
