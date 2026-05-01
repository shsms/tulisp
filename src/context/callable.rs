use crate::object::wrappers::generic::SyncSend;
use crate::{Error, Rest, TulispContext, TulispObject};

pub trait TulispCallable<
    Args: 'static,
    Output: 'static,
    const NEEDS_CONTEXT: bool,
    const NUM_ARGS: usize,
    const NUM_OPTIONAL: usize,
    const HAS_PLIST: bool,
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
        TulispCallable<($($arg,)* $($opt,)*) , OutT, false, $args_count, $opts_count, false, false, true, false> for FnT
        where
        FnT: Fn($($arg,)* $(Option<$opt>),*) -> OutT + 'static + SyncSend,
        $($arg: $crate::TulispConvertible + 'static,)*
        $($opt: $crate::TulispConvertible + 'static,)*
        OutT: $crate::TulispConvertible + 'static,
        {
            #[track_caller]
            fn add_to_context(
                self,
                ctx: &mut TulispContext,
                name: &str,
            ) {
                ctx.define_typed_defun(
                    name,
                    crate::value::DefunArity { required: $args_count, optional: $opts_count, has_rest: false },
                    move |_ctx, _args| {
                        impl_tulisp_callable!(@bind _args, $($arg)*, $($opt)*);
                        let res = (self)($($crate::TulispConvertible::from_tulisp($arg)?,)* $($opt,)*);
                        Ok($crate::TulispConvertible::into_tulisp(res))
                    }
                );
            }
        }

        // Without context, no return, no rest
        #[allow(nonstandard_style)]
        impl<FnT, $($arg,)* $($opt,)*>
        TulispCallable<($($arg,)* $($opt,)*) , (), false, $args_count, $opts_count, false, false, false, false> for FnT
        where
        FnT: Fn($($arg,)* $(Option<$opt>),*) + 'static + SyncSend,
        $($arg: $crate::TulispConvertible + 'static,)*
        $($opt: $crate::TulispConvertible + 'static,)*
        {
            #[track_caller]
            fn add_to_context(
                self,
                ctx: &mut TulispContext,
                name: &str,
            ) {
                ctx.define_typed_defun(
                    name,
                    crate::value::DefunArity { required: $args_count, optional: $opts_count, has_rest: false },
                    move |_ctx, _args| {
                        impl_tulisp_callable!(@bind _args, $($arg)*, $($opt)*);
                        (self)($($crate::TulispConvertible::from_tulisp($arg)?,)* $($opt,)*);
                        Ok(TulispObject::nil())
                    }
                );
            }
        }

        // With context, infallible, no rest
        #[allow(nonstandard_style)]
        impl<OutT, FnT, $($arg,)* $($opt,)*>
        TulispCallable<($($arg,)* $($opt,)*) , OutT, true, $args_count, $opts_count, false, false, true,false> for FnT
        where
        FnT: Fn(&mut TulispContext, $($arg,)* $(Option<$opt>),*) -> OutT + 'static + SyncSend,
        $($arg: $crate::TulispConvertible + 'static,)*
        $($opt: $crate::TulispConvertible + 'static,)*
        OutT: $crate::TulispConvertible + 'static,
        {
            #[track_caller]
            fn add_to_context(
                self,
                ctx: &mut TulispContext,
                name: &str,
            ) {
                ctx.define_typed_defun(
                    name,
                    crate::value::DefunArity { required: $args_count, optional: $opts_count, has_rest: false },
                    move |ctx, _args| {
                        impl_tulisp_callable!(@bind _args, $($arg)*, $($opt)*);
                        let res = (self)(ctx, $($crate::TulispConvertible::from_tulisp($arg)?,)* $($opt,)*);
                        Ok($crate::TulispConvertible::into_tulisp(res))
                    }
                );
            }
        }

        // With context, no return, no rest
        #[allow(nonstandard_style)]
        impl<FnT, $($arg,)* $($opt,)*>
        TulispCallable<($($arg,)* $($opt,)*), (), true, $args_count, $opts_count, false, false, false,false> for FnT
        where
        FnT: Fn(&mut TulispContext, $($arg,)* $(Option<$opt>),*) + 'static + SyncSend,
        $($arg: $crate::TulispConvertible + 'static,)*
        $($opt: $crate::TulispConvertible + 'static,)*
        {
            #[track_caller]
            fn add_to_context(
                self,
                ctx: &mut TulispContext,
                name: &str,
            ) {
                ctx.define_typed_defun(
                    name,
                    crate::value::DefunArity { required: $args_count, optional: $opts_count, has_rest: false },
                    move |ctx, _args| {
                        impl_tulisp_callable!(@bind _args, $($arg)*, $($opt)*);
                        (self)(ctx, $($crate::TulispConvertible::from_tulisp($arg)?,)* $($opt,)*);
                        Ok(TulispObject::nil())
                    }
                );
            }
        }

        // Without context, fallible, no rest
        #[allow(nonstandard_style)]
        impl<OutT, FnT, $($arg,)* $($opt,)*>
        TulispCallable<($($arg,)* $($opt,)*) , OutT, false, $args_count, $opts_count, false, false, true, true> for FnT
        where
        FnT: Fn($($arg,)* $(Option<$opt>),*) -> Result<OutT, Error> + 'static + SyncSend,
        $($arg: $crate::TulispConvertible + 'static,)*
        $($opt: $crate::TulispConvertible + 'static,)*
        OutT: $crate::TulispConvertible + 'static,
        {
            #[track_caller]
            fn add_to_context(
                self,
                ctx: &mut TulispContext,
                name: &str,
            ) {
                ctx.define_typed_defun(
                    name,
                    crate::value::DefunArity { required: $args_count, optional: $opts_count, has_rest: false },
                    move |_ctx, _args| {
                        impl_tulisp_callable!(@bind _args, $($arg)*, $($opt)*);
                        let res = (self)($($crate::TulispConvertible::from_tulisp($arg)?,)* $($opt,)*)?;
                        Ok($crate::TulispConvertible::into_tulisp(res))
                    }
                );
            }
        }

        // With context, fallible, no rest
        #[allow(nonstandard_style)]
        impl<OutT, FnT, $($arg,)* $($opt,)*>
        TulispCallable<($($arg,)* $($opt,)*) , OutT, true, $args_count, $opts_count, false, false, true, true> for FnT
        where
        FnT: Fn(&mut TulispContext, $($arg,)* $(Option<$opt>),*) -> Result<OutT, Error> + 'static + SyncSend,
        $($arg: $crate::TulispConvertible + 'static,)*
        $($opt: $crate::TulispConvertible + 'static,)*
        OutT: $crate::TulispConvertible + 'static,
        {
            #[track_caller]
            fn add_to_context(
                self,
                ctx: &mut TulispContext,
                name: &str,
            ) {
                ctx.define_typed_defun(
                    name,
                    crate::value::DefunArity { required: $args_count, optional: $opts_count, has_rest: false },
                    move |ctx, _args| {
                        impl_tulisp_callable!(@bind _args, $($arg)*, $($opt)*);
                        let res = (self)(ctx, $($crate::TulispConvertible::from_tulisp($arg)?,)* $($opt,)*)?;
                        Ok($crate::TulispConvertible::into_tulisp(res))
                    }
                );
            }
        }

        // Without context, infallible, with rest
        #[allow(nonstandard_style)]
        impl<RestT, OutT, FnT, $($arg,)* $($opt,)*>
        TulispCallable<($($arg,)* $($opt,)* RestT,), OutT, false, $args_count, $opts_count, false, true, true, false> for FnT
        where
        FnT: Fn($($arg,)* $(Option<$opt>,)* Rest<RestT>) -> OutT + 'static + SyncSend,
        $($arg: $crate::TulispConvertible + 'static,)*
        $($opt: $crate::TulispConvertible + 'static,)*
        RestT: $crate::TulispConvertible + 'static,
        OutT: $crate::TulispConvertible + 'static,
        {
            #[track_caller]
            fn add_to_context(
                self,
                ctx: &mut TulispContext,
                name: &str,
            ) {
                ctx.define_typed_defun(
                    name,
                    crate::value::DefunArity { required: $args_count, optional: $opts_count, has_rest: true },
                    move |_ctx, _args| {
                        impl_tulisp_callable!(@bind_rest _args, $($arg)*, $($opt)*, RestT, rest);
                        let res = (self)(
                            $($crate::TulispConvertible::from_tulisp($arg)?,)*
                            $($opt,)*
                            rest
                        );
                        Ok($crate::TulispConvertible::into_tulisp(res))
                    }
                );
            }
        }

        // Without context, no return, with rest
        #[allow(nonstandard_style)]
        impl<RestT, FnT, $($arg,)* $($opt,)*>
        TulispCallable<($($arg,)* $($opt,)* RestT,), (), false, $args_count, $opts_count, false, true, false, false> for FnT
        where
        FnT: Fn($($arg,)* $(Option<$opt>,)* Rest<RestT>) + 'static + SyncSend,
        $($arg: $crate::TulispConvertible + 'static,)*
        $($opt: $crate::TulispConvertible + 'static,)*
        RestT: $crate::TulispConvertible + 'static,
        {
            #[track_caller]
            fn add_to_context(
                self,
                ctx: &mut TulispContext,
                name: &str,
            ) {
                ctx.define_typed_defun(
                    name,
                    crate::value::DefunArity { required: $args_count, optional: $opts_count, has_rest: true },
                    move |_ctx, _args| {
                        impl_tulisp_callable!(@bind_rest _args, $($arg)*, $($opt)*, RestT, rest);
                        (self)(
                            $($crate::TulispConvertible::from_tulisp($arg)?,)*
                            $($opt,)*
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
        TulispCallable<($($arg,)* $($opt,)* RestT,), OutT, true, $args_count, $opts_count, false, true, true, false> for FnT
        where
        FnT: Fn(&mut TulispContext, $($arg,)* $(Option<$opt>,)* Rest<RestT>) -> OutT + 'static + SyncSend,
        $($arg: $crate::TulispConvertible + 'static,)*
        $($opt: $crate::TulispConvertible + 'static,)*
        RestT: $crate::TulispConvertible + 'static,
        OutT: $crate::TulispConvertible + 'static,
        {
            #[track_caller]
            fn add_to_context(
                self,
                ctx: &mut TulispContext,
                name: &str,
            ) {
                ctx.define_typed_defun(
                    name,
                    crate::value::DefunArity { required: $args_count, optional: $opts_count, has_rest: true },
                    move |ctx, _args| {
                        impl_tulisp_callable!(@bind_rest _args, $($arg)*, $($opt)*, RestT, rest);
                        let res = (self)(
                            ctx,
                            $($crate::TulispConvertible::from_tulisp($arg)?,)*
                            $($opt,)*
                            rest
                        );
                        Ok($crate::TulispConvertible::into_tulisp(res))
                    }
                );
            }
        }

        // With context, no return, with rest
        #[allow(nonstandard_style)]
        impl<RestT, FnT, $($arg,)* $($opt,)*>
        TulispCallable<($($arg,)* $($opt,)* RestT,), (), true, $args_count, $opts_count, false, true, false, false> for FnT
        where
        FnT: Fn(&mut TulispContext, $($arg,)* $(Option<$opt>,)* Rest<RestT>) + 'static + SyncSend,
        $($arg: $crate::TulispConvertible + 'static,)*
        $($opt: $crate::TulispConvertible + 'static,)*
        RestT: $crate::TulispConvertible + 'static,
        {
            #[track_caller]
            fn add_to_context(
                self,
                ctx: &mut TulispContext,
                name: &str,
            ) {
                ctx.define_typed_defun(
                    name,
                    crate::value::DefunArity { required: $args_count, optional: $opts_count, has_rest: true },
                    move |ctx, _args| {
                        impl_tulisp_callable!(@bind_rest _args, $($arg)*, $($opt)*, RestT, rest);
                        (self)(
                            ctx,
                            $($crate::TulispConvertible::from_tulisp($arg)?,)*
                            $($opt,)*
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
        TulispCallable<($($arg,)* $($opt,)* RestT,), OutT, false, $args_count, $opts_count, false, true, true, true> for FnT
        where
        FnT: Fn($($arg,)* $(Option<$opt>,)* Rest<RestT>) -> Result<OutT, Error> + 'static + SyncSend,
        $($arg: $crate::TulispConvertible + 'static,)*
        $($opt: $crate::TulispConvertible + 'static,)*
        RestT: $crate::TulispConvertible + 'static,
        OutT: $crate::TulispConvertible + 'static,
        {
            #[track_caller]
            fn add_to_context(
                self,
                ctx: &mut TulispContext,
                name: &str,
            ) {
                ctx.define_typed_defun(
                    name,
                    crate::value::DefunArity { required: $args_count, optional: $opts_count, has_rest: true },
                    move |_ctx, _args| {
                        impl_tulisp_callable!(@bind_rest _args, $($arg)*, $($opt)*, RestT, rest);
                        let res = (self)(
                            $($crate::TulispConvertible::from_tulisp($arg)?,)*
                            $($opt,)*
                            rest
                        )?;
                        Ok($crate::TulispConvertible::into_tulisp(res))
                    }
                );
            }
        }

        // With context, fallible, with rest
        #[allow(nonstandard_style)]
        impl<RestT, OutT, FnT, $($arg,)* $($opt,)*>
        TulispCallable<($($arg,)* $($opt,)* RestT,), OutT, true, $args_count, $opts_count, false, true, true, true> for FnT
        where
        FnT: Fn(&mut TulispContext, $($arg,)* $(Option<$opt>,)* Rest<RestT>) -> Result<OutT, Error> + 'static + SyncSend,
        $($arg: $crate::TulispConvertible + 'static,)*
        $($opt: $crate::TulispConvertible + 'static,)*
        RestT: $crate::TulispConvertible + 'static,
        OutT: $crate::TulispConvertible + 'static,
        {
            #[track_caller]
            fn add_to_context(
                self,
                ctx: &mut TulispContext,
                name: &str,
            ) {
                ctx.define_typed_defun(
                    name,
                    crate::value::DefunArity { required: $args_count, optional: $opts_count, has_rest: true },
                    move |ctx, _args| {
                        impl_tulisp_callable!(@bind_rest _args, $($arg)*, $($opt)*, RestT, rest);
                        let res = (self)(
                            ctx,
                            $($crate::TulispConvertible::from_tulisp($arg)?,)*
                            $($opt,)*
                            rest
                        )?;
                        Ok($crate::TulispConvertible::into_tulisp(res))
                    }
                );
            }
        }
    };

    // Bind required + optional args from an evaluated arg slice
    // (`&[TulispObject]`). Required args become `&TulispObject`
    // bindings (named after the type-param ident); optional args
    // become `Option<$opt>` bindings, with `null` treated as
    // "absent" to match the prior `destruct_eval_bind!` semantics.
    //
    // Arity is checked by the dispatcher before this runs:
    // `compile_form`'s `Defun` arm rejects mismatches at compile
    // time for VM call sites; `eval::funcall`'s `Defun` arm rejects
    // them at TW call time. The slice is therefore guaranteed to
    // have at least `@count $($arg)*` entries and at most
    // `@count $($arg)* + @count $($opt)*`, so we can index directly
    // without bounds checks here.
    (@bind $args_slice:ident, $($arg:ident)*, $($opt:ident)*) => {
        #[allow(unused_assignments, unused_mut)]
        let mut __idx: usize = 0;
        $(
            let $arg = &$args_slice[__idx];
            __idx += 1;
        )*
        $(
            #[allow(unused_assignments)]
            let $opt: Option<$opt> = if __idx < $args_slice.len() {
                let __v = &$args_slice[__idx];
                __idx += 1;
                if __v.null() {
                    None
                } else {
                    Some(<$opt as $crate::TulispConvertible>::from_tulisp(__v)?)
                }
            } else {
                None
            };
        )*
    };

    // Same as @bind, plus a trailing `$rest_name: Rest<$rest_ty>`
    // collected from the leftover slice. Same arity-already-checked
    // contract.
    (@bind_rest $args_slice:ident, $($arg:ident)*, $($opt:ident)*, $rest_ty:ident, $rest_name:ident) => {
        #[allow(unused_assignments, unused_mut)]
        let mut __idx: usize = 0;
        $(
            let $arg = &$args_slice[__idx];
            __idx += 1;
        )*
        $(
            let $opt: Option<$opt> = if __idx < $args_slice.len() {
                let __v = &$args_slice[__idx];
                __idx += 1;
                if __v.null() {
                    None
                } else {
                    Some(<$opt as $crate::TulispConvertible>::from_tulisp(__v)?)
                }
            } else {
                None
            };
        )*
        let $rest_name: Rest<$rest_ty> = $args_slice[__idx..]
            .iter()
            .map(|__a| <$rest_ty as $crate::TulispConvertible>::from_tulisp(__a))
            .collect::<Result<_, _>>()?;
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

mod plist_args {
    use crate::{Plist, Plistable};

    use super::*;

    /// Reassemble a typed-defun's already-evaluated args slice into the
    /// `(KEY VALUE …)` shape `Plist::new` expects. Values pass through
    /// unchanged — `Plist::new` uses `DummyEval`, so there's no inner
    /// re-eval to defeat with quoting.
    fn build_plist_obj(args: &[TulispObject]) -> Result<TulispObject, Error> {
        let plist = TulispObject::nil();
        for arg in args.iter() {
            plist.push(arg.clone())?;
        }
        Ok(plist)
    }

    #[allow(nonstandard_style)]
    impl<PlistT, OutT, FnT> TulispCallable<(PlistT,), OutT, false, 0, 0, true, false, true, false>
        for FnT
    where
        FnT: Fn(Plist<PlistT>) -> OutT + 'static + SyncSend,
        PlistT: Plistable + 'static,
        OutT: crate::TulispConvertible + 'static,
    {
        #[track_caller]
        fn add_to_context(self, ctx: &mut TulispContext, name: &str) {
            ctx.define_typed_defun(
                name,
                crate::value::DefunArity {
                    required: 0,
                    optional: 0,
                    has_rest: true,
                },
                move |ctx, args| {
                    let obj = build_plist_obj(args)?;
                    let res = (self)(Plist::new(ctx, &obj)?);
                    Ok(crate::TulispConvertible::into_tulisp(res))
                },
            );
        }
    }
    #[allow(nonstandard_style)]
    impl<PlistT, FnT> TulispCallable<(PlistT,), (), false, 0, 0, true, false, true, false> for FnT
    where
        FnT: Fn(Plist<PlistT>) + 'static + SyncSend,
        PlistT: Plistable + 'static,
    {
        #[track_caller]
        fn add_to_context(self, ctx: &mut TulispContext, name: &str) {
            ctx.define_typed_defun(
                name,
                crate::value::DefunArity {
                    required: 0,
                    optional: 0,
                    has_rest: true,
                },
                move |ctx, args| {
                    let obj = build_plist_obj(args)?;
                    (self)(Plist::new(ctx, &obj)?);
                    Ok(TulispObject::nil())
                },
            );
        }
    }
    #[allow(nonstandard_style)]
    impl<PlistT, OutT, FnT> TulispCallable<(PlistT,), OutT, true, 0, 0, true, false, true, false>
        for FnT
    where
        FnT: Fn(&mut TulispContext, Plist<PlistT>) -> OutT + 'static + SyncSend,
        PlistT: Plistable + 'static,
        OutT: crate::TulispConvertible + 'static,
    {
        #[track_caller]
        fn add_to_context(self, ctx: &mut TulispContext, name: &str) {
            ctx.define_typed_defun(
                name,
                crate::value::DefunArity {
                    required: 0,
                    optional: 0,
                    has_rest: true,
                },
                move |ctx, args| {
                    let obj = build_plist_obj(args)?;
                    let plist = Plist::new(ctx, &obj)?;
                    let res = (self)(ctx, plist);
                    Ok(crate::TulispConvertible::into_tulisp(res))
                },
            );
        }
    }
    #[allow(nonstandard_style)]
    impl<PlistT, FnT> TulispCallable<(PlistT,), (), true, 0, 0, true, false, false, false> for FnT
    where
        FnT: Fn(&mut TulispContext, Plist<PlistT>) + 'static + SyncSend,
        PlistT: Plistable + 'static,
    {
        #[track_caller]
        fn add_to_context(self, ctx: &mut TulispContext, name: &str) {
            ctx.define_typed_defun(
                name,
                crate::value::DefunArity {
                    required: 0,
                    optional: 0,
                    has_rest: true,
                },
                move |ctx, args| {
                    let obj = build_plist_obj(args)?;
                    let plist = Plist::new(ctx, &obj)?;
                    (self)(ctx, plist);
                    Ok(TulispObject::nil())
                },
            );
        }
    }
    #[allow(nonstandard_style)]
    impl<PlistT, OutT, FnT> TulispCallable<(PlistT,), OutT, false, 0, 0, true, false, true, true>
        for FnT
    where
        FnT: Fn(Plist<PlistT>) -> Result<OutT, Error> + 'static + SyncSend,
        PlistT: Plistable + 'static,
        OutT: crate::TulispConvertible + 'static,
    {
        #[track_caller]
        fn add_to_context(self, ctx: &mut TulispContext, name: &str) {
            ctx.define_typed_defun(
                name,
                crate::value::DefunArity {
                    required: 0,
                    optional: 0,
                    has_rest: true,
                },
                move |ctx, args| {
                    let obj = build_plist_obj(args)?;
                    let plist = Plist::new(ctx, &obj)?;
                    let res = (self)(plist);
                    res.map(|x| x.into_tulisp())
                },
            );
        }
    }
    #[allow(nonstandard_style)]
    impl<PlistT, OutT, FnT> TulispCallable<(PlistT,), OutT, true, 0, 0, true, false, true, true> for FnT
    where
        FnT: Fn(&mut TulispContext, Plist<PlistT>) -> Result<OutT, Error> + 'static + SyncSend,
        PlistT: Plistable + 'static,
        OutT: crate::TulispConvertible + 'static,
    {
        #[track_caller]
        fn add_to_context(self, ctx: &mut TulispContext, name: &str) {
            ctx.define_typed_defun(
                name,
                crate::value::DefunArity {
                    required: 0,
                    optional: 0,
                    has_rest: true,
                },
                move |ctx, args| {
                    let obj = build_plist_obj(args)?;
                    let plist = Plist::new(ctx, &obj)?;
                    let res = (self)(ctx, plist);
                    res.map(|x| x.into_tulisp())
                },
            );
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::test_utils::{eval_assert_equal, eval_assert_error};
    use crate::{Error, Rest, TulispContext, TulispObject};

    #[test]
    fn test_add_functions_only_rest() -> Result<(), crate::Error> {
        let ctx = &mut TulispContext::new();
        ctx.defun("sum", |items: Rest<f64>| -> f64 { items.into_iter().sum() });

        eval_assert_equal(ctx, "(sum)", "0.0");
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
            r#"ERR MissingArgument: Too few arguments
<eval_string>:1.1-1.13:  at (add_round 2)
"#,
        );

        ctx.defun("greet", |name: String| format!("Hello, {}!", name));
        eval_assert_equal(ctx, r#"(greet "Alice")"#, r#""Hello, Alice!""#);
        eval_assert_error(
            ctx,
            r#"(greet "Alice" "Peter")"#,
            r#"ERR InvalidArgument: Too many arguments
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
            r#"ERR InvalidArgument: Too many arguments
<eval_string>:1.1-1.13:  at (power 2 3 4)
"#,
        );
        eval_assert_error(
            ctx,
            "(power)",
            r#"ERR MissingArgument: Too few arguments
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
}
