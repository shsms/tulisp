//! [`AsSymbol!`](macro@crate::AsSymbol): a Rust enum whose unit
//! variants are Lisp symbols.

use crate::TulispObject;

/// Calls `f` on the name `value` reads as a symbol: a symbol's or a
/// lexical binding's name, `nil` or `t`; `None` for any other value.
#[doc(hidden)]
pub fn with_symbol_name<R>(value: &TulispObject, f: impl FnOnce(&str) -> R) -> Option<R> {
    value.inner_ref().0.symbol_name().map(f)
}

/// Declares an enum whose variants convert to and from symbols.
///
/// ```text
/// AsSymbol! {
///     [attributes]
///     [pub] enum Name {
///         Variant[<"symbol-name">],
///         ...
///     }
/// }
/// ```
///
/// A variant without `<"...">` uses its own name as the symbol, without
/// the `r#` of a raw identifier. `symbol_name` and `from_symbol_name`
/// map a variant to its symbol and back, and `SYMBOL_NAMES` lists every
/// symbol in declaration order. Two variants naming the same symbol
/// are a compile error:
///
/// ```compile_fail
/// tulisp::AsSymbol! { enum Dup { Alpha<"same">, Beta<"same"> } }
/// ```
///
/// `from_tulisp` requires a symbol; a string is a type mismatch and an
/// unknown symbol is an invalid argument naming the accepted
/// symbols. `into_tulisp` interns the variant's symbol. `nil` and `t`
/// are accepted as symbol names, so a variant may be spelled
/// `<"nil">` or `<"t">`; through an `Option`, though, `nil` is
/// always `None`.
///
/// # Example
///
/// ```rust
/// use tulisp::{AsSymbol, TulispContext};
///
/// AsSymbol! {
///     #[derive(Debug, Clone, Copy, PartialEq)]
///     pub enum Mode { Fast<"fast">, Careful<"careful"> }
/// }
///
/// let mut ctx = TulispContext::new();
/// ctx.defun("mode-name", |m: Mode| -> String { m.symbol_name().to_uppercase() });
/// assert_eq!(ctx.eval_string("(mode-name 'careful)").unwrap().to_string(), r#""CAREFUL""#);
/// assert_eq!(Mode::from_symbol_name("fast"), Some(Mode::Fast));
/// assert_eq!(Mode::SYMBOL_NAMES, ["fast", "careful"]);
/// ```
#[macro_export]
macro_rules! AsSymbol {
    (@symbol $variant:ident<$symbol:literal>) => { $symbol };
    (@symbol $variant:ident) => { $crate::as_list::field_key(stringify!($variant)) };

    (
        $( #[$meta:meta] )*
        $vis:vis enum $name:ident {
            $(
                $( #[$($variant_meta:tt)+] )*
                $variant:ident $(<$symbol:literal>)?
            ),+ $(,)?
        }
    ) => {
        $( #[$meta] )*
        $vis enum $name {
            $( $( #[$($variant_meta)+] )* $variant ),+
        }

        impl $name {
            /// The symbol this variant reads from and writes to.
            pub fn symbol_name(&self) -> &'static str {
                match self {
                    $( $name::$variant => $crate::AsSymbol!(@symbol $variant $(<$symbol>)?), )+
                }
            }

            /// The variant that reads from the symbol `name`, if any.
            pub fn from_symbol_name(__name: &str) -> Option<Self> {
                $( if __name == $crate::AsSymbol!(@symbol $variant $(<$symbol>)?) {
                    return Some($name::$variant);
                } )+
                None
            }

            /// Every variant's symbol, in declaration order.
            pub const SYMBOL_NAMES: &'static [&'static str] = &[
                $( $crate::AsSymbol!(@symbol $variant $(<$symbol>)?), )+
            ];
        }

        const _: () = assert!(
            $crate::as_symbol::distinct($name::SYMBOL_NAMES),
            "AsSymbol! variants must map to distinct symbols"
        );

        impl $crate::TulispConvertible for $name {
            fn from_tulisp(
                __ctx: &mut $crate::TulispContext,
                __value: &$crate::TulispObject,
            ) -> Result<Self, $crate::Error> {
                let Some(__result) = $crate::as_symbol::with_symbol_name(__value, |__symbol| {
                    Self::from_symbol_name(__symbol).ok_or_else(|| {
                        $crate::Error::invalid_argument(format!(
                            "unknown {} '{__symbol}'; expected one of {}",
                            stringify!($name),
                            Self::SYMBOL_NAMES.join(", ")
                        ))
                        .with_trace(__value.clone())
                    })
                }) else {
                    return Err($crate::Error::type_mismatch(format!(
                        "Expected a symbol for {}, got: {__value}",
                        stringify!($name)
                    ))
                    .with_trace(__value.clone()));
                };
                __result
            }

            fn into_tulisp(self, __ctx: &mut $crate::TulispContext) -> $crate::TulispObject {
                __ctx.intern(self.symbol_name())
            }
        }
    };
}

/// True when no two of `names` are equal; evaluated at compile time
/// for every `AsSymbol!` declaration.
#[doc(hidden)]
pub const fn distinct(names: &[&str]) -> bool {
    let mut i = 0;
    while i < names.len() {
        let mut j = i + 1;
        while j < names.len() {
            if crate::as_list::bytes_eq(names[i].as_bytes(), names[j].as_bytes()) {
                return false;
            }
            j += 1;
        }
        i += 1;
    }
    true
}

#[cfg(test)]
mod tests {
    use crate::test_utils::{eval_assert_equal, eval_assert_error};
    use crate::{TulispContext, TulispConvertible};

    crate::AsSymbol! {
        #[derive(Debug, Clone, Copy, PartialEq)]
        pub enum Health {
            Ok<"ok">,
            Error<"error">,
            Standby,
        }
    }

    crate::AsSymbol! {
        #[derive(Debug, PartialEq)]
        #[allow(non_camel_case_types)]
        enum Raw {
            r#type,
        }
    }

    // Constants spelled like the generated bindings must not capture
    // them: an identifier pattern resolves to a constant in scope.
    #[allow(non_upper_case_globals, dead_code)]
    mod beside_constants {
        const name: &str = "";
        const value: &str = "";
        const symbol: &str = "";
        const result: &str = "";
        const ctx: &str = "";
        crate::AsSymbol! {
            #[derive(Debug, PartialEq)]
            pub enum Shadowed {
                One<"one">,
            }
        }
    }

    #[test]
    fn an_as_symbol_beside_same_named_constants_compiles() {
        assert_eq!(
            beside_constants::Shadowed::from_symbol_name("one"),
            Some(beside_constants::Shadowed::One)
        );
        assert_eq!(beside_constants::Shadowed::SYMBOL_NAMES, ["one"]);
    }

    #[test]
    fn a_symbol_name_maps_back_to_its_variant() {
        assert_eq!(Health::from_symbol_name("ok"), Some(Health::Ok));
        assert_eq!(Health::from_symbol_name("Standby"), Some(Health::Standby));
        assert_eq!(Health::from_symbol_name("Ok"), None);
    }

    #[test]
    fn a_raw_identifier_variant_is_the_symbol_without_the_prefix() {
        let mut ctx = TulispContext::new();
        assert_eq!(Raw::r#type.symbol_name(), "type");
        let value = ctx.eval_string("'type").unwrap();
        assert_eq!(Raw::from_tulisp(&mut ctx, &value).unwrap(), Raw::r#type);
    }

    crate::AsSymbol! {
        #[derive(Debug, Clone, Copy, PartialEq)]
        enum Tri {
            Off<"nil">,
            On<"t">,
            Maybe<"maybe">,
        }
    }

    #[test]
    fn a_symbol_reads_as_its_variant_and_writes_back() {
        let mut ctx = TulispContext::new();
        let sym = ctx.intern("error");
        assert_eq!(Health::from_tulisp(&mut ctx, &sym).unwrap(), Health::Error);
        let sym = ctx.intern("Standby");
        assert_eq!(
            Health::from_tulisp(&mut ctx, &sym).unwrap(),
            Health::Standby
        );
        assert!(Health::Ok.into_tulisp(&mut ctx).eq(&ctx.intern("ok")));
        assert_eq!(Health::Standby.symbol_name(), "Standby");
    }

    #[test]
    fn a_string_and_an_unknown_symbol_are_errors() {
        let mut ctx = TulispContext::new();
        let s = "ok".to_string().into_tulisp(&mut ctx);
        let err = Health::from_tulisp(&mut ctx, &s).unwrap_err();
        assert!(
            err.to_string().contains("Expected a symbol for Health"),
            "{err}"
        );
        let sym = ctx.intern("broken");
        let err = Health::from_tulisp(&mut ctx, &sym).unwrap_err();
        let msg = err.to_string();
        assert!(
            msg.contains("unknown Health 'broken'") && msg.contains("ok, error, Standby"),
            "{msg}"
        );
    }

    #[test]
    fn nil_and_t_variants_round_trip_and_reject_a_string() {
        let mut ctx = TulispContext::new();
        for (variant, name) in [(Tri::Off, "nil"), (Tri::On, "t"), (Tri::Maybe, "maybe")] {
            let obj = variant.into_tulisp(&mut ctx);
            assert_eq!(Tri::from_tulisp(&mut ctx, &obj).unwrap(), variant);
            assert_eq!(variant.symbol_name(), name);
        }
        let s = "nil".to_string().into_tulisp(&mut ctx);
        let err = Tri::from_tulisp(&mut ctx, &s).unwrap_err();
        assert!(
            err.to_string().contains("Expected a symbol for Tri"),
            "{err}"
        );
    }

    #[test]
    fn an_enum_is_a_defun_parameter_and_return() {
        let mut ctx = TulispContext::new();
        ctx.defun("flip", |h: Health| -> Health {
            match h {
                Health::Ok => Health::Error,
                Health::Error | Health::Standby => Health::Ok,
            }
        });
        eval_assert_equal(&mut ctx, "(flip 'ok)", "'error");
        eval_assert_equal(&mut ctx, "(flip 'Standby)", "'ok");
        eval_assert_error(
            &mut ctx,
            "(flip \"ok\")",
            "ERR TypeMismatch: Expected a symbol for Health, got: \"ok\"\n<eval_string>:1.1-1.11:  at (flip \"ok\")\n",
        );
    }
}
