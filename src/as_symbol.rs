//! [`AsSymbol!`](macro@crate::AsSymbol): a Rust enum whose unit
//! variants are Lisp symbols.

use crate::{Error, TulispObject, TulispValue};

/// Calls `f` on the name `value` reads as: a symbol's or a lexical binding's
/// name, `nil` or `t`, and with `strings` also a string's text; `None` for any
/// other value.
#[doc(hidden)]
pub fn with_name<R>(value: &TulispObject, strings: bool, f: impl FnOnce(&str) -> R) -> Option<R> {
    let inner = value.inner_ref();
    match &inner.0 {
        TulispValue::String { value } if strings => Some(f(value)),
        other => other.symbol_name().map(f),
    }
}

/// The error for a value that is not a name at all; it names the accepted
/// spellings, as `unknown_name` does.
#[doc(hidden)]
pub fn not_a_name(type_name: &str, strings: bool, names: &[&str], value: &TulispObject) -> Error {
    let expected = if strings {
        "a symbol or a string"
    } else {
        "a symbol"
    };
    Error::type_mismatch(format!(
        "Expected {expected} for {type_name} (one of {}), got: {value}",
        names.join(", ")
    ))
    .with_trace(value.clone())
}

/// The error for a name that is none of `names`.
#[doc(hidden)]
pub fn unknown_name(type_name: &str, name: &str, names: &[&str]) -> Error {
    Error::invalid_argument(format!(
        "unknown {type_name} '{name}'; expected one of {}",
        names.join(", ")
    ))
}

/// Declares an enum whose variants convert to and from symbols.
///
/// ```text
/// AsSymbol! {
///     [doc comment]
///     [#[lisp(strings)]]      // also read and write strings
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
/// Without `#[lisp(strings)]`, `from_tulisp` requires a symbol; a
/// string is a type mismatch and an unknown symbol is an invalid
/// argument, and both errors name the accepted symbols. `into_tulisp`
/// interns the variant's symbol. `nil` and `t` are accepted as symbol
/// names, so a variant may be spelled `<"nil">` or `<"t">`; through an
/// `Option`, though, `nil` is always `None`.
///
/// A `#[lisp(strings)]` marker, after the doc comment and before the
/// other attributes, lets `from_tulisp` also read a string with a
/// variant's spelling, and makes `into_tulisp` write the spelling as a
/// string. Use it for names that callers write as strings, or that do
/// not read as a symbol, such as `"90"`. Through an `Option`, the
/// string `"nil"` then reads as a variant spelled `<"nil">`. A marker
/// after another attribute is a compile error, and so is an unknown
/// option:
///
/// ```compile_fail
/// tulisp::AsSymbol! { #[derive(Debug)] #[lisp(strings)] enum Late { A } }
/// ```
///
/// ```compile_fail
/// tulisp::AsSymbol! { #[lisp(string)] enum Typo { A } }
/// ```
///
/// ```rust
/// use tulisp::{AsSymbol, TulispContext};
///
/// AsSymbol! {
///     #[lisp(strings)]
///     #[derive(Debug, Clone, Copy, PartialEq)]
///     pub enum Turn { Half<"180">, Quarter<"90"> }
/// }
///
/// let mut ctx = TulispContext::new();
/// ctx.defun("turn", |t: Turn| t);
/// assert_eq!(ctx.eval_string(r#"(turn "90")"#).unwrap().to_string(), r#""90""#);
/// ```
///
/// `Display` writes a variant's spelling, and `FromStr` reads it back; its
/// error names the accepted spellings.
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
/// assert_eq!("careful".parse::<Mode>().unwrap(), Mode::Careful);
/// assert_eq!(Mode::Fast.to_string(), "fast");
/// ```
#[macro_export]
macro_rules! AsSymbol {
    (@symbol $variant:ident<$symbol:literal>) => { $symbol };
    (@symbol $variant:ident) => { $crate::as_list::field_key(stringify!($variant)) };

    ($( #[doc = $doc:literal] )* #[lisp(strings)] $($rest:tt)*) => {
        $crate::AsSymbol!(@decl true $( #[doc = $doc] )* $($rest)*);
    };
    ($( #[doc = $doc:literal] )* #[lisp($option:ident)] $($rest:tt)*) => {
        compile_error!(concat!(
            "unknown AsSymbol! option #[lisp(",
            stringify!($option),
            ")]; the only one is #[lisp(strings)]"
        ));
    };

    ($( #[$meta:meta] )* $vis:vis enum $($rest:tt)*) => {
        $crate::AsSymbol!(@decl false $( #[$meta] )* $vis enum $($rest)*);
    };

    (
        @decl $strings:literal
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
            /// The spelling this variant reads from and writes to.
            pub fn symbol_name(&self) -> &'static str {
                match self {
                    $( $name::$variant => $crate::AsSymbol!(@symbol $variant $(<$symbol>)?), )+
                }
            }

            /// The variant with this spelling, if any.
            pub fn from_symbol_name(__name: &str) -> Option<Self> {
                $( if __name == $crate::AsSymbol!(@symbol $variant $(<$symbol>)?) {
                    return Some($name::$variant);
                } )+
                None
            }

            /// Every variant's spelling, in declaration order.
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
                let Some(__result) = $crate::as_symbol::with_name(__value, $strings, |__name| {
                    <Self as ::std::str::FromStr>::from_str(__name)
                        .map_err(|__err| __err.with_trace(__value.clone()))
                }) else {
                    return Err($crate::as_symbol::not_a_name(
                        stringify!($name),
                        $strings,
                        Self::SYMBOL_NAMES,
                        __value,
                    ));
                };
                __result
            }

            fn into_tulisp(self, __ctx: &mut $crate::TulispContext) -> $crate::TulispObject {
                if $strings {
                    $crate::TulispObject::from(self.symbol_name())
                } else {
                    __ctx.intern(self.symbol_name())
                }
            }
        }

        impl ::std::fmt::Display for $name {
            fn fmt(&self, __f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                __f.write_str(self.symbol_name())
            }
        }

        impl ::std::str::FromStr for $name {
            type Err = $crate::Error;

            fn from_str(__name: &str) -> Result<Self, $crate::Error> {
                Self::from_symbol_name(__name).ok_or_else(|| {
                    $crate::as_symbol::unknown_name(stringify!($name), __name, Self::SYMBOL_NAMES)
                })
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
    use crate::{TulispContext, TulispConvertible, TulispObject};

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

    crate::AsSymbol! {
        /// Turns, read from symbols or strings.
        #[lisp(strings)]
        #[derive(Debug, Clone, Copy, PartialEq)]
        enum Turn {
            None<"nil">,
            Quarter<"90">,
            Half<"half">,
        }
    }

    #[test]
    fn with_strings_a_string_also_reads_and_a_variant_writes_as_a_string() {
        let mut ctx = TulispContext::new();
        let sym = ctx.intern("half");
        assert_eq!(Turn::from_tulisp(&mut ctx, &sym).unwrap(), Turn::Half);
        for (text, variant) in [("90", Turn::Quarter), ("half", Turn::Half)] {
            let s = text.to_string().into_tulisp(&mut ctx);
            assert_eq!(Turn::from_tulisp(&mut ctx, &s).unwrap(), variant);
            let back = variant.into_tulisp(&mut ctx);
            assert!(back.stringp(), "{back}");
            assert_eq!(back.as_string().unwrap(), text);
        }
    }

    #[test]
    fn with_strings_an_option_keeps_nil_apart_from_the_string_nil() {
        let mut ctx = TulispContext::new();
        let nil = TulispObject::nil();
        assert_eq!(Option::<Turn>::from_tulisp(&mut ctx, &nil).unwrap(), None);
        let s = "nil".to_string().into_tulisp(&mut ctx);
        let turn = Option::<Turn>::from_tulisp(&mut ctx, &s).unwrap();
        assert_eq!(turn, Some(Turn::None));
        let back = turn.into_tulisp(&mut ctx);
        assert_eq!(back.as_string().unwrap(), "nil");
        assert!(None::<Turn>.into_tulisp(&mut ctx).null());
    }

    #[test]
    fn with_strings_other_values_and_unknown_names_are_errors() {
        let mut ctx = TulispContext::new();
        ctx.defun("turn", |t: Turn| t);
        eval_assert_error(
            &mut ctx,
            "(turn 90)",
            "ERR TypeMismatch: Expected a symbol or a string for Turn (one of nil, 90, half), got: 90\n<eval_string>:1.1-1.9:  at (turn 90)\n",
        );
        eval_assert_error(
            &mut ctx,
            r#"(turn "45")"#,
            "ERR InvalidArgument: unknown Turn '45'; expected one of nil, 90, half\n<eval_string>:1.1-1.11:  at (turn \"45\")\n",
        );
    }

    #[test]
    fn display_and_from_str_use_the_spelling() {
        assert_eq!(Health::Ok.to_string(), "ok");
        assert_eq!(Health::Standby.to_string(), "Standby");
        assert_eq!("error".parse::<Health>().unwrap(), Health::Error);
        let err = "broken".parse::<Health>().unwrap_err();
        assert_eq!(
            err.to_string(),
            "ERR InvalidArgument: unknown Health 'broken'; expected one of ok, error, Standby"
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
            "ERR TypeMismatch: Expected a symbol for Health (one of ok, error, Standby), got: \"ok\"\n<eval_string>:1.1-1.11:  at (flip \"ok\")\n",
        );
    }
}
