//! Property-list (plist) primitives and the typed [`Plistable`] layer.
//!
//! A plist is a flat alternating-key/value list, e.g. `(:a 1 :b 2)`.
//! See [the Emacs Lisp manual] for the canonical reference.
//!
//! [the Emacs Lisp manual]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Property-Lists.html

use std::ops::Deref;

use crate::{Error, TulispContext, TulispObject};

/// Makes a plist from the given arguments.
pub fn plist_from<const N: usize>(input: [(TulispObject, TulispObject); N]) -> TulispObject {
    let mut builder = crate::cons::ListBuilder::new();
    for (key, value) in input.into_iter() {
        builder.push(key);
        builder.push(value);
    }
    builder.build()
}

/// Returns the value of the property `property` stored in the property list
/// `plist`.
pub fn plist_get(plist: &TulispObject, property: &TulispObject) -> Result<TulispObject, Error> {
    let mut cur = plist.clone();
    while cur.consp() {
        if cur.car_and_then(|car| Ok(car.eq(property)))? {
            return cur.cadr();
        }
        cur = cur.cddr()?;
    }
    Ok(TulispObject::nil())
}

/// A typed wrapper around a Lisp plist, for use as a [`defun`](crate::TulispContext::defun) argument.
///
/// When `Plist<T>` appears as a parameter type, the function receives the
/// caller's entire argument list as a plist and deserializes it into `T`.
///
/// `T` must implement [`Plistable`], which is most easily done via the
/// [`AsPlist!`](macro@crate::AsPlist) macro.
///
/// `Plist<T>` implements [`Deref<Target = T>`], so fields of the inner struct
/// can be accessed directly.
///
/// # Example
///
/// ```rust
/// use tulisp::{TulispContext, Plist, Plistable, AsPlist};
///
/// AsPlist! {
///     struct Point { x: i64, y: i64 }
/// }
///
/// let mut ctx = TulispContext::new();
///
/// ctx.defun("distance", |p: Plist<Point>| -> f64 {
///     ((p.x * p.x + p.y * p.y) as f64).sqrt()
/// });
///
/// assert_eq!(
///     ctx.eval_string("(distance :x 3 :y 4)").unwrap().as_number().unwrap(),
///     5.0
/// );
/// ```
pub struct Plist<T: Plistable> {
    plist: T,
}

impl<T> Plist<T>
where
    T: Plistable,
{
    pub(crate) fn new(ctx: &mut TulispContext, args: &[TulispObject]) -> Result<Self, Error> {
        Ok(Self {
            plist: T::from_plist_as_slice(ctx, args)?,
        })
    }

    pub fn into_inner(self) -> T {
        self.plist
    }
}

impl<T> Deref for Plist<T>
where
    T: Plistable,
{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.plist
    }
}

/// Conversion between a Rust struct and a Lisp plist.
///
/// The plist's values are treated as already-evaluated lisp objects
/// and passed through to the field types' `TryFrom<TulispObject>` impls
/// without further evaluation — which is what you want for plists held
/// in free variables, literals, or any value already produced by the
/// interpreter:
///
/// ```ignore
/// let cfg = MyType::from_plist(&mut ctx, &obj)?;
/// ```
///
/// The [`AsPlist!`](macro@crate::AsPlist) macro generates both
/// `from_plist_as_slice` and `from_plist` from a struct definition.
pub trait Plistable {
    /// Deserialize `Self` from a flat `[k0, v0, k1, v1, …]` slice of
    /// already-evaluated lisp values. The defun-arg path (`Plist<T>`)
    /// calls this directly with the evaluated arg slice.
    fn from_plist_as_slice(
        ctx: &mut TulispContext,
        kvs: &[TulispObject],
    ) -> Result<Self, Error>
    where
        Self: Sized;

    /// Deserialize an already-evaluated lisp plist value into `Self`.
    /// Iterates `obj.base_iter()` directly without an intermediate
    /// `Vec`.
    fn from_plist(ctx: &mut TulispContext, obj: &TulispObject) -> Result<Self, Error>
    where
        Self: Sized;

    /// Serialize `self` into a Lisp plist.
    fn into_plist(self, ctx: &mut TulispContext) -> TulispObject;
}

/// Derive [`Plistable`] for a struct, enabling it to be used as a [`Plist<T>`]
/// argument in [`defun`](crate::TulispContext::defun)-registered functions.
///
/// # Syntax
///
/// ```text
/// AsPlist! {
///     [attributes]
///     [pub] struct Name {
///         [field_vis] field[<":key-name">]: Type [{= default}],
///         ...
///     }
/// }
/// ```
///
/// - Each field maps to a plist keyword.  By default the keyword is
///   `":fieldname"`.  Use `field<":custom-key">` to override.
/// - A field with `{= expr}` is optional; if absent from the plist the
///   default expression is used.
/// - A field without a default is required; a missing key is an error.
///
/// # Example
///
/// ```rust
/// use tulisp::{TulispContext, Plist, AsPlist};
///
/// AsPlist! {
///     struct Config {
///         host: String,
///         port<":port-number">: i64 {= 8080},
///         scheme: Option<String> {= None},
///     }
/// }
///
/// let mut ctx = TulispContext::new();
/// ctx.defun("make-server", |cfg: Plist<Config>| -> String {
///     format!(
///         "{}://{}:{}",
///         cfg.scheme.clone().unwrap_or_else(||"http".to_string()),
///         cfg.host,
///         cfg.port
///     )
/// });
///
/// assert_eq!(
///    ctx.eval_string(r#"(make-server :host "localhost")"#).unwrap().as_string().unwrap(),
///    "http://localhost:8080"
/// );
/// assert_eq!(
///    ctx.eval_string(r#"(make-server :host "example.com" :port-number 443)"#).unwrap().as_string().unwrap(),
///    "http://example.com:443"
/// );
///
/// assert_eq!(
///    ctx.eval_string(r#"(make-server :host "localhost" :scheme "https")"#).unwrap().as_string().unwrap(),
///    "https://localhost:8080"
/// );
/// ```
#[macro_export]
macro_rules! AsPlist {
    (@key-name
        $field:ident<$field_key:literal>) => {
        $field_key
    };
    (@key-name $field:ident) => {
        concat!(":", stringify!($field))
    };

    (@missing-field $key_name:expr, $default:expr) => {
        Ok($default)
    };
    (@missing-field $key_name:expr,) => {
        Err($crate::Error::plist_error(concat!("Missing ", $key_name, " field")))
    };

    (@extract-field $value:ident, None) => {
        if $value.null() {
            None
        } else {
            Some($value.try_into()?)
        }
    };

    (@extract-field $value:ident, Some($e: expr)) => {
        if $value.null() {
            None
        } else {
            Some($value.try_into()?)
        }
    };

    (@extract-field $value:ident, $e: expr) => {
        $value.try_into()?
    };

    (@extract-field $value:ident) => {
        $value.try_into()?
    };

    (
        $( #[$meta:meta] )*
        $vis:vis struct $struct_name:ident {
            $(
                $( #[$($field_meta:tt)+] )*
                $field_vis:vis $field:ident$(<$field_key:literal>)? : $type:ty
                $({= $($default:tt)+ })?
            ),+ $(,)?
        }
    ) => {

        $( #[$meta] )*
        $vis struct $struct_name {

            $($( #[$($field_meta)+] )* $field_vis $field: $type),+
        }

        impl $crate::Plistable for $struct_name {
            fn from_plist_as_slice(
                ctx: &mut TulispContext,
                kvs: &[$crate::TulispObject],
            ) -> Result<Self, $crate::Error> {
                #[derive(Default)]
                struct Builder {
                    $($field: Option<$type>),+
                }

                impl Builder {
                    fn build(self) -> Result<$struct_name, $crate::Error> {
                        Ok($struct_name {
                            $($field: if let Some(f) = self.$field { f } else {
                                $crate::AsPlist!(
                                    @missing-field
                                    $crate::AsPlist!(@key-name $field $(<$field_key>)?),
                                    $( $($default)+ )?
                                )?}),+
                        })
                    }
                }

                let symbols = $crate::intern!(ctx => {
                    $($field: $crate::AsPlist!(@key-name $field $(<$field_key>)?)),+
                });

                let (kvpairs, extra) = kvs.as_chunks::<2>();

                if !extra.is_empty() {
                    return Err($crate::Error::plist_error(
                        "Expected an even number of items in the plist",
                    ));
                }

                let mut builder = Builder::default();

                for [key, value] in kvpairs {
                    $(if key.eq(&symbols.$field) {
                        let value = value.clone();
                        builder.$field = Some($crate::AsPlist!(@extract-field value $(, $($default)+)?));
                    } else)+ {
                        return Err($crate::Error::plist_error(format!(
                            "Unexpected key in plist: {}",
                            key
                        )));
                    }
                }

                builder.build()
            }

            fn from_plist(
                ctx: &mut TulispContext,
                obj: &$crate::TulispObject,
            ) -> Result<Self, $crate::Error> {
                #[derive(Default)]
                struct Builder {
                    $($field: Option<$type>),+
                }

                impl Builder {
                    fn build(self) -> Result<$struct_name, $crate::Error> {
                        Ok($struct_name {
                            $($field: if let Some(f) = self.$field { f } else {
                                $crate::AsPlist!(
                                    @missing-field
                                    $crate::AsPlist!(@key-name $field $(<$field_key>)?),
                                    $( $($default)+ )?
                                )?}),+
                        })
                    }
                }

                let symbols = $crate::intern!(ctx => {
                    $($field: $crate::AsPlist!(@key-name $field $(<$field_key>)?)),+
                });

                let mut iter = obj.base_iter();
                let mut builder = Builder::default();

                while let Some(key) = iter.next() {
                    let Some(value) = iter.next() else {
                        return Err($crate::Error::plist_error(
                            "Expected an even number of items in the plist",
                        ));
                    };
                    $(if key.eq(&symbols.$field) {
                        builder.$field = Some($crate::AsPlist!(@extract-field value $(, $($default)+)?));
                    } else)+ {
                        return Err($crate::Error::plist_error(format!(
                            "Unexpected key in plist: {}",
                            key
                        )));
                    }
                }

                builder.build()
            }

            fn into_plist(self, ctx: &mut TulispContext) -> $crate::TulispObject {
                let symbols = $crate::intern!(ctx => {
                    $($field: $crate::AsPlist!(@key-name $field $(<$field_key>)?)),+
                });

                $crate::plist::plist_from([
                    $((symbols.$field.clone(), self.$field.into())),+
                ])
            }
        }
     };
}

#[cfg(test)]
mod tests {
    use super::{plist_from, plist_get};
    use crate::{
        Error, Plist, Plistable, TulispContext,
        test_utils::{eval_assert_equal, eval_assert_error},
    };

    #[test]
    fn test_plist_primitives() -> Result<(), Error> {
        let mut ctx = TulispContext::new();
        let a = ctx.intern("a");
        let b = ctx.intern("b");
        let c = ctx.intern("c");
        let d = ctx.intern("d");
        let list = plist_from([
            (a.clone(), 20.into()),
            (b.clone(), 30.into()),
            (c.clone(), 40.into()),
        ]);
        assert!(plist_get(&list, &b)?.equal(&30.into()));
        assert!(plist_get(&list, &d)?.null());
        Ok(())
    }

    AsPlist! {
        #[derive(Default)]
        struct Person {
            /// The person's first name
            name<":first-name">: String,

            /// The person's age
            age: i64,

            /// The person's addresses
            addr: Vec<String>,

            /// The person's education (optional)
            education<":edu">: Option<String> {= None},

            /// The person's current place (optional, default is "Home")
            place: Option<String> {= Some("Home".to_string())},

            /// The person's answer to the ultimate question of life, the
            /// universe, and everything (optional, default is 42)
            answer: i64 {= 42},
        }
    }

    #[test]
    fn test_plistable() -> Result<(), Error> {
        let mut ctx = TulispContext::new();

        ctx.defun("get-name", |person: Plist<Person>| person.name.clone())
            .defun("get-age", |p: Plist<Person>| -> i64 { p.age })
            .defun("get-ans", |p: Plist<Person>| -> i64 { p.answer })
            .defun("get-place", |p: Plist<Person>| p.place.clone().unwrap())
            .defun("get-edu", |p: Plist<Person>| {
                p.education.clone().unwrap_or("Unknown".to_string())
            })
            .defun("get-addr", |p: Plist<Person>| {
                p.addr.last().unwrap().clone()
            });

        eval_assert_equal(
            &mut ctx,
            r#"(get-name :first-name "Alice" :age 30 :addr nil)"#,
            r#""Alice""#,
        );
        eval_assert_equal(
            &mut ctx,
            r#"(get-age :first-name "Alice" :age 30 :addr nil)"#,
            r#"30"#,
        );

        eval_assert_equal(
            &mut ctx,
            r#"(get-edu :first-name "Alice" :age 30 :addr nil)"#,
            r#""Unknown""#,
        );
        eval_assert_equal(
            &mut ctx,
            r#"(get-edu :first-name "Alice" :age 30 :addr nil :edu "School")"#,
            r#""School""#,
        );

        // Explicit nil for an optional field is read as `None`, not as
        // a value to extract via `try_into::<String>()`. (Before the
        // nil-handling fix this errored with "expected string, got: nil".)
        eval_assert_equal(
            &mut ctx,
            r#"(get-edu :first-name "Alice" :age 30 :addr nil :edu nil)"#,
            r#""Unknown""#,
        );

        eval_assert_equal(
            &mut ctx,
            r#"(get-place :first-name "Alice" :age 30 :addr nil)"#,
            r#""Home""#,
        );
        eval_assert_equal(
            &mut ctx,
            r#"(get-place :first-name "Alice" :age 30 :addr nil :place "Office")"#,
            r#""Office""#,
        );

        eval_assert_equal(
            &mut ctx,
            r#"(get-ans :first-name "Alice" :age 30 :addr nil)"#,
            r#"42"#,
        );
        eval_assert_equal(
            &mut ctx,
            r#"(get-ans :first-name "Alice" :age 30 :addr nil :place "Office" :answer 5)"#,
            r#"5"#,
        );

        eval_assert_equal(
            &mut ctx,
            r#"(get-addr :first-name "Alice" :age 30 :addr '("street" "other street"))"#,
            r#""other street""#,
        );
        eval_assert_equal(
            &mut ctx,
            r#"(get-addr :first-name "Alice" :age 30 :addr '("street" "other street"))"#,
            r#""other street""#,
        );

        eval_assert_error(
            &mut ctx,
            r#"(get-age :first-name "Alice" :age 30 :other 10)"#,
            r#"ERR PlistError: Unexpected key in plist: :other
<eval_string>:1.1-1.47:  at (get-age :first-name "Alice" :age 30 :other 10)
"#,
        );

        eval_assert_error(
            &mut ctx,
            r#"(get-age :first-name "Alice")"#,
            r#"ERR PlistError: Missing :age field
<eval_string>:1.1-1.29:  at (get-age :first-name "Alice")
"#,
        );

        Ok(())
    }

    #[test]
    fn test_plistable_from_lisp_value_no_eval() -> Result<(), Error> {
        // A plist held in a free variable contains already-evaluated
        // values. `from_plist` should pass each value straight through
        // to its `TryFrom<TulispObject>` impl with no further
        // evaluation — so a list field like `addr` resolves cleanly
        // instead of erroring on "calling string as function".
        let mut ctx = TulispContext::new();
        ctx.eval_string(
            r#"(setq x '(:first-name "Bob"
                          :age 25
                          :addr ("Main St" "Oak Ave")
                          :place "Office"))"#,
        )?;
        let x = ctx.eval_string("x")?;

        // The default `from_plist` shortcut uses DummyEval.
        let p = Person::from_plist(&mut ctx, &x)?;
        assert_eq!(p.name, "Bob");
        assert_eq!(p.age, 25);
        assert_eq!(p.addr, vec!["Main St".to_string(), "Oak Ave".to_string()]);
        assert_eq!(p.place.as_deref(), Some("Office"));
        // `:edu` is omitted — its default kicks in.
        assert_eq!(p.education, None);

        Ok(())
    }
}
