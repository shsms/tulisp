//! Property-list (plist) primitives and the typed [`Plistable`] layer.
//!
//! A plist is a flat alternating-key/value list, e.g. `(:a 1 :b 2)`.
//! See [the Emacs Lisp manual] for the canonical reference.
//!
//! [the Emacs Lisp manual]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Property-Lists.html

use std::ops::Deref;

use crate::{DummyEval, Error, Eval, Evaluator, TulispContext, TulispObject};

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
pub struct Plist<T: Plistable<Eval>> {
    plist: T,
}

impl<T> Plist<T>
where
    T: Plistable<Eval>,
{
    pub(crate) fn new(ctx: &mut TulispContext, obj: &TulispObject) -> Result<Self, Error> {
        Ok(Self {
            plist: <T as Plistable<Eval>>::from_plist(ctx, obj)?,
        })
    }

    pub fn into_inner(self) -> T {
        self.plist
    }
}

impl<T> Deref for Plist<T>
where
    T: Plistable<Eval>,
{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.plist
    }
}

/// Conversion between a Rust struct and a Lisp plist.
///
/// Parameterized over an [`Evaluator`] strategy `E`:
///
/// - `Plistable<DummyEval>` (the default) treats values in the plist as
///   already-evaluated lisp values and converts them as-is. Use this
///   when the plist comes from a free variable, a literal, or any
///   value already produced by the interpreter.
/// - `Plistable<Eval>` re-evaluates each value before conversion.
///   `Plist<T>` (the [`defun`](crate::TulispContext::defun) argument
///   wrapper) uses this strategy because the argument list reaches
///   `Plistable` unevaluated.
///
/// The [`AsPlist!`](macro@crate::AsPlist) macro generates a blanket
/// `impl<E: Evaluator> Plistable<E>` for the given struct, so a single
/// derivation covers both strategies.
///
/// # Call-site syntax
///
/// Because the macro generates impls for every `E`, a bare
/// `T::from_plist(...)` call is ambiguous. Name the trait explicitly:
///
/// ```ignore
/// // Already-evaluated plist (free variable, literal, etc.) — default:
/// let cfg = <MyType as Plistable>::from_plist(&mut ctx, &obj)?;
///
/// // Or, if values are unevaluated forms that should be evaluated:
/// let cfg = <MyType as Plistable<Eval>>::from_plist(&mut ctx, &obj)?;
/// ```
pub trait Plistable<E: Evaluator = DummyEval> {
    /// Deserialize `obj` (a Lisp plist) into `Self` using `E` to resolve
    /// each value.
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

    (@extract-field $ctx:ident, $value:ident, None) => {
        Some(<E as $crate::Evaluator>::eval($ctx, $value)?.into_owned().try_into()?)
    };

    (@extract-field $ctx:ident, $value:ident, Some($e: expr)) => {
        Some(<E as $crate::Evaluator>::eval($ctx, $value)?.into_owned().try_into()?)
    };

    (@extract-field $ctx:ident, $value:ident, $e: expr) => {
        <E as $crate::Evaluator>::eval($ctx, $value)?.into_owned().try_into()?
    };

    (@extract-field $ctx:ident, $value:ident) => {
        <E as $crate::Evaluator>::eval($ctx, $value)?.into_owned().try_into()?
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

        impl<E: $crate::Evaluator> $crate::Plistable<E> for $struct_name {
            fn from_plist(
                ctx: &mut TulispContext, plist: &$crate::TulispObject
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

                let plist = plist.base_iter().collect::<Vec<_>>();

                let (kvpairs, extra) = plist.as_chunks::<2>();

                if !extra.is_empty() {
                    return Err($crate::Error::plist_error(
                        "Expected an even number of items in the plist",
                    ));
                }

                let mut builder = Builder::default();

                for [key, value] in kvpairs {
                    $(if key.eq(&symbols.$field) {
                        builder.$field = Some($crate::AsPlist!(@extract-field ctx, value $(, $($default)+)?));
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
        DummyEval, Error, Plist, Plistable, TulispContext,
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
        // values. Converting it via `Plistable<DummyEval>` (the default)
        // should pass each value straight through without
        // re-evaluating — so a list field like `addr` resolves cleanly
        // instead of erroring on "calling string as function".
        let mut ctx = TulispContext::new();
        ctx.eval_string(
            r#"(setq x '(:first-name "Bob"
                          :age 25
                          :addr ("Main St" "Oak Ave")
                          :place "Office"))"#,
        )?;
        let x = ctx.eval_string("x")?;

        // Default-evaluator (DummyEval) form via the trait alias.
        // `Person::from_plist(...)` would be ambiguous because `Person`
        // implements `Plistable<E>` for every `E`, so name the trait
        // explicitly to pick up the trait-level default.
        let p = <Person as Plistable>::from_plist(&mut ctx, &x)?;
        assert_eq!(p.name, "Bob");
        assert_eq!(p.age, 25);
        assert_eq!(p.addr, vec!["Main St".to_string(), "Oak Ave".to_string()]);
        assert_eq!(p.place.as_deref(), Some("Office"));
        // `:edu` is omitted — its default kicks in.
        assert_eq!(p.education, None);

        // Explicit DummyEval is equivalent.
        let p2 = <Person as Plistable<DummyEval>>::from_plist(&mut ctx, &x)?;
        assert_eq!(p2.name, "Bob");

        Ok(())
    }
}
