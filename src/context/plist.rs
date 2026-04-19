use std::ops::Deref;

use crate::{Error, TulispContext, TulispObject};

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
    pub(crate) fn new(ctx: &mut TulispContext, obj: &TulispObject) -> Result<Self, Error> {
        Ok(Self {
            plist: T::from_plist(ctx, obj)?,
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
/// Implement this trait to use a struct as the argument type of a
/// [`defun`](crate::TulispContext::defun)-registered function via [`Plist<T>`].
///
/// The [`AsPlist!`](macro@crate::AsPlist) macro generates this implementation
/// automatically from a struct definition.
pub trait Plistable {
    /// Deserialize `obj` (a Lisp plist) into `Self`.
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
        Some($ctx.eval($value)?.try_into()?)
    };

    (@extract-field $ctx:ident, $value:ident, Some($e: expr)) => {
        Some($ctx.eval($value)?.try_into()?)
    };

    (@extract-field $ctx:ident, $value:ident, $e: expr) => {
        $ctx.eval($value)?.try_into()?
    };

    (@extract-field $ctx:ident, $value:ident) => {
        $ctx.eval($value)?.try_into()?
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

                $crate::lists::plist_from([
                    $((symbols.$field.clone(), self.$field.into())),+
                ])
            }
        }
     };
}

#[cfg(test)]
mod tests {
    use crate::{
        Error, Plist,
        context::TulispContext,
        test_utils::{eval_assert_equal, eval_assert_error},
    };

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
    fn test_plist() -> Result<(), Error> {
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
}
