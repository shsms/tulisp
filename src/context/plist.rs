use std::ops::Deref;

use crate::{Error, TulispContext, TulispObject};

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

pub trait Plistable {
    fn from_plist(ctx: &mut TulispContext, obj: &TulispObject) -> Result<Self, Error>
    where
        Self: Sized;
}

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

        ctx.add_function("get-name", |person: Plist<Person>| person.name.clone())
            .add_function("get-age", |p: Plist<Person>| -> i64 { p.age })
            .add_function("get-ans", |p: Plist<Person>| -> i64 { p.answer })
            .add_function("get-place", |p: Plist<Person>| p.place.clone().unwrap())
            .add_function("get-edu", |p: Plist<Person>| {
                p.education.clone().unwrap_or("Unknown".to_string())
            })
            .add_function("get-addr", |p: Plist<Person>| {
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
