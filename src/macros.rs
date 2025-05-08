/**
Provides a lisp-like syntax for constructing lists.

## Example

```rust
use tulisp::{list, TulispObject, Error};

fn main() -> Result<(), Error> {
    // Create a list with 3 values inside.
    let list1 = list!(
        ,10.into()
        ,"hello".into()
        ,5.2.into()
    )?;
    assert_eq!(
        list1.to_string(),
        r#"(10 "hello" 5.2)"#
    );

    // Create a list that splices `list1` in the second pos.
    let list2 = list!(
        ,20.into()
        ,@list1.clone()
        ,list1
        ,"world".into()
    )?;
    assert_eq!(
        list2.to_string(),
        r#"(20 10 "hello" 5.2 (10 "hello" 5.2) "world")"#
    );

    Ok(())
}
```
*/
#[macro_export]
macro_rules! list {
    (@push $ret:ident, $item:expr) => {
        $ret.push($item)
    };
    (@push $ret:ident, @ $item:expr) => {
        $item.deep_copy().and_then(|ret| $ret.append(ret))
    };
    (@push $ret:ident, $item:expr, $($items:tt)+) => {
        list!(@push $ret, $item).and_then(|ret|
        list!(@push ret, $($items)+))
    };
    (@push $ret:ident, @ $item:expr, $($items:tt)+) => {
        list!(@push $ret, @ $item).and_then(|ret|
        list!(@push ret, $($items)+))
    };
    (, $($items:tt)+) => { list!($($items)+) };
    ($($items:tt)+) => {{
	let ret = TulispObject::nil();
        list!(@push ret, $($items)+)
            .and_then(|ret| Ok(ret.to_owned()))
    }};
    () => { TulispObject::nil() }
}

/**
Destructures lists and binds the components to separate symbols.

Has a syntax similar to emacs lisp defun parameters.

## Example

```rust
use tulisp::{destruct_bind, list, TulispObject, Error, ErrorKind};

fn main() -> Result<(), Error> {
    // Destruct from a list with just the required item present.
    let list1 = list!(
        ,10.into()
    )?;
    destruct_bind!((num1 &optional str1 num2) = list1);

    assert_eq!(num1.as_int()?, 10);
    assert!(str1.null());
    assert!(num2.null());

    // Destruct from a list with just the required items present.
    let list1 = list!(
        ,10.into()
        ,"hello".into()
        ,5.2.into()
    )?;
    destruct_bind!((num1 str1 num2 &rest other) = list1);

    assert_eq!(num1.as_int()?, 10);
    assert_eq!(str1.as_string()?, "hello");
    assert_eq!(num2.as_float()?, 5.2);
    assert!(other.null());

    // Destruct from a list with all values present.
    let list1 = list!(
        ,10.into()
        ,"hello".into()
        ,5.2.into()
        ,22.into()
        ,42.into()
    )?;
    destruct_bind!((num1 &optional str1 num2 &rest other) = list1);

    assert_eq!(num1.as_int()?, 10);
    assert_eq!(str1.as_string()?, "hello");
    assert_eq!(num2.as_float()?, 5.2);
    assert_eq!(other.to_string(), "(22 42)");

    Ok(())
}
 ```
*/
#[macro_export]
macro_rules! destruct_bind {
    (@reqr $vv:ident, $var:ident) => {
        let $var = $vv.car()?;
        let $vv = $vv.cdr()?;
    };
    (@reqr $vv:ident, $var:ident $($vars:tt)+) => {
        destruct_bind!(@reqr $vv, $var);
        destruct_bind!(@reqr $vv, $($vars)+);
    };
    (@reqr $vv:ident,) => {};
    (@no-rest $vv:ident) => {
        if !$vv.null() {
            return Err(Error::new(ErrorKind::TypeMismatch,"Too many arguments".to_string()));
        }
    };
    (@rest $rest:ident $vv:ident) => {
        let $rest = $vv;
    };
    (@optvar $vv:ident, $var:ident) => {
        let ($var, $vv) = if !$vv.null() {
            ($vv.car()?, $vv.cdr()?)
        } else {
            (TulispObject::nil(), TulispObject::nil())
        };
    };
    (@optvar $vv:ident, $var:ident $($vars:ident)+) => {
        destruct_bind!(@optvar $vv, $var);
        destruct_bind!(@optvar $vv, $($vars)+)
    };
    (@impl ($($vars:ident)+) = $vv:ident) => {
        destruct_bind!(@reqr $vv, $($vars)+);
        destruct_bind!(@no-rest $vv);
    };
    (@impl ($($vars:ident)* &optional $($optvars:ident)+) = $vv:ident) => {
	destruct_bind!(@reqr $vv, $($vars)*);
        destruct_bind!(@optvar $vv, $($optvars)+);
        destruct_bind!(@no-rest $vv);
    };
    (@impl ($($vars:ident)* &rest $rest:ident) = $vv:ident) => {
	destruct_bind!(@reqr $vv, $($vars)*);
        destruct_bind!(@rest $rest $vv);
    };
    (@impl ($($vars:ident)* &optional $($optvars:ident)+ &rest $rest:ident) = $vv:ident) => {
	destruct_bind!(@reqr $vv, $($vars)*);
        destruct_bind!(@optvar $vv, $($optvars)+);
        destruct_bind!(@rest $rest $vv);
    };
    (($($rest:tt)*) = $vv:ident) => {
        destruct_bind!(@impl ($($rest)*) = $vv);
    };
}

/**
Creates a struct that holds interned symbols.

## Example

```rust
use tulisp::{TulispContext, intern};

intern!{
    #[derive(Clone)]
    pub(crate) struct Keywords {
        name: ":name",
        scale: ":scale",
        pos: ":pos",
    }
}


let ctx = &mut TulispContext::new();

let kw = Keywords::new(ctx);

assert!(kw.name.eq(&ctx.intern(":name")));
assert!(kw.scale.eq(&ctx.intern(":scale")));
assert!(kw.pos.eq(&ctx.intern(":pos")));
```

It can also be used to create an instance of the struct directly:

```rust
use tulisp::{TulispContext, intern};

let ctx = &mut TulispContext::new();

let kw = intern!(ctx => {
    name: ":name",
    scale: ":scale",
    pos: ":pos",
});

assert!(kw.name.eq(&ctx.intern(":name")));
assert!(kw.scale.eq(&ctx.intern(":scale")));
assert!(kw.pos.eq(&ctx.intern(":pos")));
```
*/
#[macro_export]
macro_rules! intern {
    ($( #[$meta:meta] )*
     $vis:vis struct $struct_name:ident {
         $($name:ident : $symbol:literal),+ $(,)?
     }) => {
        $( #[$meta] )*
        $vis struct $struct_name {
            $(pub $name: $crate::TulispObject),+
        }

        impl $struct_name {
            fn new(ctx: &mut $crate::TulispContext) -> Self {
                let ret = $struct_name {
                    $($name: ctx.intern($symbol),)+
                };
                ret
            }
        }
    };

    ($ctx: ident => {$($name:ident : $symbol:literal),+ $(,)?}) => {{
        intern!(pub(crate) struct Keywords {$($name : $symbol),+});
        Keywords::new($ctx)
    }};
}
