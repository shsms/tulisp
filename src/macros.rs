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
        $item.deep_copy().and_then(|__ret| $ret.append(__ret))
    };
    (@push $ret:ident, $item:expr, $($items:tt)+) => {
        $crate::list!(@push $ret, $item).and_then(|__ret|
        $crate::list!(@push __ret, $($items)+))
    };
    (@push $ret:ident, @ $item:expr, $($items:tt)+) => {
        $crate::list!(@push $ret, @ $item).and_then(|__ret|
        $crate::list!(@push __ret, $($items)+))
    };
    (, $($items:tt)+) => { $crate::list!($($items)+) };
    ($($items:tt)+) => {{
        let __ret = $crate::TulispObject::nil();
        $crate::list!(@push __ret, $($items)+)
            .and_then(|__ret| Ok(__ret.to_owned()))
    }};
    () => { $crate::TulispObject::nil() }
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
        if !$vv.consp() {
            return Err($crate::Error::too_few_arguments());
        }
        let $var = $vv.car()?;
        let $vv = $vv.cdr()?;
    };
    (@reqr $vv:ident, $var:ident $($vars:tt)+) => {
        $crate::destruct_bind!(@reqr $vv, $var);
        $crate::destruct_bind!(@reqr $vv, $($vars)+);
    };
    (@reqr $vv:ident,) => {};
    (@no-rest $vv:ident) => {
        if !$vv.null() {
            return Err($crate::Error::too_many_arguments());
        }
    };
    (@rest $rest:ident $vv:ident) => {
        let $rest = $vv;
    };
    (@optvar $vv:ident, $var:ident) => {
        let ($var, $vv) = if !$vv.null() {
            ($vv.car()?, $vv.cdr()?)
        } else {
            ($crate::TulispObject::nil(), $crate::TulispObject::nil())
        };
    };
    (@optvar $vv:ident, $var:ident $($vars:ident)+) => {
        $crate::destruct_bind!(@optvar $vv, $var);
        $crate::destruct_bind!(@optvar $vv, $($vars)+)
    };
    (@impl ($($vars:ident)+) = $vv:ident) => {
        $crate::destruct_bind!(@reqr $vv, $($vars)+);
        $crate::destruct_bind!(@no-rest $vv);
    };
    (@impl ($($vars:ident)* &optional $($optvars:ident)+) = $vv:ident) => {
	$crate::destruct_bind!(@reqr $vv, $($vars)*);
        $crate::destruct_bind!(@optvar $vv, $($optvars)+);
        $crate::destruct_bind!(@no-rest $vv);
    };
    (@impl ($($vars:ident)* &rest $rest:ident) = $vv:ident) => {
	$crate::destruct_bind!(@reqr $vv, $($vars)*);
        $crate::destruct_bind!(@rest $rest $vv);
    };
    (@impl ($($vars:ident)* &optional $($optvars:ident)+ &rest $rest:ident) = $vv:ident) => {
	$crate::destruct_bind!(@reqr $vv, $($vars)*);
        $crate::destruct_bind!(@optvar $vv, $($optvars)+);
        $crate::destruct_bind!(@rest $rest $vv);
    };
    (($($rest:tt)*) = $vv:ident) => {
        $crate::destruct_bind!(@impl ($($rest)*) = $vv);
    };
}

#[macro_export]
macro_rules! destruct_eval_bind {
    (@reqr $ctx:ident, $vv:ident, $var:ident) => {
        if !$vv.consp() {
            return Err($crate::Error::too_few_arguments());
        }
        let $var = $vv.car_and_then(|__x| $ctx.eval(__x))?;
        let $vv = $vv.cdr()?;
    };
    (@reqr $ctx:ident, $vv:ident, $var:ident $($vars:tt)+) => {
        $crate::destruct_eval_bind!(@reqr $ctx, $vv, $var);
        $crate::destruct_eval_bind!(@reqr $ctx, $vv, $($vars)+);
    };
    (@reqr $ctx:ident, $vv:ident,) => {};
    (@no-rest $ctx:ident, $vv:ident) => {
        if !$vv.null() {
            return Err($crate::Error::too_many_arguments());
        }
    };
    (@rest $ctx:ident, $rest:ident $vv:ident) => {
        let $rest = $ctx.eval_each(&$vv)?;
    };
    (@optvar $ctx:ident, $vv:ident, $var:ident) => {
        let ($var, $vv) = if !$vv.null() {
            ($vv.car_and_then(|__x| $ctx.eval(__x))?, $vv.cdr()?)
        } else {
            ($crate::TulispObject::nil(), $crate::TulispObject::nil())
        };
    };
    (@optvar $ctx:ident, $vv:ident, $var:ident $($vars:ident)+) => {
        $crate::destruct_eval_bind!(@optvar $ctx, $vv, $var);
        $crate::destruct_eval_bind!(@optvar $ctx, $vv, $($vars)+)
    };
    (@impl $ctx:ident, ($($vars:ident)+) = $vv:ident) => {
        $crate::destruct_eval_bind!(@reqr $ctx, $vv, $($vars)+);
        $crate::destruct_eval_bind!(@no-rest $ctx, $vv);
    };
    (@impl $ctx:ident, ($($vars:ident)* &optional $($optvars:ident)+) = $vv:ident) => {
	$crate::destruct_eval_bind!(@reqr $ctx, $vv, $($vars)*);
        $crate::destruct_eval_bind!(@optvar $ctx, $vv, $($optvars)+);
        $crate::destruct_eval_bind!(@no-rest $ctx, $vv);
    };
    (@impl $ctx:ident, ($($vars:ident)* &rest $rest:ident) = $vv:ident) => {
	$crate::destruct_eval_bind!(@reqr $ctx, $vv, $($vars)*);
        $crate::destruct_eval_bind!(@rest $ctx, $rest $vv);
    };
    (@impl $ctx:ident, ($($vars:ident)* &optional $($optvars:ident)+ &rest $rest:ident) = $vv:ident) => {
	$crate::destruct_eval_bind!(@reqr $ctx, $vv, $($vars)*);
        $crate::destruct_eval_bind!(@optvar $ctx, $vv, $($optvars)+);
        $crate::destruct_eval_bind!(@rest $ctx, $rest $vv);
    };
    ($ctx:ident, ($($rest:tt)*) = $vv:ident) => {
        $crate::destruct_eval_bind!(@impl $ctx, ($($rest)*) = $vv);
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
         $($name:ident : $symbol:expr),+ $(,)?
     }) => {
        $( #[$meta] )*
        $vis struct $struct_name {
            $(pub $name: $crate::TulispObject),+
        }

        impl $struct_name {
            fn new(__ctx: &mut $crate::TulispContext) -> Self {
                $struct_name {
                    $($name: __ctx.intern($symbol),)+
                }
            }
        }
    };

    ($ctx: ident => {$($name:ident : $symbol:expr),+ $(,)?}) => {{
        $crate::intern!(pub(crate) struct Keywords {$($name : $symbol),+});
        Keywords::new($ctx)
    }};
}

#[cfg(test)]
mod tests {
    // Constants spelled like the generated bindings must not capture
    // them: an identifier pattern resolves to a constant in scope.
    #[allow(non_upper_case_globals, dead_code)]
    mod beside_constants {
        use crate::{Error, TulispContext, TulispObject};

        const ret: &str = "";
        const x: &str = "";

        pub fn second(ctx: &mut TulispContext) -> Result<TulispObject, Error> {
            let args = crate::list!(,TulispObject::from(1) ,TulispObject::from(2))?;
            crate::destruct_eval_bind!(ctx, (a b) = args);
            let _ = a;
            Ok(b)
        }
    }

    #[test]
    fn a_list_and_destructure_beside_same_named_constants_compile() {
        let mut ctx = crate::TulispContext::new();
        assert_eq!(beside_constants::second(&mut ctx).unwrap().to_string(), "2");
    }
}
