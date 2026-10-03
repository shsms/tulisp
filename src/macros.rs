/**
Builds a list, with a backquote-like syntax.

Each element is written after a `,` (optional before the first), and
`,@x` splices in the elements of `x`. The result is
`Result<TulispObject, Error>`: splicing fails for a value that is not a
proper list.

- `list!(...)` converts each element with `Into<TulispObject>`.
- `list!(ctx => ...)`, with `ctx` a `&mut TulispContext` binding,
  converts each element with [`TulispConvertible`](crate::TulispConvertible),
  and also takes string literals and `&TulispObject`. An element may
  use `ctx` itself.

`,@x` takes a list (`TulispObject` or `&TulispObject`), or anything
iterable, such as a `Vec<T>`, a [`Rest<T>`](crate::Rest) or an array.
A list's cons cells are always copied, even in the last position, and
its elements are shared; anything else has each item converted.

Each element is evaluated and added before the next one is evaluated,
so `?` in an element returns from the enclosing function.

## Example

```rust
use tulisp::{list, Error, TulispContext, TulispObject};

fn main() -> Result<(), Error> {
    let numbers = list!(,10 ,"hello" ,5.2)?;
    assert_eq!(numbers.to_string(), r#"(10 "hello" 5.2)"#);

    let spliced = list!(,20 ,@&numbers ,numbers.clone() ,@vec![1, 2])?;
    assert_eq!(
        spliced.to_string(),
        r#"(20 10 "hello" 5.2 (10 "hello" 5.2) 1 2)"#
    );

    let ctx = &mut TulispContext::new();
    let form = list!(ctx => ,ctx.intern("setq") ,ctx.intern("x") ,1)?;
    assert_eq!(form.to_string(), "(setq x 1)");
    Ok(())
}
```
*/
#[macro_export]
macro_rules! list {
    // The plain arm's elements, one at a time.
    (@__plain $l:lifetime $b:ident;) => {};
    (@__plain $l:lifetime $b:ident; , $($rest:tt)*) => {
        $crate::list!(@__plain $l $b; $($rest)*)
    };
    (@__plain $l:lifetime $b:ident; @ $item:expr $(, $($rest:tt)*)?) => {
        if let ::core::result::Result::Err(__e) = $crate::Splice::splice_into($item, &mut $b) {
            break $l ::core::result::Result::Err(__e);
        }
        $crate::list!(@__plain $l $b; $($($rest)*)?)
    };
    (@__plain $l:lifetime $b:ident; $item:expr $(, $($rest:tt)*)?) => {
        $b.push(::core::convert::Into::<$crate::TulispObject>::into($item));
        $crate::list!(@__plain $l $b; $($($rest)*)?)
    };

    // The `ctx =>` arm's elements, one at a time.
    (@__ctx $ctx:ident $l:lifetime $b:ident;) => {};
    (@__ctx $ctx:ident $l:lifetime $b:ident; , $($rest:tt)*) => {
        $crate::list!(@__ctx $ctx $l $b; $($rest)*)
    };
    (@__ctx $ctx:ident $l:lifetime $b:ident; @ $item:expr $(, $($rest:tt)*)?) => {
        if let ::core::result::Result::Err(__e) =
            $crate::SpliceWithContext::splice_into($item, $ctx, &mut $b)
        {
            break $l ::core::result::Result::Err(__e);
        }
        $crate::list!(@__ctx $ctx $l $b; $($($rest)*)?)
    };
    (@__ctx $ctx:ident $l:lifetime $b:ident; $item:expr $(, $($rest:tt)*)?) => {
        let __item = $crate::ListItem::into_item($item, $ctx);
        $b.push(__item);
        $crate::list!(@__ctx $ctx $l $b; $($($rest)*)?)
    };

    () => {
        ::core::result::Result::<$crate::TulispObject, $crate::Error>::Ok(
            $crate::TulispObject::nil(),
        )
    };
    ($ctx:ident => $($items:tt)*) => {
        '__tulisp_list: {
            // `ctx` must be a `&mut TulispContext`, even with no element.
            let _: &mut $crate::TulispContext = $ctx;
            let mut __list = $crate::ListMaker::new();
            $crate::list!(@__ctx $ctx '__tulisp_list __list; $($items)*);
            break '__tulisp_list ::core::result::Result::<$crate::TulispObject, $crate::Error>::Ok(
                __list.finish(),
            )
        }
    };
    ($($items:tt)+) => {
        '__tulisp_list: {
            let mut __list = $crate::ListMaker::new();
            $crate::list!(@__plain '__tulisp_list __list; $($items)+);
            break '__tulisp_list ::core::result::Result::<$crate::TulispObject, $crate::Error>::Ok(
                __list.finish(),
            )
        }
    };
}

/// The list `list!` builds. Not part of the API.
#[doc(hidden)]
pub struct ListMaker(crate::cons::ListBuilder);

impl Default for ListMaker {
    fn default() -> Self {
        ListMaker(crate::cons::ListBuilder::new())
    }
}

impl ListMaker {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push(&mut self, item: crate::TulispObject) {
        self.0.push(item);
    }

    /// Adds each element of the proper list LIST, sharing them.
    pub fn splice(&mut self, list: &crate::TulispObject) -> Result<(), crate::Error> {
        self.0.push_all(list)
    }

    pub fn finish(self) -> crate::TulispObject {
        self.0.build()
    }
}

/// An element of `list!(ctx => ...)`. Not part of the API.
#[doc(hidden)]
pub trait ListItem {
    fn into_item(self, ctx: &mut crate::TulispContext) -> crate::TulispObject;
}

impl<T: crate::TulispConvertible> ListItem for T {
    fn into_item(self, ctx: &mut crate::TulispContext) -> crate::TulispObject {
        self.into_tulisp(ctx)
    }
}

impl ListItem for &str {
    fn into_item(self, _ctx: &mut crate::TulispContext) -> crate::TulispObject {
        self.into()
    }
}

impl ListItem for &crate::TulispObject {
    fn into_item(self, _ctx: &mut crate::TulispContext) -> crate::TulispObject {
        self.clone()
    }
}

/// What `,@` splices in `list!(...)`. Not part of the API.
#[doc(hidden)]
pub trait Splice {
    fn splice_into(self, list: &mut ListMaker) -> Result<(), crate::Error>;
}

impl Splice for crate::TulispObject {
    fn splice_into(self, list: &mut ListMaker) -> Result<(), crate::Error> {
        list.splice(&self)
    }
}

impl Splice for &crate::TulispObject {
    fn splice_into(self, list: &mut ListMaker) -> Result<(), crate::Error> {
        list.splice(self)
    }
}

impl<I: IntoIterator> Splice for I
where
    I::Item: Into<crate::TulispObject>,
{
    fn splice_into(self, list: &mut ListMaker) -> Result<(), crate::Error> {
        self.into_iter().for_each(|item| list.push(item.into()));
        Ok(())
    }
}

/// What `,@` splices in `list!(ctx => ...)`. Not part of the API.
#[doc(hidden)]
pub trait SpliceWithContext {
    fn splice_into(
        self,
        ctx: &mut crate::TulispContext,
        list: &mut ListMaker,
    ) -> Result<(), crate::Error>;
}

impl SpliceWithContext for crate::TulispObject {
    fn splice_into(
        self,
        _ctx: &mut crate::TulispContext,
        list: &mut ListMaker,
    ) -> Result<(), crate::Error> {
        list.splice(&self)
    }
}

impl SpliceWithContext for &crate::TulispObject {
    fn splice_into(
        self,
        _ctx: &mut crate::TulispContext,
        list: &mut ListMaker,
    ) -> Result<(), crate::Error> {
        list.splice(self)
    }
}

impl<I: IntoIterator> SpliceWithContext for I
where
    I::Item: ListItem,
{
    fn splice_into(
        self,
        ctx: &mut crate::TulispContext,
        list: &mut ListMaker,
    ) -> Result<(), crate::Error> {
        self.into_iter()
            .for_each(|item| list.push(item.into_item(ctx)));
        Ok(())
    }
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

`new` is as visible as the struct.

It can also be used to create an instance of the struct directly. The
context may be any expression that gives a `&mut TulispContext`:

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
            /// Interns each field's symbol in `__ctx`.
            $vis fn new(__ctx: &mut $crate::TulispContext) -> Self {
                $struct_name {
                    $($name: __ctx.intern($symbol),)+
                }
            }
        }
    };

    ($ctx:expr => {$($name:ident : $symbol:expr),+ $(,)?}) => {{
        $crate::intern!(pub(crate) struct Keywords {$($name : $symbol),+});
        Keywords::new($ctx)
    }};
}

#[cfg(test)]
mod tests {
    use crate::{Error, Rest, TulispContext, TulispObject};

    crate::AsSymbol! {
        #[derive(Clone, Copy)]
        enum Shape {
            Circle<"circle">,
            Square<"sq">,
        }
    }

    crate::AsList! {
        struct Cfg {
            size: i64,
        }
    }

    mod symbols {
        crate::intern! {
            pub struct Symbols {
                a: "a",
            }
        }
    }

    // `new` is as visible as the struct, so another module can call it.
    #[test]
    fn intern_new_is_as_visible_as_its_struct() {
        let ctx = &mut TulispContext::new();
        let symbols = symbols::Symbols::new(ctx);
        assert!(symbols.a.eq(&ctx.intern("a")));
    }

    // The context may be any expression that gives a
    // `&mut TulispContext`.
    #[test]
    fn intern_takes_any_expression_for_the_context() {
        struct Host {
            ctx: TulispContext,
        }
        let mut host = Host {
            ctx: TulispContext::new(),
        };
        let symbols = crate::intern!(&mut host.ctx => { a: "a" });
        assert!(symbols.a.eq(&host.ctx.intern("a")));
    }

    #[test]
    fn the_plain_arm_converts_each_element() -> Result<(), Error> {
        let l = list!(,10 ,"hello" ,5.2 ,true ,TulispObject::nil())?;
        assert_eq!(l.to_string(), r#"(10 "hello" 5.2 t nil)"#);
        // The first element needs no leading comma.
        assert_eq!(list!(1, 2)?.to_string(), "(1 2)");
        Ok(())
    }

    #[test]
    fn the_ctx_arm_converts_each_element() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();
        let obj = TulispObject::from(1);
        let l = list!(ctx =>
            ,"name" ,&obj ,10 ,5.2 ,true ,Shape::Circle ,Cfg { size: 3 }
            ,ctx.intern("x") ,@vec![Shape::Square, Shape::Circle]
        )?;
        assert_eq!(
            l.to_string(),
            r#"("name" 1 10 5.2 t circle (:size 3) x sq circle)"#
        );
        Ok(())
    }

    // `?` in an element returns from the function that holds the
    // `list!`, whatever its error type.
    fn second_of(s: &str) -> Result<TulispObject, std::num::ParseIntError> {
        let l = list!(,1 ,s.parse::<i64>()?);
        Ok(l.and_then(|l| l.cadr()).unwrap_or_default())
    }

    #[test]
    fn a_question_mark_in_an_element_returns_from_the_caller() {
        assert!(second_of("x").is_err());
        assert_eq!(second_of("7").unwrap().to_string(), "7");
    }

    #[test]
    fn a_spliced_list_is_copied_but_its_elements_are_shared() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();
        let inner = ctx.eval_string("(list (list 1) 2)")?;
        let l = list!(,0 ,@&inner ,@&inner)?;
        assert_eq!(l.to_string(), "(0 (1) 2 (1) 2)");
        assert!(l.cadr()?.eq(&inner.car()?));
        assert!(l.cadddr()?.eq(&inner.car()?));
        assert_eq!(inner.to_string(), "((1) 2)");
        // By value, too.
        assert_eq!(list!(,@inner ,3)?.to_string(), "((1) 2 3)");
        Ok(())
    }

    #[test]
    fn vecs_rests_arrays_and_nil_splice() -> Result<(), Error> {
        let rest: Rest<i64> = [5, 6].into_iter().collect();
        let l = list!(,@vec![1, 2] ,@[3, 4] ,@rest ,@TulispObject::nil() ,7)?;
        assert_eq!(l.to_string(), "(1 2 3 4 5 6 7)");
        Ok(())
    }

    #[test]
    fn splicing_a_non_list_is_an_error() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();
        assert!(list!(,@TulispObject::from(5)).is_err());
        let dotted = ctx.eval_string("'(1 . 2)")?;
        assert!(list!(,0 ,@dotted).is_err());
        let circular = ctx.eval_string("(let ((l (list 1 2))) (setcdr (cdr l) l) l)")?;
        assert!(list!(,@circular).is_err());
        Ok(())
    }

    #[test]
    fn borrowed_objects_go_in_either_arm() -> Result<(), Error> {
        let obj = TulispObject::from(1);
        let objs = vec![TulispObject::from(2), TulispObject::from(3)];
        assert_eq!(list!(,&obj ,@&objs)?.to_string(), "(1 2 3)");
        let ctx = &mut TulispContext::new();
        assert_eq!(list!(ctx => ,&obj ,@&objs)?.to_string(), "(1 2 3)");
        Ok(())
    }

    #[test]
    fn splicing_a_non_list_with_a_context_is_an_error() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();
        let dotted = ctx.eval_string("'(1 . 2)")?;
        assert!(list!(ctx => ,@TulispObject::from(5)).is_err());
        assert!(list!(ctx => ,0 ,@&dotted).is_err());
        Ok(())
    }

    #[test]
    fn an_empty_list_is_nil() -> Result<(), Error> {
        assert!(list!()?.null());
        let ctx = &mut TulispContext::new();
        assert!(list!(ctx =>)?.null());
        Ok(())
    }
}
