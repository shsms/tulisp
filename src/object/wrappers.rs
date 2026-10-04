use crate::{Error, TulispContext, TulispObject};

/// The body of a macro registered with [`defmacro`](TulispContext::defmacro):
/// it gets the call's arguments, unevaluated, as a list, and returns the form
/// to run in the call's place. With the `sync` feature it must be `Send` and
/// `Sync`.
///
/// ```rust
/// use tulisp::{TulispContext, TulispFn};
///
/// fn register(ctx: &mut TulispContext, name: &str, body: impl TulispFn) {
///     ctx.defmacro(name, body);
/// }
///
/// let mut ctx = TulispContext::new();
/// register(&mut ctx, "first-arg", |_, args| args.car());
/// assert_eq!(ctx.eval_string("(first-arg 7 8)").unwrap().to_string(), "7");
/// ```
pub trait TulispFn:
    Fn(&mut TulispContext, &TulispObject) -> Result<TulispObject, Error>
    + generic::SendSyncIfSync
    + 'static
{
}
impl<T> TulispFn for T where
    T: Fn(&mut TulispContext, &TulispObject) -> Result<TulispObject, Error>
        + generic::SendSyncIfSync
        + 'static
{
}

/// The closure behind a `ctx.defun`-registered function. It gets its
/// arguments as values, evaluated by the caller or passed from Rust, as
/// a slice. It may call back into `ctx`.
pub trait DefunFn:
    Fn(&mut TulispContext, &[TulispObject]) -> Result<TulispObject, Error>
    + generic::SendSyncIfSync
    + 'static
{
}
impl<T> DefunFn for T where
    T: Fn(&mut TulispContext, &[TulispObject]) -> Result<TulispObject, Error>
        + generic::SendSyncIfSync
        + 'static
{
}

/// A check the host gives [`TulispContext::set_interrupt_check`]: a closure
/// that returns an [`Interrupt`](crate::Interrupt), or a `bool`, to stop the
/// running evaluation. With the `sync` feature it must be `Send`, as it moves
/// with the context, but need not be `Sync`, as only the context calls it.
///
/// ```rust
/// use tulisp::{InterruptCheckFn, TulispContext};
///
/// fn install(ctx: &mut TulispContext, check: impl InterruptCheckFn<bool>) {
///     ctx.set_interrupt_check(check);
/// }
///
/// let mut ctx = TulispContext::new();
/// install(&mut ctx, || false);
/// ```
pub trait InterruptCheckFn<R>: FnMut() -> R + generic::SendIfSync + 'static {}
impl<T, R> InterruptCheckFn<R> for T where T: FnMut() -> R + generic::SendIfSync + 'static {}

/// The closure of a special form: the evaluated arguments, and the
/// unevaluated ones as forms, each in order.
pub trait SpecialFn:
    Fn(&mut TulispContext, &[TulispObject], Vec<crate::Form>) -> Result<TulispObject, Error>
    + generic::SendSyncIfSync
    + 'static
{
}
impl<T> SpecialFn for T where
    T: Fn(&mut TulispContext, &[TulispObject], Vec<crate::Form>) -> Result<TulispObject, Error>
        + generic::SendSyncIfSync
        + 'static
{
}

#[cfg(not(feature = "sync"))]
pub mod generic {
    use std::ops::Deref;

    use crate::TulispAny;

    use super::*;

    /// `Send + Sync` with the `sync` feature, and no bound without it: what a
    /// value Tulisp shares, such as a [`TulispAny`] value or a registered
    /// closure, must be.
    ///
    /// ```rust
    /// fn keep<T: tulisp::SendSyncIfSync + 'static>(_: T) {}
    /// keep(5);
    /// ```
    pub trait SendSyncIfSync {}
    impl<T> SendSyncIfSync for T {}

    /// `Send` with the `sync` feature, and no bound without it: what a value
    /// only the context calls, such as an interrupt check, must be.
    ///
    /// ```rust
    /// fn keep<T: tulisp::SendIfSync + 'static>(_: T) {}
    /// keep(5);
    /// ```
    pub trait SendIfSync {}
    impl<T> SendIfSync for T {}

    /// A shared pointer: an `Rc` without the `sync` feature, and an `Arc` with
    /// it. A [`TulispAny`] value goes into a Lisp object through one, and
    /// [`TulispObject::downcast`] gives it back as one.
    #[repr(transparent)]
    #[derive(Debug)]
    pub struct Shared<T: ?Sized>(std::rc::Rc<T>);

    pub type SharedRef<'a, T> = std::cell::Ref<'a, T>;

    impl<T: ?Sized + std::fmt::Display> std::fmt::Display for Shared<T> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.0)
        }
    }

    impl<T: ?Sized> Clone for Shared<T> {
        fn clone(&self) -> Self {
            Self(self.0.clone())
        }
    }

    impl Shared<dyn TulispAny> {
        pub(crate) fn new_tulisp_fn(val: impl TulispFn) -> Shared<dyn TulispFn> {
            Shared(std::rc::Rc::new(val))
        }

        pub(crate) fn new_defun_fn(val: impl DefunFn) -> Shared<dyn DefunFn> {
            Shared(std::rc::Rc::new(val))
        }

        pub(crate) fn new_special_fn(val: impl SpecialFn) -> Shared<dyn SpecialFn> {
            Shared(std::rc::Rc::new(val))
        }

        pub(crate) fn downcast<U: TulispAny + 'static>(
            self,
        ) -> Result<Shared<U>, Shared<dyn TulispAny>> {
            match std::rc::Rc::downcast::<U>(self.0.clone()) {
                Ok(v) => Ok(Shared(v)),
                Err(_) => Err(Shared(self.0)),
            }
        }
    }

    impl<T: ?Sized> Deref for Shared<T> {
        type Target = T;

        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }

    impl<T> Shared<T> {
        /// A typed shared handle; `.into()` erases it into a Lisp value.
        pub fn new(val: T) -> Self {
            Shared(std::rc::Rc::new(val))
        }
    }

    impl<T: TulispAny> Shared<T> {
        /// The handle as a type-erased value, pointing at the same
        /// allocation.
        pub(crate) fn into_any(self) -> Shared<dyn TulispAny> {
            Shared(self.0)
        }
    }

    impl<T: ?Sized> Shared<T> {
        /// True if both point at the same allocation.
        pub fn ptr_eq(&self, other: &Self) -> bool {
            std::rc::Rc::ptr_eq(&self.0, &other.0)
        }

        /// Address of the allocation, for identity hashing.
        pub(crate) fn addr_as_usize(&self) -> usize {
            std::rc::Rc::as_ptr(&self.0) as *const () as usize
        }
    }

    /// A shared cell that can change: an `Rc<RefCell<T>>` without the `sync`
    /// feature, and an `Arc<RwLock<T>>` with it. A [`TulispAny`] value can keep
    /// changing state in one and work in both builds.
    #[repr(transparent)]
    #[derive(Debug)]
    pub struct SharedMut<T>(std::rc::Rc<std::cell::RefCell<T>>);

    // `#[derive(Clone)]` would synthesize `T: Clone` — but `Rc` clones
    // without that bound, and callers rely on sharing non-Clone inner
    // types (terminals, sockets, …).
    impl<T> Clone for SharedMut<T> {
        fn clone(&self) -> Self {
            SharedMut(self.0.clone())
        }
    }

    impl<T> SharedMut<T> {
        pub fn new(val: T) -> Self {
            SharedMut(std::rc::Rc::new(std::cell::RefCell::new(val)))
        }
        pub fn ptr_eq(&self, other: &Self) -> bool {
            std::rc::Rc::ptr_eq(&self.0, &other.0)
        }

        pub fn borrow(&self) -> std::cell::Ref<'_, T> {
            self.0.borrow()
        }

        pub fn borrow_mut(&self) -> std::cell::RefMut<'_, T> {
            self.0.borrow_mut()
        }

        pub(crate) fn addr_as_usize(&self) -> usize {
            self.0.as_ptr() as usize
        }

        pub(crate) fn strong_count(&self) -> usize {
            std::rc::Rc::strong_count(&self.0)
        }

        /// The value, when this is its only reference.
        pub(crate) fn get_mut(&mut self) -> Option<&mut T> {
            std::rc::Rc::get_mut(&mut self.0).map(std::cell::RefCell::get_mut)
        }
    }

    impl<T: Default> Default for SharedMut<T> {
        fn default() -> Self {
            SharedMut::new(T::default())
        }
    }
}

#[cfg(feature = "sync")]
pub mod generic {
    use std::ops::Deref;

    use crate::TulispAny;

    use super::*;

    /// `Send + Sync` with the `sync` feature, and no bound without it: what a
    /// value Tulisp shares, such as a [`TulispAny`] value or a registered
    /// closure, must be.
    ///
    /// ```rust
    /// fn keep<T: tulisp::SendSyncIfSync + 'static>(_: T) {}
    /// keep(5);
    /// ```
    pub trait SendSyncIfSync: Sync + Send {}
    impl<T> SendSyncIfSync for T where T: Send + Sync {}

    /// `Send` with the `sync` feature, and no bound without it: what a value
    /// only the context calls, such as an interrupt check, must be.
    ///
    /// ```rust
    /// fn keep<T: tulisp::SendIfSync + 'static>(_: T) {}
    /// keep(5);
    /// ```
    pub trait SendIfSync: Send {}
    impl<T> SendIfSync for T where T: Send {}

    /// A shared pointer: an `Rc` without the `sync` feature, and an `Arc` with
    /// it. A [`TulispAny`] value goes into a Lisp object through one, and
    /// [`TulispObject::downcast`] gives it back as one.
    #[repr(transparent)]
    #[derive(Debug)]
    pub struct Shared<T: ?Sized>(std::sync::Arc<T>);

    pub type SharedRef<'a, T> = std::sync::RwLockReadGuard<'a, T>;

    impl<T: ?Sized + std::fmt::Display> std::fmt::Display for Shared<T> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.0)
        }
    }

    impl<T: ?Sized> Clone for Shared<T> {
        fn clone(&self) -> Self {
            Self(self.0.clone())
        }
    }

    impl Shared<dyn TulispAny> {
        pub(crate) fn new_tulisp_fn(val: impl TulispFn) -> Shared<dyn TulispFn> {
            Shared(std::sync::Arc::new(val))
        }

        pub(crate) fn new_defun_fn(val: impl DefunFn) -> Shared<dyn DefunFn> {
            Shared(std::sync::Arc::new(val))
        }

        pub(crate) fn new_special_fn(val: impl SpecialFn) -> Shared<dyn SpecialFn> {
            Shared(std::sync::Arc::new(val))
        }

        pub(crate) fn downcast<U: TulispAny + 'static>(
            self,
        ) -> Result<Shared<U>, Shared<dyn TulispAny>> {
            match std::sync::Arc::downcast::<U>(self.0.clone()) {
                Ok(v) => Ok(Shared(v)),
                Err(_) => Err(Shared(self.0)),
            }
        }
    }

    impl<T: ?Sized> Deref for Shared<T> {
        type Target = T;

        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }

    impl<T> Shared<T> {
        /// A typed shared handle; `.into()` erases it into a Lisp value.
        pub fn new(val: T) -> Self {
            Shared(std::sync::Arc::new(val))
        }
    }

    impl<T: TulispAny> Shared<T> {
        /// The handle as a type-erased value, pointing at the same
        /// allocation.
        pub(crate) fn into_any(self) -> Shared<dyn TulispAny> {
            Shared(self.0)
        }
    }

    impl<T: ?Sized> Shared<T> {
        /// True if both point at the same allocation.
        pub fn ptr_eq(&self, other: &Self) -> bool {
            std::sync::Arc::ptr_eq(&self.0, &other.0)
        }

        /// Address of the allocation, for identity hashing.
        pub(crate) fn addr_as_usize(&self) -> usize {
            std::sync::Arc::as_ptr(&self.0) as *const () as usize
        }
    }

    /// A shared cell that can change: an `Rc<RefCell<T>>` without the `sync`
    /// feature, and an `Arc<RwLock<T>>` with it. A [`TulispAny`] value can keep
    /// changing state in one and work in both builds.
    #[repr(transparent)]
    #[derive(Debug)]
    pub struct SharedMut<T>(std::sync::Arc<std::sync::RwLock<T>>);

    impl<T> Clone for SharedMut<T> {
        fn clone(&self) -> Self {
            SharedMut(self.0.clone())
        }
    }

    impl<T> SharedMut<T> {
        pub fn new(val: T) -> Self {
            SharedMut(std::sync::Arc::new(std::sync::RwLock::new(val)))
        }
        pub fn ptr_eq(&self, other: &Self) -> bool {
            std::sync::Arc::ptr_eq(&self.0, &other.0)
        }

        // A panic while a write guard was held poisons the lock.
        // The value is still usable, so take it as is: the Rc/RefCell
        // build has no poisoning either.
        pub fn borrow(&self) -> std::sync::RwLockReadGuard<'_, T> {
            self.0
                .read()
                .unwrap_or_else(|poisoned| poisoned.into_inner())
        }

        pub fn borrow_mut(&self) -> std::sync::RwLockWriteGuard<'_, T> {
            self.0
                .write()
                .unwrap_or_else(|poisoned| poisoned.into_inner())
        }

        pub(crate) fn addr_as_usize(&self) -> usize {
            std::sync::Arc::as_ptr(&self.0) as usize
        }

        pub(crate) fn strong_count(&self) -> usize {
            std::sync::Arc::strong_count(&self.0)
        }

        /// The value, when this is its only reference.
        pub(crate) fn get_mut(&mut self) -> Option<&mut T> {
            std::sync::Arc::get_mut(&mut self.0).map(|lock| {
                lock.get_mut()
                    .unwrap_or_else(|poisoned| poisoned.into_inner())
            })
        }
    }

    impl<T: Default> Default for SharedMut<T> {
        fn default() -> Self {
            SharedMut::new(T::default())
        }
    }

    #[cfg(test)]
    mod tests {
        use super::SharedMut;

        // A panic while a write guard is held poisons the lock. The
        // next borrow must still work instead of panicking again.
        #[test]
        fn a_poisoned_lock_can_still_be_borrowed() {
            let cell = SharedMut::new(1);
            let poison = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                let _guard = cell.borrow_mut();
                panic!("poison the lock");
            }));
            assert!(poison.is_err());
            assert_eq!(*cell.borrow(), 1);
            *cell.borrow_mut() = 2;
            assert_eq!(*cell.borrow(), 2);
        }
    }
}
