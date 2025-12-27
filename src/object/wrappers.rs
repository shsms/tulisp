use crate::{Error, TulispContext, TulispObject};

#[cfg(not(feature = "sync"))]
pub mod generic {
    use std::ops::Deref;

    use crate::TulispAny;

    use super::*;

    pub trait SyncSend {}
    impl<T> SyncSend for T {}

    #[repr(transparent)]
    #[derive(Debug)]
    pub struct Shared<T: ?Sized>(std::rc::Rc<T>);

    pub type SharedRef<'a, T> = std::cell::Ref<'a, T>;

    impl<T: ?Sized + std::fmt::Display> std::fmt::Display for Shared<T> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "Shared({})", self.0)
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

        pub fn new_any(val: impl TulispAny) -> Shared<dyn TulispAny> {
            Shared(std::rc::Rc::new(val))
        }

        pub fn downcast_ref<U: TulispAny + 'static>(&self) -> Option<&U> {
            let a: &dyn std::any::Any = &*self.0;
            a.downcast_ref::<U>()
        }

        pub fn downcast<U: TulispAny + 'static>(self) -> Result<Shared<U>, Shared<dyn TulispAny>> {
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

    impl<T: ?Sized> std::ops::DerefMut for Shared<T> {
        fn deref_mut(&mut self) -> &mut Self::Target {
            std::rc::Rc::get_mut(&mut self.0).expect("Multiple references exist")
        }
    }

    #[repr(transparent)]
    #[derive(Clone, Debug)]
    pub struct SharedMut<T>(std::rc::Rc<std::cell::RefCell<T>>);

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

        pub fn addr_as_usize(&self) -> usize {
            self.0.as_ptr() as usize
        }

        pub fn strong_count(&self) -> usize {
            std::rc::Rc::strong_count(&self.0)
        }
    }

    impl<T> Deref for SharedMut<T> {
        type Target = T;

        fn deref(&self) -> &Self::Target {
            todo!()
        }
    }

    pub trait TulispFn:
        Fn(&mut TulispContext, &TulispObject) -> Result<TulispObject, Error> + 'static
    {
    }
    impl<T> TulispFn for T where
        T: Fn(&mut TulispContext, &TulispObject) -> Result<TulispObject, Error> + 'static
    {
    }
}

#[cfg(feature = "sync")]
pub mod generic {
    use std::ops::Deref;

    use crate::TulispAny;

    use super::*;

    pub trait SyncSend: Sync + Send {}
    impl<T> SyncSend for T where T: Send + Sync {}

    #[repr(transparent)]
    #[derive(Debug)]
    pub struct Shared<T: ?Sized>(std::sync::Arc<T>);

    pub type SharedRef<'a, T> = std::sync::RwLockReadGuard<'a, T>;

    impl<T: ?Sized + std::fmt::Display> std::fmt::Display for Shared<T> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "Shared({})", self.0)
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

        pub fn new_any(val: impl TulispAny) -> Shared<dyn TulispAny> {
            Shared(std::sync::Arc::new(val))
        }

        pub fn downcast_ref<U: TulispAny + 'static>(&self) -> Option<&U> {
            let a: &dyn std::any::Any = &*self.0;
            a.downcast_ref::<U>()
        }

        pub fn downcast<U: TulispAny + 'static>(self) -> Result<Shared<U>, Shared<dyn TulispAny>> {
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

    #[repr(transparent)]
    #[derive(Clone, Debug)]
    pub struct SharedMut<T>(std::sync::Arc<std::sync::RwLock<T>>);
    impl<T> SharedMut<T> {
        pub fn new(val: T) -> Self {
            SharedMut(std::sync::Arc::new(std::sync::RwLock::new(val)))
        }
        pub fn ptr_eq(&self, other: &Self) -> bool {
            std::sync::Arc::ptr_eq(&self.0, &other.0)
        }

        pub fn borrow(&self) -> std::sync::RwLockReadGuard<'_, T> {
            self.0.read().unwrap()
        }

        pub fn borrow_mut(&self) -> std::sync::RwLockWriteGuard<'_, T> {
            self.0.write().unwrap()
        }

        pub fn addr_as_usize(&self) -> usize {
            std::sync::Arc::as_ptr(&self.0) as usize
        }

        pub fn strong_count(&self) -> usize {
            std::sync::Arc::strong_count(&self.0)
        }
    }
    pub trait TulispFn:
        Fn(&mut TulispContext, &TulispObject) -> Result<TulispObject, Error> + SyncSend + 'static
    {
    }
    impl<T> TulispFn for T where
        T: Fn(&mut TulispContext, &TulispObject) -> Result<TulispObject, Error>
            + SyncSend
            + 'static
    {
    }
}
