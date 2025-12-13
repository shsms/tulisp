use crate::{Error, TulispContext, TulispObject};

pub(crate) mod generic {
    use super::*;

    pub trait TulispFn:
        Fn(&mut TulispContext, &TulispObject) -> Result<TulispObject, Error> + 'static
    {
    }
    impl<T> TulispFn for T where
        T: Fn(&mut TulispContext, &TulispObject) -> Result<TulispObject, Error> + 'static
    {
    }
}
