use crate::{Error, Number, Shared, TulispAny, TulispObject, TulispValue};

pub trait TulispConvertible {
    fn from_tulisp(value: &TulispObject) -> Result<Self, Error>
    where
        Self: Sized;

    fn into_tulisp(self) -> TulispObject;
}

impl TulispConvertible for String {
    fn from_tulisp(value: &TulispObject) -> Result<String, Error> {
        value.as_string().map_err(|e| e.with_trace(value.clone()))
    }

    fn into_tulisp(self) -> TulispObject {
        TulispValue::from(self).into_ref(None)
    }
}

impl TulispConvertible for f64 {
    fn from_tulisp(value: &TulispObject) -> Result<f64, Error> {
        let res = value.rc.borrow().0.try_float();
        res.map_err(|e| e.with_trace(value.clone()))
    }
    fn into_tulisp(self) -> TulispObject {
        TulispValue::from(self).into_ref(None)
    }
}

impl TulispConvertible for i64 {
    fn from_tulisp(value: &TulispObject) -> Result<i64, Error> {
        let res = value.rc.borrow().0.as_int();
        res.map_err(|e| e.with_trace(value.clone()))
    }
    fn into_tulisp(self) -> TulispObject {
        TulispValue::from(self).into_ref(None)
    }
}

impl TulispConvertible for bool {
    fn from_tulisp(value: &TulispObject) -> Result<bool, Error> {
        Ok(value.is_truthy())
    }
    fn into_tulisp(self) -> TulispObject {
        TulispValue::from(self).into_ref(None)
    }
}

impl TulispConvertible for Shared<dyn TulispAny> {
    fn from_tulisp(value: &TulispObject) -> Result<Shared<dyn TulispAny>, Error> {
        value.as_any().map_err(|e| e.with_trace(value.clone()))
    }
    fn into_tulisp(self) -> TulispObject {
        TulispValue::from(self).into_ref(None)
    }
}

impl<T> TulispConvertible for Vec<T>
where
    T: TulispConvertible,
{
    fn from_tulisp(value: &TulispObject) -> Result<Vec<T>, Error> {
        value
            .base_iter()
            .map(|item| T::from_tulisp(&item))
            .collect::<Result<Vec<T>, Error>>()
            .map_err(|e| e.with_trace(value.clone()))
    }
    fn into_tulisp(self) -> TulispObject {
        TulispValue::from_iter(self.into_iter().map(|x| TulispConvertible::into_tulisp(x)))
            .into_ref(None)
    }
}

impl TulispConvertible for TulispObject {
    fn from_tulisp(value: &TulispObject) -> Result<TulispObject, Error> {
        Ok(value.clone())
    }
    fn into_tulisp(self) -> TulispObject {
        self
    }
}

impl TulispConvertible for Number {
    fn from_tulisp(value: &TulispObject) -> Result<Number, Error> {
        value.as_number().map_err(|e| e.with_trace(value.clone()))
    }
    fn into_tulisp(self) -> TulispObject {
        TulispValue::from(self).into_ref(None)
    }
}
