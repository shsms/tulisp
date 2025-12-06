use crate::{TulispObject, cons};

pub struct Rest<T> {
    values: RestEnum<T>,
}

enum RestEnum<T> {
    Typed(Vec<T>),
    Boxed(TulispObject, std::marker::PhantomData<T>),
}

impl From<Rest<TulispObject>> for TulispObject {
    fn from(val: Rest<TulispObject>) -> Self {
        match val.values {
            RestEnum::Typed(..) => {
                unreachable!()
            }
            RestEnum::Boxed(obj, _) => obj,
        }
    }
}

impl<T> FromIterator<T> for Rest<T>
where
    T: Into<TulispObject> + 'static,
{
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        if std::any::TypeId::of::<T>() == std::any::TypeId::of::<TulispObject>() {
            let obj: TulispObject = iter.into_iter().map(|t| t.into()).collect();
            Rest {
                values: RestEnum::Boxed(obj, std::marker::PhantomData),
            }
        } else {
            let values: Vec<T> = iter.into_iter().collect();
            Rest {
                values: RestEnum::Typed(values),
            }
        }
    }
}

pub enum RestEnumIter<T> {
    Typed(std::vec::IntoIter<T>),
    Boxed(cons::BaseIter, std::marker::PhantomData<T>),
}

impl<T> Iterator for RestEnumIter<T>
where
    T: TryFrom<TulispObject>,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            RestEnumIter::Typed(iter) => iter.next(),
            RestEnumIter::Boxed(base_iter, _) => {
                base_iter.next().and_then(|obj| T::try_from(obj).ok())
            }
        }
    }
}

impl<T> IntoIterator for Rest<T>
where
    T: TryFrom<TulispObject>,
{
    type Item = T;
    type IntoIter = RestEnumIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        match self.values {
            RestEnum::Typed(vec) => RestEnumIter::Typed(vec.into_iter()),
            RestEnum::Boxed(obj, _) => {
                let base_iter = obj.base_iter();
                RestEnumIter::Boxed(base_iter, std::marker::PhantomData)
            }
        }
    }
}
