use crate::{Error, ErrorKind, TulispObject, TulispValue};

#[derive(Clone)]
pub(crate) enum VMStackValue {
    TulispObject(TulispObject),
    Bool(bool),
    Float(f64),
    Int(i64),
}

macro_rules! impl_from_for_stack_value {
    ($name:ident, $type:ty) => {
        impl From<$type> for VMStackValue {
            fn from(val: $type) -> Self {
                VMStackValue::$name(val)
            }
        }
    };
}

impl_from_for_stack_value!(Float, f64);
impl_from_for_stack_value!(Int, i64);
impl_from_for_stack_value!(Bool, bool);

impl From<TulispObject> for VMStackValue {
    fn from(val: TulispObject) -> Self {
        match &*val.inner_ref() {
            TulispValue::Int { value } => return VMStackValue::Int(*value),
            TulispValue::Float { value } => return VMStackValue::Float(*value),
            TulispValue::T => return VMStackValue::Bool(true),
            TulispValue::Nil => return VMStackValue::Bool(false),
            _ => {}
        }
        VMStackValue::TulispObject(val)
    }
}

impl Into<TulispObject> for VMStackValue {
    fn into(self) -> TulispObject {
        match self {
            VMStackValue::TulispObject(obj) => obj,
            VMStackValue::Bool(b) => b.into(),
            VMStackValue::Float(fl) => fl.into(),
            VMStackValue::Int(i) => i.into(),
        }
    }
}

impl VMStackValue {
    pub fn null(&self) -> bool {
        match self {
            VMStackValue::TulispObject(obj) => obj.null(),
            VMStackValue::Bool(b) => !b,
            VMStackValue::Float(_) | VMStackValue::Int(_) => false,
        }
    }

    pub fn add(&self, other: &VMStackValue) -> Result<VMStackValue, Error> {
        add(self, other)
    }

    pub fn sub(&self, other: &VMStackValue) -> Result<VMStackValue, Error> {
        sub(self, other)
    }

    pub fn mul(&self, other: &VMStackValue) -> Result<VMStackValue, Error> {
        mul(self, other)
    }

    pub fn div(&self, other: &VMStackValue) -> Result<VMStackValue, Error> {
        div(self, other)
    }

    pub fn lt(&self, other: &VMStackValue) -> Result<bool, Error> {
        lt(self, other)
    }

    pub fn le(&self, other: &VMStackValue) -> Result<bool, Error> {
        le(self, other)
    }

    pub fn gt(&self, other: &VMStackValue) -> Result<bool, Error> {
        gt(self, other)
    }

    pub fn ge(&self, other: &VMStackValue) -> Result<bool, Error> {
        ge(self, other)
    }

    pub fn equal(&self, other: &VMStackValue) -> bool {
        match self {
            VMStackValue::TulispObject(obj1) => match other {
                VMStackValue::TulispObject(obj2) => obj1.equal(obj2),
                VMStackValue::Bool(b2) => obj1.equal(&(*b2).into()),
                VMStackValue::Float(fl2) => obj1.equal(&(*fl2).into()),
                VMStackValue::Int(i2) => obj1.equal(&(*i2).into()),
            },
            VMStackValue::Bool(b) => match other {
                VMStackValue::Bool(b2) => b == b2,
                _ => false,
            },
            VMStackValue::Float(fl) => match other {
                VMStackValue::Float(fl2) => fl == fl2,
                _ => false,
            },
            VMStackValue::Int(i) => match other {
                VMStackValue::Int(i2) => i == i2,
                _ => false,
            },
        }
    }

    pub fn eq(&self, other: &VMStackValue) -> bool {
        match self {
            VMStackValue::TulispObject(obj1) => match other {
                VMStackValue::TulispObject(obj2) => obj1.eq(obj2),
                _ => false,
            },
            VMStackValue::Bool(b) => match other {
                VMStackValue::Bool(b2) => b == b2,
                _ => false,
            },
            VMStackValue::Float(fl) => match other {
                VMStackValue::Float(fl2) => fl == fl2,
                _ => false,
            },
            VMStackValue::Int(i) => match other {
                VMStackValue::Int(i2) => i == i2,
                _ => false,
            },
        }
    }
}

impl std::fmt::Display for VMStackValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VMStackValue::TulispObject(obj) => write!(f, "{}", obj),
            VMStackValue::Bool(b) => write!(f, "{}", b),
            VMStackValue::Float(fl) => write!(f, "{}", fl),
            VMStackValue::Int(i) => write!(f, "{}", i),
        }
    }
}

macro_rules! compare_ops {
    ($name:ident, $oper:expr) => {
        fn $name(selfobj: &VMStackValue, other: &VMStackValue) -> Result<bool, Error> {
            match selfobj {
                VMStackValue::Float(f1) => match other {
                    VMStackValue::Float(f2) => Ok($oper(f1, f2)),
                    VMStackValue::Int(i2) => Ok($oper(f1, &(*i2 as f64))),
                    _ => Err(Error::new(
                        ErrorKind::TypeMismatch,
                        format!("Expected number, found: {}", other),
                    )),
                },
                VMStackValue::Int(i1) => match other {
                    VMStackValue::Float(f2) => Ok($oper(&(*i1 as f64), f2)),
                    VMStackValue::Int(i2) => Ok($oper(i1, i2)),
                    _ => Err(Error::new(
                        ErrorKind::TypeMismatch,
                        format!("Expected number, found: {}", other),
                    )),
                },
                _ => Err(Error::new(
                    ErrorKind::TypeMismatch,
                    format!("Expected number, found: {}", selfobj),
                )),
            }
        }
    };
}

compare_ops!(lt, std::cmp::PartialOrd::lt);
compare_ops!(le, std::cmp::PartialOrd::le);
compare_ops!(gt, std::cmp::PartialOrd::gt);
compare_ops!(ge, std::cmp::PartialOrd::ge);

macro_rules! binary_ops {
    ($name:ident, $oper:expr) => {
        fn $name(selfobj: &VMStackValue, other: &VMStackValue) -> Result<VMStackValue, Error> {
            match selfobj {
                VMStackValue::Float(f1) => match other {
                    VMStackValue::Float(f2) => Ok(VMStackValue::Float($oper(f1, f2))),
                    VMStackValue::Int(i2) => Ok(VMStackValue::Float($oper(f1, &(*i2 as f64)))),
                    _ => Err(Error::new(
                        ErrorKind::TypeMismatch,
                        format!("Expected number, found: {}", other),
                    )),
                },
                VMStackValue::Int(i1) => match other {
                    VMStackValue::Float(f2) => Ok(VMStackValue::Float($oper(&(*i1 as f64), f2))),
                    VMStackValue::Int(i2) => Ok(VMStackValue::Int($oper(i1, i2))),
                    _ => Err(Error::new(
                        ErrorKind::TypeMismatch,
                        format!("Expected number, found: {}", other),
                    )),
                },
                _ => Err(Error::new(
                    ErrorKind::TypeMismatch,
                    format!("Expected number, found: {}", selfobj),
                )),
            }
        }
    };
}

binary_ops!(add, std::ops::Add::add);
binary_ops!(sub, std::ops::Sub::sub);
binary_ops!(mul, std::ops::Mul::mul);
binary_ops!(div, std::ops::Div::div);
