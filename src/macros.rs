#[macro_export]
macro_rules! __list__ {
    (@push $ret:ident, $item:expr) => {
        $ret.push($item)
    };
    (@push $ret:ident, @ $item:expr) => {
        $ret.append($item)
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
	let mut ret = TulispValue::new_list();
        list!(@push ret, $($items)+)
            .and_then(|ret| Ok(ret.to_owned()))
            .map(|x| x.into_ref())
    }};
    () => { TulispValue::new_list().into_ref() }
}

#[macro_export]
macro_rules! __defun_args__ {
    (@reqr $vv:ident, $var:ident) => {
        let $var = car($vv.clone())?;
        let $vv = cdr($vv)?;
    };
    (@reqr $vv:ident, $var:ident $($vars:tt)+) => {
        defun_args!(@reqr $vv, $var);
        defun_args!(@reqr $vv, $($vars)+);
    };
    (@reqr $vv:ident,) => {};
    (@no-rest $vv:ident) => {
        if $vv != TulispValue::Uninitialized {
            return Err(Error::new(ErrorKind::TypeMismatch,"Too many arguments".to_string(), ));
        }
    };
    (@rest $rest:ident $vv:ident) => {
        let $rest = if $vv == TulispValue::Uninitialized {
            TulispValue::Nil.into_ref()
        } else {
            $vv
        };
    };
    (@optvar $vv:ident, $var:ident) => {
        let ($var, $vv) = if $vv != TulispValue::Uninitialized {
            (car($vv.clone())?, cdr($vv)?)
        } else {
            (TulispValue::Nil.into_ref(), TulispValue::Uninitialized.into_ref())
        };
    };
    (@optvar $vv:ident, $var:ident $($vars:ident)+) => {
        defun_args!(@optvar $vv, $var);
        defun_args!(@optvar $vv, $($vars)+)
    };
    (($($vars:ident)+) = $vv:ident) => {
        defun_args!(@reqr $vv, $($vars)+);
        defun_args!(@no-rest $vv);
    };
    (($($vars:ident)* &optional $($optvars:ident)+) = $vv:ident) => {
	defun_args!(@reqr $vv, $($vars)*);
        defun_args!(@optvar $vv, $($optvars)+);
        defun_args!(@no-rest $vv);
    };
    (($($vars:ident)* &rest $rest:ident) = $vv:ident) => {
	defun_args!(@reqr $vv, $($vars)*);
        defun_args!(@rest $rest $vv);
    };
    (($($vars:ident)* &optional $($optvars:ident)+ &rest $rest:ident) = $vv:ident) => {
	defun_args!(@reqr $vv, $($vars)*);
        defun_args!(@optvar $vv, $($optvars)+);
        defun_args!(@rest $rest $vv);
    };
    (_ ($($rest:tt)*) = $vv:ident) => {
        let $vv = cdr($vv)?;
        defun_args!(($($rest)*) = $vv);
    };
    ($defun_name:ident ($($rest:tt)*) = $vv:ident) => {
        let $defun_name = car($vv.clone())?;
        defun_args!(_ ($($rest)*) = $vv);
    }
}

pub use __defun_args__ as defun_args;
pub use __list__ as list;
