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
	let ret = TulispValue::nil();
        list!(@push ret, $($items)+)
            .and_then(|ret| Ok(ret.to_owned()))
    }};
    () => { TulispValue::nil() }
}

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
            return Err(Error::new(ErrorKind::TypeMismatch,"Too many arguments".to_string(), ));
        }
    };
    (@rest $rest:ident $vv:ident) => {
        let $rest = if $vv.null() {
            TulispValue::nil()
        } else {
            $vv
        };
    };
    (@optvar $vv:ident, $var:ident) => {
        let ($var, $vv) = if !$vv.null() {
            ($vv.car()?, $vv.cdr()?)
        } else {
            (TulispValue::nil(), TulispValue::nil())
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
