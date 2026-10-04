use std::borrow::Cow;

use crate::{TulispContext, TulispObject};

mod table;
pub(crate) use table::ErrorTable;

/// A macro for defining the `ErrorKind` enum, the `Display` implementation for
/// it, and the constructors for the `Error` struct. The kinds before the `;`
/// carry only a description; those after it carry a value too, and print
/// through `ErrorKind::fmt_value`.
macro_rules! ErrorKind {
    (
        $(($kind:ident, $ctor:ident)),* $(,)?
        ;
        $($(#[$meta:meta])* $valued:ident $fields:tt),* $(,)?
    ) => {
        /// The kind of error that occurred.
        #[derive(Debug, Clone)]
        #[non_exhaustive]
        pub enum ErrorKind {
            $(
                $kind,
            )*
            $(
                $(#[$meta])*
                $valued $fields,
            )*
        }

        impl std::fmt::Display for ErrorKind {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(
                        Self::$kind => f.write_str(stringify!($kind)),
                    )*
                    $(
                        Self::$valued { .. } => self.fmt_value(f),
                    )*
                }
            }
        }

        /// Constructors for [`Error`].
        impl Error {
            $(
                #[doc = concat!(
                    "Creates a new [`Error`] with the `",
                    stringify!($kind),
                    "` kind and the given description."
                )]
                pub fn $ctor(desc: impl Into<String>) -> crate::error::Error {
                    Self::new(ErrorKind::$kind, desc)
                }
            )*
        }
    };
}

ErrorKind!(
    (ArithError,      arith_error),
    (InvalidArgument, invalid_argument),
    (LispError,       lisp_error),
    (NotImplemented,  not_implemented),
    (OutOfRange,      out_of_range),
    (OSError,         os_error),
    (BrokenPipe,      broken_pipe),
    (TypeMismatch,    type_mismatch),
    (PlistError,      plist_error),
    (AlistError,      alist_error),
    (MissingArgument, missing_argument),
    (ArityMismatch,   arity_mismatch),
    (Undefined,       undefined),
    (Uninitialized,   uninitialized),
    (ParsingError,    parsing_error),
    (SyntaxError,     syntax_error),
    (Interrupted,     interrupted);
    /// A `throw`, holding `(TAG . VALUE)`; see [`Error::throw`].
    Throw(TulispObject),
    /// An error symbol raised with its data, by `signal` or
    /// [`TulispContext::signal`].
    Signal { symbol: TulispObject, data: TulispObject },
);

impl ErrorKind {
    /// Prints a kind that carries a value by its name alone: a `throw` by its
    /// tag, a signal by its error symbol. The description shows the rest.
    fn fmt_value(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorKind::Throw(pair) => write!(f, "Throw({})", pair.car().unwrap_or_default()),
            ErrorKind::Signal { symbol, .. } => write!(f, "Signal({symbol})"),
            // Only a kind with a value comes here; one not listed above still
            // prints as something.
            other => write!(f, "{other:?}"),
        }
    }
}

impl Error {
    /// The error for a call short of its required arguments.
    pub fn too_few_arguments() -> Error {
        Error::arity_mismatch("Too few arguments".to_string())
    }

    /// The error for a call with more arguments than parameters.
    pub fn too_many_arguments() -> Error {
        Error::arity_mismatch("Too many arguments".to_string())
    }

    /// The error for a list whose cdrs loop back to an earlier cell.
    /// Emacs signals `circular-list` here.
    pub fn circular_list() -> Error {
        Error::out_of_range("Circular list".to_string())
    }

    /// The error for binding or setting a constant, such as `nil`,
    /// `t` or a keyword. Emacs signals `setting-constant` here.
    pub fn setting_constant(name: impl std::fmt::Display) -> Error {
        Error::type_mismatch(format!("Can't set constant symbol: {name}"))
    }

    /// The error for calling NAME when it holds no function. Emacs
    /// signals `void-function` here.
    pub fn void_function(name: impl std::fmt::Display) -> Error {
        Error::undefined(format!("function is void: {name}"))
    }
}

/// Represents an error that occurred during Tulisp evaluation.
///
/// Use [format](crate::Error::format) to produce a formatted representation of the error
/// including backtraces and source code spans.
#[derive(Clone)]
pub struct Error {
    kind: ErrorKind,
    desc: String,
    backtrace: Vec<TulispObject>,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let desc = self.desc();
        if desc.is_empty() {
            write!(f, "ERR {}", self.kind)?;
        } else {
            write!(f, "ERR {}: {}", self.kind, desc)?;
        }
        for span_obj in &self.backtrace {
            if span_obj.numberp() || span_obj.is_symbol_variant() || span_obj.stringp() {
                continue;
            }
            let prefix = if let Some(span) = span_obj.span() {
                format!(
                    "<file {}>:{}.{}-{}.{}:",
                    span.file_id, span.start.0, span.start.1, span.end.0, span.end.1
                )
            } else {
                continue;
            };
            let string = span_obj.to_string().replace('\n', "\\n");
            if string.len() > 80 {
                write!(f, "\n{}  at {:.80}...", prefix, string)?;
            } else {
                write!(f, "\n{}  at {}", prefix, string)?;
            }
        }
        Ok(())
    }
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
    }
}

impl std::error::Error for Error {}

/// An I/O error becomes a `BrokenPipe` error for a broken pipe, and an
/// `OSError` otherwise, with the I/O error's message.
impl From<std::io::Error> for Error {
    fn from(err: std::io::Error) -> Self {
        if err.kind() == std::io::ErrorKind::BrokenPipe {
            Error::broken_pipe(err.to_string())
        } else {
            Error::os_error(err.to_string())
        }
    }
}

impl Error {
    /// Creates a new [`Error`] with the given kind and description.
    pub(crate) fn new(kind: ErrorKind, desc: impl Into<String>) -> Self {
        Self {
            kind,
            desc: desc.into(),
            backtrace: vec![],
        }
    }

    /// Creates a new `Throw` error with the given tag and value. Its
    /// description is what shows when no `catch` receives it. Unlike Lisp's
    /// `throw`, it does not check for a running `catch` for the tag: with
    /// none, it reaches the host as a `Throw`, not a `no-catch` error.
    pub fn throw(tag: TulispObject, value: TulispObject) -> Self {
        Self::new(
            ErrorKind::Throw(TulispObject::cons(tag, value)),
            String::new(),
        )
    }

    /// Creates a `Signal` error for SYMBOL with DATA, described by DESC.
    pub(crate) fn new_signal(symbol: TulispObject, data: TulispObject, desc: String) -> Self {
        Self::new(ErrorKind::Signal { symbol, data }, desc)
    }

    fn format_span(&self, ctx: &TulispContext, object: &TulispObject) -> String {
        if let Some(span) = object.span() {
            let filename = ctx.get_filename(span.file_id);
            format!(
                "{}:{}.{}-{}.{}:",
                filename, span.start.0, span.start.1, span.end.0, span.end.1
            )
        } else {
            String::new()
        }
    }

    /// Formats the error into a human-readable string, including backtrace information.
    pub fn format(&self, ctx: &TulispContext) -> String {
        let desc = self.desc();
        let mut span_str = if desc.is_empty() {
            format!("ERR {}", self.kind)
        } else {
            format!("ERR {}: {}", self.kind, desc)
        };
        for span in &self.backtrace {
            let prefix = self.format_span(ctx, span);
            if prefix.is_empty() {
                continue;
            }
            if span.numberp() || span.is_symbol_variant() || span.stringp() {
                continue;
            }
            let string = span.to_string().replace("\n", "\\n");
            if string.len() > 80 {
                span_str.push_str(&format!("\n{}  at {:.80}...", prefix, string));
            } else {
                span_str.push_str(&format!("\n{}  at {}", prefix, string));
            }
        }
        span_str + "\n"
    }
}

impl Error {
    /// Adds a trace span to the error's backtrace.
    ///
    /// Dedup is **positional** — only collapses against the
    /// `backtrace.last()` entry, not the full set. Today the
    /// well-formedness invariant that justifies that is:
    ///
    /// 1. A call instruction wraps its callee's error with
    ///    `with_trace(form)` once.
    /// 2. The VM's `run_impl` walks `trace_ranges` from
    ///    innermost-out and applies them with `with_trace(form)` in
    ///    order, so the same form can't appear non-adjacently in the
    ///    same trace.
    ///
    /// A refactor that reorders trace application — e.g. attaching
    /// an outer form before the inner one is finalized — could
    /// produce duplicates that this last-only check misses. If that
    /// happens, switch to a set-based dedup (e.g. by
    /// `addr_as_usize`) and update the call sites that rely on the
    /// last-only collapse.
    pub fn with_trace(mut self, span: TulispObject) -> Self {
        if self.backtrace.last().is_some_and(|last| last.eq(&span)) {
            return self;
        }
        self.backtrace.push(span);
        self
    }

    /// Returns the kind of the error.
    pub fn kind(&self) -> &ErrorKind {
        &self.kind
    }

    /// Returns the description of the error. For a `Throw`, it is built on
    /// each call and reads `No catch for tag: TAG, VALUE`.
    pub fn desc(&self) -> Cow<'_, str> {
        match &self.kind {
            ErrorKind::Throw(pair) => Cow::Owned(format!(
                "No catch for tag: {}, {}",
                pair.car().unwrap_or_default(),
                pair.cdr().unwrap_or_default()
            )),
            _ => Cow::Borrowed(&self.desc),
        }
    }

    /// The error's data, what a `condition-case` handler sees after the error
    /// symbol: `(DESC)` for a built-in kind, the data given to `signal` for a
    /// `Signal`, and nil for a `throw` (a Lisp `throw` with no `catch` for its
    /// tag is a `no-catch` signal instead, whose data is `(TAG VALUE)`).
    pub fn data(&self) -> TulispObject {
        match &self.kind {
            ErrorKind::Throw(_) => TulispObject::nil(),
            ErrorKind::Signal { data, .. } => data.clone(),
            _ => TulispObject::cons(TulispObject::from(self.desc.clone()), TulispObject::nil()),
        }
    }

    /// Whether a `condition-case` handler for CONDITION catches this error in
    /// CTX, following the error's parents. False for a `throw`, which no
    /// handler catches; a Lisp `throw` with no `catch` for its tag is a
    /// `no-catch` error instead, under `error`. False too for an `Interrupted`
    /// error, from [`Interrupt::Stop`](crate::Interrupt::Stop).
    pub fn is_a(&self, ctx: &TulispContext, condition: &str) -> bool {
        self.symbol_name()
            .is_some_and(|name| ctx.error_table.matches(&name, condition))
    }

    /// The Emacs error symbol `condition-case` matches this error against, or
    /// `None` for a `throw` or a stop, which `condition-case` never catches.
    pub(crate) fn symbol_name(&self) -> Option<Cow<'static, str>> {
        let name = match &self.kind {
            ErrorKind::TypeMismatch | ErrorKind::InvalidArgument => "wrong-type-argument",
            ErrorKind::OutOfRange => "args-out-of-range",
            ErrorKind::ArithError => "arith-error",
            ErrorKind::LispError => "error",
            ErrorKind::MissingArgument | ErrorKind::ArityMismatch => "wrong-number-of-arguments",
            ErrorKind::Undefined => "void-function",
            ErrorKind::Uninitialized => "void-variable",
            ErrorKind::ParsingError | ErrorKind::SyntaxError => "invalid-read-syntax",
            ErrorKind::NotImplemented => "not-implemented",
            ErrorKind::OSError | ErrorKind::BrokenPipe => "file-error",
            ErrorKind::PlistError | ErrorKind::AlistError => "wrong-type-argument",
            ErrorKind::Signal { symbol, .. } => return symbol.as_symbol().ok().map(Cow::Owned),
            ErrorKind::Throw(_) | ErrorKind::Interrupted => return None,
        };
        Some(Cow::Borrowed(name))
    }
}

#[cfg(test)]
mod tests {
    use super::{Error, ErrorKind};

    // `?` carries an `Error` into a `Box<dyn std::error::Error>`, and an
    // I/O error converts to an `OSError`.
    #[test]
    fn error_works_with_std_error_handling() {
        fn run() -> Result<(), Box<dyn std::error::Error>> {
            Err(Error::lisp_error("boom"))?
        }
        assert_eq!(run().unwrap_err().to_string(), "ERR LispError: boom");

        let io = std::io::Error::new(std::io::ErrorKind::NotFound, "no such file");
        let err: Error = io.into();
        assert!(matches!(err.kind(), ErrorKind::OSError));
        assert_eq!(err.to_string(), "ERR OSError: no such file");

        let io = std::io::Error::new(std::io::ErrorKind::BrokenPipe, "closed");
        let err: Error = io.into();
        assert!(matches!(err.kind(), ErrorKind::BrokenPipe));
        assert_eq!(err.to_string(), "ERR BrokenPipe: closed");
    }

    // `kind` and `desc` lend what the error holds.
    #[test]
    fn kind_and_desc_borrow_from_the_error() {
        let err = Error::lisp_error("boom");
        let kind: &ErrorKind = err.kind();
        let desc: std::borrow::Cow<'_, str> = err.desc();
        assert!(matches!(kind, ErrorKind::LispError));
        assert!(matches!(desc, std::borrow::Cow::Borrowed("boom")));
    }
}
