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
        $(($kind:ident $(, $vis:vis $ctor:ident)?)),* $(,)?
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
                $(
                #[doc = concat!(
                    "Creates a new [`Error`] with the `",
                    stringify!($kind),
                    "` kind and the given description."
                )]
                $vis fn $ctor(desc: impl Into<String>) -> crate::error::Error {
                    Self {
                        kind: ErrorKind::$kind,
                        desc: desc.into(),
                        backtrace: vec![],
                    }
                }
                )?
            )*
        }
    };
}

ErrorKind!(
    (ArithError,      pub arith_error),
    (InvalidArgument, pub invalid_argument),
    (LispError,       pub lisp_error),
    (NotImplemented,  pub not_implemented),
    (OutOfRange,      pub out_of_range),
    (OSError,         pub os_error),
    (BrokenPipe,      pub(crate) broken_pipe),
    (TypeMismatch,    pub type_mismatch),
    (PlistError,      pub plist_error),
    (AlistError,      pub alist_error),
    (MissingArgument, pub missing_argument),
    (ArityMismatch,   pub(crate) arity_mismatch),
    (Undefined,       pub(crate) undefined),
    (Uninitialized,   pub(crate) uninitialized),
    (ParsingError,    pub(crate) parsing_error),
    (SyntaxError,     pub(crate) syntax_error);
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
    pub(crate) fn circular_list() -> Error {
        Error::out_of_range("Circular list".to_string())
    }

    /// The error for binding or setting a constant, such as `nil`,
    /// `t` or a keyword. Emacs signals `setting-constant` here.
    pub(crate) fn setting_constant(name: impl std::fmt::Display) -> Error {
        Error::type_mismatch(format!("Can't set constant symbol: {name}"))
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
        let desc = self.description();
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
        Self {
            kind: ErrorKind::Throw(TulispObject::cons(tag, value)),
            desc: String::new(),
            backtrace: vec![],
        }
    }

    /// Creates a `Signal` error for SYMBOL with DATA, described by DESC.
    pub(crate) fn new_signal(symbol: TulispObject, data: TulispObject, desc: String) -> Self {
        Self {
            kind: ErrorKind::Signal { symbol, data },
            desc,
            backtrace: vec![],
        }
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
        let desc = self.description();
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
    pub fn kind(&self) -> ErrorKind {
        self.kind.clone()
    }

    pub(crate) fn kind_ref(&self) -> &ErrorKind {
        &self.kind
    }

    /// Returns the description of the error.
    pub fn desc(&self) -> String {
        self.description().into_owned()
    }

    /// The description. A `throw`'s is built when read, so a `throw` that a
    /// `catch` receives prints nothing.
    fn description(&self) -> Cow<'_, str> {
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
    /// `no-catch` error instead, under `error`.
    pub fn is_a(&self, ctx: &TulispContext, condition: &str) -> bool {
        self.symbol_name()
            .is_some_and(|name| ctx.error_table.matches(&name, condition))
    }

    /// The Emacs error symbol `condition-case` matches this error against,
    /// or `None` for a `throw`, which `condition-case` never catches.
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
            ErrorKind::Throw(_) => return None,
        };
        Some(Cow::Borrowed(name))
    }
}
