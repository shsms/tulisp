use crate::{TulispContext, TulispObject};

macro_rules! replace_expr {
    ($_t:ty, $sub:ident) => {
        $sub
    };
}

/// A macro for defining the `ErrorKind` enum, the `Display` implementation for
/// it, and the constructors for the `Error` struct.
macro_rules! ErrorKind {
    ($(
        ($kind:ident$(($param:ty))? $(, $vis:vis $ctor:ident)?)
    ),* $(,)?) => {
        /// The kind of error that occurred.
        #[derive(Debug, Clone)]
        pub enum ErrorKind {
            $(
                $kind$(( $param ))?,
            )*
        }

        impl std::fmt::Display for ErrorKind {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(
                        Self::$kind$((replace_expr!($param, vv)))? => {
                            write!(
                                f,
                                "{}",
                                stringify!($kind)
                                $(.to_owned() + "(" + &replace_expr!($param, vv).to_string() + ")")?
                            )
                        },
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
    (InvalidArgument, pub invalid_argument),
    (LispError,       pub lisp_error),
    (NotImplemented,  pub not_implemented),
    (OutOfRange,      pub out_of_range),
    (OSError,         pub os_error),
    (TypeMismatch,    pub type_mismatch),
    (PlistError,      pub plist_error),
    (AlistError,      pub alist_error),
    (MissingArgument, pub missing_argument),
    (ArityMismatch,   pub(crate) arity_mismatch),
    (Undefined,       pub(crate) undefined),
    (Uninitialized,   pub(crate) uninitialized),
    (ParsingError,    pub(crate) parsing_error),
    (SyntaxError,     pub(crate) syntax_error),
    (Throw(TulispObject)), // Custom constructor below
);

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
        if self.desc.is_empty() {
            write!(f, "ERR {}", self.kind)?;
        } else {
            write!(f, "ERR {}: {}", self.kind, self.desc)?;
        }
        for span_obj in &self.backtrace {
            if span_obj.numberp() || span_obj.symbolp() || span_obj.stringp() {
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

    /// Creates a new `Throw` error with the given tag and value.
    pub fn throw(tag: TulispObject, value: TulispObject) -> Self {
        Self {
            kind: ErrorKind::Throw(TulispObject::cons(tag, value)),
            desc: String::new(),
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
        let mut span_str = if self.desc.is_empty() {
            format!("ERR {}", self.kind)
        } else {
            format!("ERR {}: {}", self.kind, self.desc)
        };
        for span in &self.backtrace {
            let prefix = self.format_span(ctx, span);
            if prefix.is_empty() {
                continue;
            }
            if span.numberp() || span.symbolp() || span.stringp() {
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
    /// 1. `eval_basic` and `eval_form` wrap an inner result with
    ///    `with_trace(expr.clone())` once each, in nested order.
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
        self.desc.to_owned()
    }
}
