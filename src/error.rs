use std::borrow::Cow;

use crate::{TulispContext, TulispObject, object::Span};

mod table;
pub(crate) use table::ErrorTable;

/// A macro for defining the `ErrorKind` enum, the `Display` implementation for
/// it, and the constructors for the `Error` struct. The kinds before the `;`
/// carry only a description; those after it carry a value too, and print
/// through `ErrorKind::fmt_value`.
macro_rules! ErrorKind {
    (
        $($(#[$kind_meta:meta])* ($kind:ident, $ctor:ident)),* $(,)?
        ;
        $($(#[$meta:meta])* $valued:ident $fields:tt),* $(,)?
    ) => {
        /// The kind of error that occurred.
        #[derive(Debug, Clone)]
        #[non_exhaustive]
        pub enum ErrorKind {
            $(
                $(#[$kind_meta])*
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

// Each kind's doc names the Emacs error symbol a `condition-case` handler
// matches it by; `Error::symbol_name` maps them.
ErrorKind!(
    /// An integer overflow, an integer division by zero, or a float that
    /// does not fit an integer. `arith-error` in Lisp.
    (ArithError,      arith_error),
    /// An argument of the right type that the function cannot use, such
    /// as a macro called as a function. `wrong-type-argument` in Lisp.
    (InvalidArgument, invalid_argument),
    /// An error raised by Lisp's `error`, and other errors with no kind
    /// of their own. `error` in Lisp.
    (LispError,       lisp_error),
    /// A feature Tulisp does not have. `not-implemented` in Lisp.
    (NotImplemented,  not_implemented),
    /// An index or a value out of range, or a list that loops back.
    /// `args-out-of-range` in Lisp.
    (OutOfRange,      out_of_range),
    /// A failure from the operating system, such as a file that cannot
    /// be read. `file-error` in Lisp.
    (OSError,         os_error),
    /// Output to a pipe whose reader has gone. `file-error` in Lisp.
    (BrokenPipe,      broken_pipe),
    /// An argument of the wrong type, or setting a constant such as
    /// `nil`. `wrong-type-argument` in Lisp.
    (TypeMismatch,    type_mismatch),
    /// A malformed property list. `wrong-type-argument` in Lisp.
    (PlistError,      plist_error),
    /// A malformed association list. `wrong-type-argument` in Lisp.
    (AlistError,      alist_error),
    /// A function such as `apply` or `<` called with fewer arguments than
    /// it needs. `wrong-number-of-arguments` in Lisp.
    (MissingArgument, missing_argument),
    /// A call with too few or too many arguments for the function's
    /// parameters. `wrong-number-of-arguments` in Lisp.
    (ArityMismatch,   arity_mismatch),
    /// A call to something that is not a function, such as a name that
    /// holds none. Some malformed forms raise it too, such as a `let`
    /// binding with two values or a parameter list with `&rest` right
    /// after `&rest`.
    /// `void-function` in Lisp.
    (Undefined,       undefined),
    /// A variable read while it has no value. `void-variable` in Lisp.
    (Uninitialized,   uninitialized),
    /// Source text that cannot be read, such as an unclosed list or a bad
    /// token. `invalid-read-syntax` in Lisp.
    (ParsingError,    parsing_error),
    /// A form of the wrong shape, such as an unquote outside a backquote,
    /// a malformed `let` binding, or a parameter list that is not a list.
    /// `invalid-read-syntax` in Lisp.
    (SyntaxError,     syntax_error),
    /// A run stopped by [`Interrupt::Stop`](crate::Interrupt::Stop). No
    /// `condition-case` handler catches it.
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
/// Its `Display` shows the kind, the description and a trace of the forms the
/// error passed through, each with its position and, once a context has filled
/// it in, its file name.
#[derive(Clone)]
pub struct Error {
    kind: ErrorKind,
    desc: String,
    backtrace: Vec<TraceEntry>,
}

/// A form an error passed through, with the name of its file once a context
/// filled it in.
#[derive(Clone)]
struct TraceEntry {
    form: TulispObject,
    file: Option<Box<str>>,
}

impl TraceEntry {
    /// Where the entry's form is, when `Display` prints the entry: it
    /// skips a form with no span, and a number, a symbol or a string.
    fn printed_span(&self) -> Option<Span> {
        if self.form.numberp() || self.form.is_symbol_variant() || self.form.stringp() {
            return None;
        }
        self.form.span()
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let desc = self.desc();
        if desc.is_empty() {
            write!(f, "ERR {}", self.kind)?;
        } else {
            write!(f, "ERR {}: {}", self.kind, desc)?;
        }
        for entry in &self.backtrace {
            let Some(span) = entry.printed_span() else {
                continue;
            };
            match &entry.file {
                Some(name) => write!(f, "\n{name}:")?,
                None => write!(f, "\n<file {}>:", span.file_id)?,
            }
            write!(
                f,
                "{}.{}-{}.{}:  at ",
                span.start.0, span.start.1, span.end.0, span.end.1
            )?;
            let string = entry.form.to_string().replace('\n', "\\n");
            if string.len() > 80 {
                write!(f, "{string:.80}...")?;
            } else {
                f.write_str(&string)?;
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

    /// Formats the error into a human-readable string, including backtrace
    /// information, followed by a newline.
    pub fn format(&self, ctx: &TulispContext) -> String {
        let mut text = self.clone().with_file_names(ctx).to_string();
        text.push('\n');
        text
    }

    /// Records, for each trace entry with no file name yet, the name CTX
    /// has for the entry's file id, so the error prints it. A recorded
    /// name stays, and an id CTX has no name for is left unnamed.
    ///
    /// File ids belong to the context that parsed the form. For a form
    /// another context parsed, CTX may have no name for the id, or the
    /// name of a different file: call this with the parsing context first.
    ///
    /// An error from code that runs no context, such as a `TryFrom`
    /// conversion or `from_tulisp`, needs this call to print file names.
    pub fn with_file_names(mut self, ctx: &TulispContext) -> Self {
        for entry in &mut self.backtrace {
            if entry.file.is_none()
                && let Some(span) = entry.printed_span()
            {
                entry.file = ctx
                    .filenames
                    .get(span.file_id)
                    .map(|name| name.as_str().into());
            }
        }
        self
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
        if self
            .backtrace
            .last()
            .is_some_and(|last| last.form.eq(&span))
        {
            return self;
        }
        self.backtrace.push(TraceEntry {
            form: span,
            file: None,
        });
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

    // An error prints the names a context has for the files its trace
    // points into, once they are filled in.
    #[test]
    fn with_file_names_puts_the_names_in_display() {
        let ctx = &mut crate::TulispContext::new();
        let err = ctx.eval_string("(car 5)").unwrap_err().with_file_names(ctx);
        assert!(
            err.to_string()
                .ends_with("\n<eval_string>:1.1-1.7:  at (car 5)"),
            "{err}"
        );
    }

    // A context that does not know a file leaves it for another to name.
    #[test]
    fn a_file_one_context_does_not_know_is_left_for_another() {
        let path =
            std::env::temp_dir().join(format!("tulisp_unknown_file_{}.lisp", std::process::id()));
        std::fs::write(&path, "(car 5)").unwrap();
        let path = path.to_str().unwrap();
        let parsing = &mut crate::TulispContext::new();
        let forms = parsing.parse_file(path).unwrap();
        std::fs::remove_file(path).ok();

        let running = &mut crate::TulispContext::new();
        let err = running.eval_progn(&forms).unwrap_err();
        assert!(err.to_string().ends_with(":1.1-1.7:  at (car 5)"), "{err}");
        assert!(err.to_string().contains("\n<file "), "{err}");
        let err = err.with_file_names(parsing);
        assert!(
            err.to_string().contains(&format!("\n{path}:1.1-1.7:")),
            "{err}"
        );
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
