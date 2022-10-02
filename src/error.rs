use crate::value::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ErrorKind {
    NotImplemented,
    ParsingError,
    TypeMismatch,
    Undefined,
    Uninitialized,
    SyntaxError,
    MissingArgument,
}

impl std::fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorKind::NotImplemented => f.write_str("NotImplemented"),
            ErrorKind::ParsingError => f.write_str("ParsingError"),
            ErrorKind::TypeMismatch => f.write_str("TypeMismatch"),
            ErrorKind::Undefined => f.write_str("Undefined"),
            ErrorKind::Uninitialized => f.write_str("Uninitialized"),
            ErrorKind::SyntaxError => f.write_str("SyntaxError"),
            ErrorKind::MissingArgument => f.write_str("MissingArgument"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Error {
    kind: ErrorKind,
    desc: String,
    span: Option<Span>,
    filename: String,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let span_str = if let Some(span) = self.span {
            format!(
                "{}.{}-{}.{}:",
                span.start.0, span.start.1, span.end.0, span.end.1
            )
        } else {
            String::new()
        };
        f.write_fmt(format_args!(
            "{}:{} ERR {}: {}",
            self.filename, span_str, self.kind, self.desc
        ))
    }
}

impl Error {
    pub fn new(kind: ErrorKind, desc: String) -> Self {
        Error {
            kind,
            desc,
            span: None,
            filename: String::from("<eval_string>"),
        }
    }
    pub fn with_span(mut self, span: Option<Span>) -> Self {
        if self.span.is_none() {
            self.span = span;
        }
        self
    }

    pub fn with_filename(mut self, filename: String) -> Self {
        self.filename = filename;
        self
    }

    pub fn span(&self) -> Option<Span> {
        self.span.to_owned()
    }

    #[allow(dead_code)]
    pub fn kind(&self) -> ErrorKind {
        self.kind.clone()
    }

    #[allow(dead_code)]
    pub fn desc(&self) -> String {
        self.desc.to_owned()
    }
}
