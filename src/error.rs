use crate::value::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum ErrorKind {
    NotImplemented,
    ParsingError,
    TypeMismatch,
    Undefined,
    Uninitialized,
    SyntaxError,
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
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Error {
    kind: ErrorKind,
    desc: String,
    span: Option<Span>,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "ERROR:{}: {}, in {:?}",
            self.kind, self.desc, self.span
        ))
    }
}

impl Error {
    pub fn new(kind: ErrorKind, desc: String) -> Self {
        Error {
            kind,
            desc,
            span: None,
        }
    }
    pub fn with_span(mut self, span: Option<Span>) -> Self {
        if self.span == None {
            self.span = span;
        }
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
