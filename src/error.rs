use crate::{TulispContext, TulispObject};

#[derive(Debug, Clone)]
pub enum ErrorKind {
    InvalidArgument,
    LispError,
    MissingArgument,
    NotImplemented,
    OutOfRange,
    ParsingError,
    SyntaxError,
    Throw(TulispObject),
    TypeMismatch,
    Undefined,
    Uninitialized,
}

impl std::fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorKind::InvalidArgument => f.write_str("InvalidArgument"),
            ErrorKind::LispError => f.write_str("LispError"),
            ErrorKind::MissingArgument => f.write_str("MissingArgument"),
            ErrorKind::NotImplemented => f.write_str("NotImplemented"),
            ErrorKind::OutOfRange => f.write_str("OutOfRange"),
            ErrorKind::ParsingError => f.write_str("ParsingError"),
            ErrorKind::SyntaxError => f.write_str("SyntaxError"),
            ErrorKind::Throw(args) => write!(f, "Throw{}", args),
            ErrorKind::TypeMismatch => f.write_str("TypeMismatch"),
            ErrorKind::Undefined => f.write_str("Undefined"),
            ErrorKind::Uninitialized => f.write_str("Uninitialized"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Error {
    kind: ErrorKind,
    desc: String,
    backtrace: Vec<TulispObject>,
}

impl Error {
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

    pub fn format(&self, ctx: &TulispContext) -> String {
        let mut span_str = format!("ERR {}: {}", self.kind, self.desc);
        for span in &self.backtrace {
            let prefix = self.format_span(ctx, span);
            if prefix.is_empty() {
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
    pub fn new(kind: ErrorKind, desc: String) -> Self {
        Error {
            kind,
            desc,
            backtrace: vec![],
        }
    }
    pub fn with_trace(mut self, span: TulispObject) -> Self {
        self.backtrace.push(span);
        self
    }

    #[allow(dead_code)]
    pub fn kind(&self) -> ErrorKind {
        self.kind.clone()
    }

    pub(crate) fn kind_ref(&self) -> &ErrorKind {
        &self.kind
    }

    #[allow(dead_code)]
    pub fn desc(&self) -> String {
        self.desc.to_owned()
    }
}
