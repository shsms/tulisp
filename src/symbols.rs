//! What a context knows about a name: its kind, its signature and its
//! docstring, as [`TulispContext::describe`](crate::TulispContext::describe)
//! gives them.

use std::borrow::Cow;

use crate::value::DefunArity;

/// What a name holds. A function and a variable of the same name share one
/// value in Tulisp, so a name is one of these at a time.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub enum SymbolKind {
    Function,
    Macro,
    /// A special form, built in or made with
    /// [`defspecial`](crate::TulispContext::defspecial).
    SpecialForm,
    Variable,
}

/// Which arguments a parameter takes.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub enum ParamPosition {
    Required,
    /// After `&optional`: it may be left out.
    Optional,
    /// After `&rest`: every remaining argument.
    Rest,
    /// Every remaining argument, as keyword/value pairs.
    Keywords,
}

/// One parameter of a [`Signature`].
#[derive(Clone, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub struct SignatureParam {
    pub name: Option<String>,
    pub position: ParamPosition,
    /// The Lisp type a Rust function's parameter converts from, such as
    /// `integer`.
    pub type_name: Option<Cow<'static, str>>,
}

impl SignatureParam {
    /// How the parameter shows in a signature: its name, else its type, in
    /// capitals as Emacs writes parameters, else `ARG`.
    pub fn label(&self) -> String {
        self.name
            .as_deref()
            .or(self.type_name.as_deref())
            .map_or_else(|| "ARG".to_string(), str::to_uppercase)
    }
}

/// The parameters a function, macro or special form takes.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
#[non_exhaustive]
pub struct Signature {
    pub params: Vec<SignatureParam>,
}

impl Signature {
    /// The signature as Emacs shows one: `(NAME A &optional B &rest C)`, with
    /// `&key` before keyword arguments.
    pub fn render(&self, name: &str) -> String {
        let mut out = format!("({name}");
        let mut section = ParamPosition::Required;
        for param in &self.params {
            if param.position != section {
                section = param.position;
                out.push_str(match section {
                    ParamPosition::Required => "",
                    ParamPosition::Optional => " &optional",
                    ParamPosition::Rest => " &rest",
                    ParamPosition::Keywords => " &key",
                });
            }
            out.push(' ');
            out.push_str(&param.label());
        }
        out.push(')');
        out
    }

    /// A signature with no names, from the counts of an arity.
    pub(crate) fn from_arity(arity: &DefunArity) -> Signature {
        let param = |position| SignatureParam {
            name: None,
            position,
            type_name: None,
        };
        let mut params: Vec<SignatureParam> = (0..arity.required)
            .map(|_| param(ParamPosition::Required))
            .collect();
        params.extend((0..arity.optional).map(|_| param(ParamPosition::Optional)));
        if arity.has_rest {
            params.push(param(ParamPosition::Rest));
        }
        Signature { params }
    }

    /// The signature of a Lisp parameter list: names, with `&optional` and
    /// `&rest` starting their sections.
    pub(crate) fn from_lambda_list<'n>(names: impl IntoIterator<Item = &'n str>) -> Signature {
        let mut position = ParamPosition::Required;
        let mut params = Vec::new();
        for name in names {
            match name {
                "&optional" => position = ParamPosition::Optional,
                "&rest" => position = ParamPosition::Rest,
                _ => params.push(SignatureParam {
                    name: Some(name.to_string()),
                    position,
                    type_name: None,
                }),
            }
        }
        Signature { params }
    }
}

/// What [`TulispContext::describe`](crate::TulispContext::describe) says of a
/// name.
#[derive(Clone, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub struct SymbolInfo {
    pub kind: SymbolKind,
    pub signature: Option<Signature>,
    pub doc: Option<String>,
}

impl SymbolInfo {
    /// The info of a name of KIND. A usage line at the end of DOC gives the
    /// signature in place of SIGNATURE, and is cut from the text.
    pub(crate) fn new(kind: SymbolKind, signature: Option<Signature>, doc: Option<String>) -> Self {
        Self::from_doc(kind, doc.map(Cow::Owned), || signature)
    }

    /// [`new`](Self::new), with DOC owned or borrowed, and SIGNATURE called
    /// only when DOC has no usage line.
    pub(crate) fn from_doc(
        kind: SymbolKind,
        doc: Option<Cow<'_, str>>,
        signature: impl FnOnce() -> Option<Signature>,
    ) -> Self {
        let (signature, doc) = match doc.as_deref().and_then(split_usage) {
            Some((text, usage)) => (Some(usage), (!text.is_empty()).then(|| text.to_string())),
            None => (signature(), doc.map(Cow::into_owned)),
        };
        SymbolInfo {
            kind,
            signature,
            doc,
        }
    }
}

/// Splits a docstring that ends with a usage line, `(fn A &optional B)` after a
/// blank line as Emacs writes it, or that is only that line, into its text and
/// the signature the line gives. In a usage line, `NAME...` takes the remaining
/// arguments and `[NAME]` may be left out. A line that is not a flat list of
/// names is plain text.
pub(crate) fn split_usage(doc: &str) -> Option<(&str, Signature)> {
    let trimmed = doc.trim_end();
    let (text, line) = match trimmed.rfind("\n\n(fn") {
        Some(at) => (&trimmed[..at], &trimmed[at + 2..]),
        None if trimmed.starts_with("(fn") => ("", trimmed),
        None => return None,
    };
    let inner = line.strip_prefix("(fn")?.strip_suffix(')')?;
    if !(inner.is_empty() || inner.starts_with(' ')) || inner.contains(['(', ')', '\n']) {
        return None;
    }
    let mut position = ParamPosition::Required;
    let mut params = Vec::new();
    for word in inner.split_whitespace() {
        match word {
            "&optional" => position = ParamPosition::Optional,
            "&rest" => position = ParamPosition::Rest,
            "&key" => position = ParamPosition::Keywords,
            _ => {
                let (name, at) = if let Some(name) = word.strip_suffix("...") {
                    (name, ParamPosition::Rest)
                } else if let Some(name) = word.strip_prefix('[').and_then(|w| w.strip_suffix(']'))
                {
                    (name, ParamPosition::Optional)
                } else {
                    (word, position)
                };
                params.push(SignatureParam {
                    name: Some(name.to_string()),
                    position: at,
                    type_name: None,
                });
            }
        }
    }
    Some((text.trim_end(), Signature { params }))
}
