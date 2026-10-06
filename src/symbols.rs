//! What a context knows about a name: its kind, its signature and its
//! docstring, as [`TulispContext::describe`](crate::TulispContext::describe)
//! gives them.

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
    pub type_name: Option<String>,
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
    pub(crate) fn new(kind: SymbolKind, signature: Option<Signature>, doc: Option<String>) -> Self {
        SymbolInfo {
            kind,
            signature,
            doc,
        }
    }
}
