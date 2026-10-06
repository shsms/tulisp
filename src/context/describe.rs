//! What a context knows about each name, for editor tools.

use std::borrow::Cow;

use crate::symbols::{ParamPosition, Signature, SignatureParam, SymbolInfo, SymbolKind};
use crate::{Error, TulispContext, TulispObject, TulispValue};

/// The kind of name a value makes.
pub(crate) fn kind_of(value: &TulispValue) -> SymbolKind {
    match value {
        TulispValue::Defun { .. } | TulispValue::CompiledDefun { .. } => SymbolKind::Function,
        TulispValue::Macro(_) | TulispValue::Defmacro { .. } => SymbolKind::Macro,
        TulispValue::Special { .. } | TulispValue::SpecialForm => SymbolKind::SpecialForm,
        TulispValue::Nil
        | TulispValue::T
        | TulispValue::Symbol { .. }
        | TulispValue::Number { .. }
        | TulispValue::String { .. }
        | TulispValue::List { .. }
        | TulispValue::Quote { .. }
        | TulispValue::Backquote { .. }
        | TulispValue::Unquote { .. }
        | TulispValue::Splice { .. }
        | TulispValue::Any(_)
        | TulispValue::Bounce => SymbolKind::Variable,
    }
}

/// The identity of a function value, by the address of its shared parts, or
/// `None` for a value that is not a function. An address can be reused once the
/// old value is freed, so a stale match is possible, but it needs that exact
/// reuse.
pub(crate) fn value_identity(value: &TulispValue) -> Option<usize> {
    match value {
        TulispValue::Defun { call, .. } => Some(call.addr_as_usize()),
        TulispValue::Special { call, .. } => Some(call.addr_as_usize()),
        TulispValue::Macro(func) => Some(func.addr_as_usize()),
        TulispValue::CompiledDefun { value } => Some(value.code_addr()),
        TulispValue::Defmacro { compiled, .. } => Some(compiled.addr_as_usize()),
        TulispValue::SpecialForm => Some(0),
        TulispValue::Nil
        | TulispValue::T
        | TulispValue::Symbol { .. }
        | TulispValue::Number { .. }
        | TulispValue::String { .. }
        | TulispValue::List { .. }
        | TulispValue::Quote { .. }
        | TulispValue::Backquote { .. }
        | TulispValue::Unquote { .. }
        | TulispValue::Splice { .. }
        | TulispValue::Any(_)
        | TulispValue::Bounce => None,
    }
}

/// The kind and the `value_identity` of what a name holds: VALUE, or nothing
/// for a variable declared with `defvar` and never set.
fn held_kind_and_identity(value: Option<&TulispObject>) -> (SymbolKind, Option<usize>) {
    value.map_or((SymbolKind::Variable, None), |value| {
        let value = &value.inner_ref().0;
        (kind_of(value), value_identity(value))
    })
}

/// What `describe` reads of SYM, interned as NAME: its global value, if it has
/// one. `None` where `describe` gives `None`: for a keyword, and for a name
/// with no value that was not declared with `defvar`.
fn described_value(name: &str, sym: &TulispObject) -> Option<Option<TulispObject>> {
    if name.starts_with(':') {
        return None;
    }
    let value = sym.global();
    (value.is_some() || sym.is_special()).then_some(value)
}

/// The signature a value itself shows: an arity, or a Lisp parameter list's
/// names.
pub(crate) fn derived_signature(value: &TulispValue) -> Option<Signature> {
    match value {
        TulispValue::Defun { arity, .. } | TulispValue::Special { arity, .. } => {
            Some(Signature::from_arity(arity))
        }
        TulispValue::CompiledDefun { value } => {
            let (required, optional, rest) = value.params();
            let param = |p: &TulispObject, position| SignatureParam {
                name: p.as_symbol().ok(),
                position,
                type_name: None,
            };
            let mut params: Vec<SignatureParam> = required
                .iter()
                .map(|p| param(p, ParamPosition::Required))
                .collect();
            params.extend(optional.iter().map(|p| param(p, ParamPosition::Optional)));
            params.extend(rest.map(|p| param(p, ParamPosition::Rest)));
            Some(Signature { params })
        }
        TulispValue::Defmacro { lambda, .. } => {
            let params = lambda.cadr().ok()?;
            let names: Vec<String> = params
                .base_iter()
                .filter_map(|p| p.as_symbol().ok())
                .collect();
            Some(Signature::from_lambda_list(
                names.iter().map(String::as_str),
            ))
        }
        TulispValue::Nil
        | TulispValue::T
        | TulispValue::Symbol { .. }
        | TulispValue::Number { .. }
        | TulispValue::String { .. }
        | TulispValue::List { .. }
        | TulispValue::Quote { .. }
        | TulispValue::Backquote { .. }
        | TulispValue::Unquote { .. }
        | TulispValue::Splice { .. }
        | TulispValue::Any(_)
        | TulispValue::Bounce
        | TulispValue::Macro(_)
        | TulispValue::SpecialForm => None,
    }
}

/// The docstring a value itself holds: a Lisp function's, on its compiled code,
/// or a Lisp macro's, in its body.
fn derived_doc(value: &TulispValue) -> Option<String> {
    match value {
        TulispValue::CompiledDefun { value } => value.doc().map(str::to_string),
        TulispValue::Defmacro { lambda, .. } => {
            let body = lambda.cddr().ok()?;
            crate::builtin::docstring(&body).ok().flatten()
        }
        TulispValue::Nil
        | TulispValue::T
        | TulispValue::Symbol { .. }
        | TulispValue::Number { .. }
        | TulispValue::String { .. }
        | TulispValue::List { .. }
        | TulispValue::Quote { .. }
        | TulispValue::Backquote { .. }
        | TulispValue::Unquote { .. }
        | TulispValue::Splice { .. }
        | TulispValue::Any(_)
        | TulispValue::Bounce
        | TulispValue::Defun { .. }
        | TulispValue::Special { .. }
        | TulispValue::Macro(_)
        | TulispValue::SpecialForm => None,
    }
}

/// What the context records of a name's function beyond the value: the
/// signature a Rust registration declared, and a docstring. `describe` uses it
/// only while the name still holds the value it was recorded for. A variable's
/// docstring is kept apart from it.
pub(crate) struct FunctionDoc {
    pub(crate) kind: SymbolKind,
    /// The `value_identity` of the value the entry describes.
    pub(crate) identity: usize,
    pub(crate) signature: Option<Signature>,
    pub(crate) doc: Option<Cow<'static, str>>,
}

impl FunctionDoc {
    /// Whether the entry describes a value of KIND with IDENTITY.
    fn describes(&self, kind: SymbolKind, identity: usize) -> bool {
        self.kind == kind && self.identity == identity
    }
}

impl TulispContext {
    /// Sets, or with `None` removes, the function entry for SYM. A variable's
    /// docstring stays. Only a symbol interned under its name has an entry:
    /// describe looks names up in the obarray.
    pub(crate) fn set_function_doc(&mut self, sym: &TulispObject, entry: Option<FunctionDoc>) {
        let Some(name) = self.interned_name(sym) else {
            return;
        };
        match entry {
            Some(entry) => {
                self.function_docs.insert(name, entry);
            }
            None => {
                self.function_docs.remove(&name);
            }
        }
    }

    /// Whether SYM's function doc stays when SYM's global value goes from OLD
    /// to NEW: when it describes OLD, and NEW has OLD's kind and identity.
    pub(crate) fn function_doc_stays(
        &self,
        sym: &TulispObject,
        old: &TulispObject,
        new: &TulispObject,
    ) -> bool {
        let (kind, identity) = held_kind_and_identity(Some(old));
        let Some(identity) = identity else {
            return false;
        };
        held_kind_and_identity(Some(new)) == (kind, Some(identity))
            && self
                .interned_name(sym)
                .and_then(|name| self.function_docs.get(&name))
                .is_some_and(|entry| entry.describes(kind, identity))
    }

    /// Attaches DOC to what NAME holds, a function or a variable, for
    /// [`describe`](Self::describe) and the editor tools built on it. A last
    /// line `(fn HOST &optional PORT)` after a blank line, as Emacs writes it,
    /// gives the parameter names to show.
    ///
    /// A function's docstring goes when NAME is defined again, so call this
    /// after the `defun` it documents. A variable's docstring stays whatever
    /// NAME is given.
    ///
    /// Returns an Error if NAME has no value and was not declared with
    /// `defvar`.
    ///
    /// ```rust
    /// use tulisp::TulispContext;
    ///
    /// let mut ctx = TulispContext::new();
    /// ctx.defun("connect", |host: String, port: Option<i64>| {
    ///     format!("{host}:{}", port.unwrap_or(80))
    /// });
    /// ctx.set_doc("connect", "Connect to HOST.\n\n(fn HOST &optional PORT)").unwrap();
    /// let info = ctx.describe("connect").unwrap();
    /// assert_eq!(info.doc.as_deref(), Some("Connect to HOST."));
    /// assert_eq!(info.signature.unwrap().render("connect"), "(connect HOST &optional PORT)");
    /// ```
    pub fn set_doc(&mut self, name: &str, doc: &str) -> Result<(), Error> {
        let Some(value) = self
            .obarray
            .get(name)
            .and_then(|sym| described_value(name, sym))
        else {
            return Err(Error::invalid_argument(format!(
                "set_doc: {name} has no value"
            )));
        };
        self.attach_doc(name, value.as_ref(), Cow::Owned(doc.to_string()));
        Ok(())
    }

    /// Attaches DOC to NAME, which holds VALUE, or nothing for a variable
    /// declared with `defvar` and never set. A function's docstring goes in its
    /// entry, keeping the entry's signature when the entry describes VALUE; any
    /// other value's is the variable's docstring.
    fn attach_doc(&mut self, name: &str, value: Option<&TulispObject>, doc: Cow<'static, str>) {
        let (kind, identity) = held_kind_and_identity(value);
        let Some(identity) = identity else {
            self.variable_docs.insert(name.to_string(), doc);
            return;
        };
        match self.function_docs.get_mut(name) {
            Some(entry) if entry.describes(kind, identity) => {
                entry.doc = Some(doc);
            }
            Some(_) | None => {
                let entry = FunctionDoc {
                    kind,
                    identity,
                    signature: None,
                    doc: Some(doc),
                };
                self.function_docs.insert(name.to_string(), entry);
            }
        }
    }

    /// Attaches a built-in's docstring, without copying it. A name with no
    /// value is skipped.
    pub(crate) fn set_builtin_doc(&mut self, name: &str, doc: &'static str) {
        let Some(value) = self.obarray.get(name).and_then(|sym| sym.global()) else {
            return;
        };
        self.attach_doc(name, Some(&value), Cow::Borrowed(doc));
    }

    /// Test-only: the signature NAME's value itself shows, with no docstring's
    /// usage line in the way.
    #[cfg(test)]
    pub(crate) fn arity_signature(&self, name: &str) -> Option<Signature> {
        let value = self.obarray.get(name)?.global()?;
        derived_signature(&value.inner_ref().0)
    }

    /// Records a `defvar` docstring for SYM, when SYM is interned.
    pub(crate) fn set_variable_doc(&mut self, sym: &TulispObject, doc: String) {
        if let Some(name) = self.interned_name(sym) {
            self.variable_docs.insert(name, Cow::Owned(doc));
        }
    }

    /// The name of SYM, when SYM is the symbol interned under that name.
    fn interned_name(&self, sym: &TulispObject) -> Option<String> {
        let name = sym.as_symbol().ok()?;
        self.obarray
            .get(&name)
            .is_some_and(|interned| interned.eq_ptr(sym))
            .then_some(name)
    }

    /// What NAME holds, its signature and its docstring, for editor tools.
    /// `None` when NAME has no value and was not declared with `defvar`, or is
    /// a keyword. It does not intern NAME.
    ///
    /// The kind and the signature come from the value. The docstring is the
    /// function entry's, when the entry describes the value; else the one the
    /// value itself holds; else the variable's, while NAME holds a variable or
    /// was declared with `defvar`.
    pub fn describe(&self, name: &str) -> Option<SymbolInfo> {
        let sym = self.obarray.get(name)?;
        let value = described_value(name, sym)?;
        let (kind, identity) = held_kind_and_identity(value.as_ref());
        let entry = identity.and_then(|identity| {
            self.function_docs
                .get(name)
                .filter(|entry| entry.describes(kind, identity))
        });
        let doc = entry
            .and_then(|entry| entry.doc.as_deref())
            .map(Cow::Borrowed)
            .or_else(|| {
                value
                    .as_ref()
                    .and_then(|value| derived_doc(&value.inner_ref().0))
                    .map(Cow::Owned)
            })
            .or_else(|| {
                (kind == SymbolKind::Variable || sym.is_special())
                    .then(|| self.variable_docs.get(name))
                    .flatten()
                    .map(|doc| Cow::Borrowed(doc.as_ref()))
            });
        Some(SymbolInfo::from_doc(kind, doc, || {
            entry.and_then(|entry| entry.signature.clone()).or_else(|| {
                value
                    .as_ref()
                    .and_then(|value| derived_signature(&value.inner_ref().0))
            })
        }))
    }

    /// Every name [`symbols`](Self::symbols) lists, with the kind
    /// [`describe`](Self::describe) gives it, without describing it.
    pub(crate) fn symbol_kinds(&self) -> impl Iterator<Item = (&str, SymbolKind)> + '_ {
        self.obarray.iter().filter_map(|(name, sym)| {
            let value = described_value(name, sym)?;
            Some((name.as_str(), held_kind_and_identity(value.as_ref()).0))
        })
    }

    /// Every name that has a value or was declared with `defvar`, with what
    /// [`describe`](Self::describe) says of it, in no particular order. Symbols
    /// that were only interned, and keywords, are left out.
    pub fn symbols(&self) -> impl Iterator<Item = (&str, SymbolInfo)> + '_ {
        self.obarray
            .keys()
            .filter_map(|name| Some((name.as_str(), self.describe(name)?)))
    }
}

#[cfg(test)]
mod tests {
    use crate::TulispContext;
    use crate::symbols::SymbolKind;

    fn rendered(ctx: &TulispContext, name: &str) -> String {
        let info = ctx
            .describe(name)
            .unwrap_or_else(|| panic!("{name} is not defined"));
        let signature = info
            .signature
            .unwrap_or_else(|| panic!("{name} has no signature"));
        signature.render(name)
    }

    #[test]
    fn describe_reports_each_kind() {
        let mut ctx = TulispContext::new();
        ctx.defun("rust-fn", |a: i64, b: Option<i64>| a + b.unwrap_or(0));
        ctx.eval_string(
            "(defun lisp-fn (x &optional y &rest z) x)
             (defmacro lisp-mac (a) a)
             (defvar some-var 1)
             (defvar declared-var)",
        )
        .unwrap();
        let kind = |name: &str| ctx.describe(name).map(|info| info.kind);
        assert_eq!(kind("rust-fn"), Some(SymbolKind::Function));
        assert_eq!(kind("lisp-fn"), Some(SymbolKind::Function));
        assert_eq!(kind("lisp-mac"), Some(SymbolKind::Macro));
        assert_eq!(kind("if"), Some(SymbolKind::SpecialForm));
        assert_eq!(kind("some-var"), Some(SymbolKind::Variable));
        assert_eq!(kind("declared-var"), Some(SymbolKind::Variable));
        assert_eq!(kind("never-defined"), None);
    }

    #[test]
    fn lisp_definitions_have_their_parameter_names() {
        let mut ctx = TulispContext::new();
        ctx.eval_string(
            "(defun lisp-fn (x &optional y &rest z) x) (defmacro lisp-mac (a &rest b) a)",
        )
        .unwrap();
        assert_eq!(rendered(&ctx, "lisp-fn"), "(lisp-fn X &optional Y &rest Z)");
        assert_eq!(rendered(&ctx, "lisp-mac"), "(lisp-mac A &rest B)");
    }

    #[test]
    fn a_rust_function_has_its_arity() {
        let mut ctx = TulispContext::new();
        ctx.defun("rust-fn", |a: i64, b: Option<i64>| a + b.unwrap_or(0));
        assert_eq!(
            rendered(&ctx, "rust-fn"),
            "(rust-fn INTEGER &optional INTEGER)"
        );
    }

    #[test]
    fn a_name_setq_over_a_function_is_a_variable() {
        let mut ctx = TulispContext::new();
        ctx.defun("f", || 1);
        ctx.eval_string("(setq f 5)").unwrap();
        let info = ctx.describe("f").unwrap();
        assert_eq!(info.kind, SymbolKind::Variable);
        assert_eq!(info.signature, None);
    }

    #[test]
    fn a_defvar_doc_survives_a_function_value() {
        let mut ctx = TulispContext::new();
        ctx.eval_string("(defvar hv 1 \"Holds a function.\") (setq hv (lambda (x) x))")
            .unwrap();
        let info = ctx.describe("hv").unwrap();
        assert_eq!(info.kind, SymbolKind::Function);
        assert_eq!(info.doc.as_deref(), Some("Holds a function."));
    }

    #[test]
    fn symbols_lists_bound_and_declared_names_only() {
        let mut ctx = TulispContext::new();
        ctx.eval_string("(defvar declared-var) '(only-interned)")
            .unwrap();
        let names: Vec<&str> = ctx.symbols().map(|(name, _)| name).collect();
        assert!(names.contains(&"declared-var"));
        assert!(names.contains(&"car"));
        assert!(names.contains(&"if"));
        assert!(!names.contains(&"only-interned"));
        assert!(!names.iter().any(|name| name.starts_with(':')));
    }

    #[test]
    fn describe_does_not_intern() {
        let ctx = TulispContext::new();
        let before = ctx.obarray.len();
        assert_eq!(ctx.describe("not-a-name-yet"), None);
        assert_eq!(ctx.obarray.len(), before);
    }

    crate::AsList! {
        struct Cfg {
            a: i64,
        }
    }

    #[derive(Clone)]
    struct Handle;

    impl std::fmt::Display for Handle {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "#<handle>")
        }
    }

    impl crate::TulispAny for Handle {}

    #[test]
    fn rust_parameters_have_lisp_type_names() {
        use crate::{Number, Rest, TulispObject};
        let mut ctx = TulispContext::new();
        ctx.defun(
            "typed",
            |_a: i64,
             _b: f64,
             _c: String,
             _d: Number,
             _e: Vec<i64>,
             _f: TulispObject,
             _g: Rest<String>| {},
        );
        assert_eq!(
            rendered(&ctx, "typed"),
            "(typed INTEGER NUMBER STRING NUMBER LIST ARG &rest STRING)"
        );
    }

    #[test]
    fn a_keyword_tail_shows_as_keywords() {
        let mut ctx = TulispContext::new();
        ctx.defun("cfg", |scale: i64, c: crate::Plist<Cfg>| scale + c.a);
        assert_eq!(rendered(&ctx, "cfg"), "(cfg INTEGER &key ARG)");
    }

    #[test]
    fn a_host_type_parameter_has_its_type_name() {
        let mut ctx = TulispContext::new();
        ctx.defun("h", |_h: Handle| {});
        assert_eq!(rendered(&ctx, "h"), "(h HANDLE)");
    }

    #[test]
    fn a_special_form_from_rust_has_its_parameters() {
        let mut ctx = TulispContext::new();
        ctx.defspecial("sp", |_form: crate::Form, b: i64| b);
        assert_eq!(rendered(&ctx, "sp"), "(sp ARG INTEGER)");
    }

    #[test]
    fn registering_again_replaces_the_signature() {
        let mut ctx = TulispContext::new();
        ctx.defun("f", |a: i64| a);
        ctx.defun("f", |s: String| s);
        assert_eq!(rendered(&ctx, "f"), "(f STRING)");
    }

    #[test]
    fn fmakunbound_drops_the_entry() {
        let mut ctx = TulispContext::new();
        ctx.defun("gone", |a: i64| a);
        assert!(ctx.function_docs.contains_key("gone"));
        ctx.fmakunbound("gone").unwrap();
        assert!(!ctx.function_docs.contains_key("gone"));
    }

    #[test]
    fn a_lambda_set_over_a_rust_function_has_its_own_signature() {
        let mut ctx = TulispContext::new();
        ctx.defun("f", |a: i64| a);
        ctx.eval_string("(setq f (lambda (x) x))").unwrap();
        assert_eq!(rendered(&ctx, "f"), "(f X)");
    }

    #[test]
    fn fset_and_defmacro_replace_a_rust_entry() {
        let mut ctx = TulispContext::new();
        ctx.defun("g", |a: i64| a);
        let lambda = ctx.eval_string("(lambda (y) y)").unwrap();
        ctx.fset("g", lambda).unwrap();
        assert_eq!(rendered(&ctx, "g"), "(g Y)");
        ctx.defun("h", |a: i64| a);
        ctx.eval_string("(defmacro h (z) z)").unwrap();
        assert_eq!(rendered(&ctx, "h"), "(h Z)");
    }

    #[test]
    fn a_zero_parameter_function_has_an_empty_signature() {
        let mut ctx = TulispContext::new();
        ctx.defun("z", || 1);
        ctx.defspecial("zs", || 1);
        assert_eq!(rendered(&ctx, "z"), "(z)");
        assert_eq!(rendered(&ctx, "zs"), "(zs)");
    }

    fn doc(ctx: &TulispContext, name: &str) -> Option<String> {
        ctx.describe(name).and_then(|info| info.doc)
    }

    #[test]
    fn set_doc_attaches_a_docstring() {
        let mut ctx = TulispContext::new();
        ctx.defun("f", |a: i64| a);
        ctx.set_doc("f", "Return A.").unwrap();
        assert_eq!(doc(&ctx, "f").as_deref(), Some("Return A."));
        assert_eq!(rendered(&ctx, "f"), "(f INTEGER)");
    }

    #[test]
    fn a_usage_line_names_the_parameters() {
        let mut ctx = TulispContext::new();
        ctx.defun("f", |a: String, b: Option<i64>| format!("{a}{b:?}"));
        ctx.set_doc("f", "Connect.\n\n(fn HOST &optional PORT)")
            .unwrap();
        assert_eq!(doc(&ctx, "f").as_deref(), Some("Connect."));
        assert_eq!(rendered(&ctx, "f"), "(f HOST &optional PORT)");
    }

    #[test]
    fn a_docstring_can_be_only_a_usage_line() {
        let mut ctx = TulispContext::new();
        ctx.defun("f", |a: i64| a);
        ctx.set_doc("f", "(fn A)").unwrap();
        assert_eq!(doc(&ctx, "f"), None);
        assert_eq!(rendered(&ctx, "f"), "(f A)");
    }

    #[test]
    fn a_malformed_usage_line_is_text() {
        let mut ctx = TulispContext::new();
        ctx.defun("f", |a: i64| a);
        ctx.set_doc("f", "Text.\n\n(fn (A))").unwrap();
        assert_eq!(doc(&ctx, "f").as_deref(), Some("Text.\n\n(fn (A))"));
        assert_eq!(rendered(&ctx, "f"), "(f INTEGER)");
    }

    #[test]
    fn usage_lines_take_emacs_notations() {
        let mut ctx = TulispContext::new();
        ctx.defun("f", |a: i64| a);
        ctx.set_doc("f", "X.\n\n(fn VARLIST BODY...)").unwrap();
        assert_eq!(rendered(&ctx, "f"), "(f VARLIST &rest BODY)");
        ctx.set_doc("f", "X.\n\n(fn A [B])").unwrap();
        assert_eq!(rendered(&ctx, "f"), "(f A &optional B)");
    }

    #[test]
    fn set_doc_on_an_unknown_name_is_an_error() {
        let mut ctx = TulispContext::new();
        assert!(ctx.set_doc("nothing-here", "Doc.").is_err());
    }

    #[test]
    fn a_variable_keeps_its_defvar_docstring() {
        let mut ctx = TulispContext::new();
        ctx.eval_string("(defvar v 1 \"The v.\")").unwrap();
        assert_eq!(doc(&ctx, "v").as_deref(), Some("The v."));
        ctx.eval_string("(setq v 2)").unwrap();
        assert_eq!(doc(&ctx, "v").as_deref(), Some("The v."));
    }

    #[test]
    fn lisp_definitions_keep_their_docstrings() {
        let mut ctx = TulispContext::new();
        ctx.eval_string(
            "(defun f (a) \"Doc of f.\" a)
             (defun g () \"only the value\")
             (defun h () \"Doc of h.\" (declare (indent 0)) 1)
             (defun k () \"the value\" (declare (indent 0)))
             (defmacro m (a) \"Doc of m.\" a)",
        )
        .unwrap();
        assert_eq!(doc(&ctx, "f").as_deref(), Some("Doc of f."));
        assert_eq!(doc(&ctx, "g"), None);
        assert_eq!(doc(&ctx, "h").as_deref(), Some("Doc of h."));
        assert_eq!(doc(&ctx, "k"), None);
        assert_eq!(doc(&ctx, "m").as_deref(), Some("Doc of m."));
    }

    #[test]
    fn redefining_drops_the_docstring() {
        let mut ctx = TulispContext::new();
        // Rust over Rust.
        ctx.defun("a", || 1);
        ctx.set_doc("a", "Old.").unwrap();
        ctx.defun("a", || 2);
        assert_eq!(doc(&ctx, "a"), None);
        // Lisp over Lisp, in one program.
        ctx.eval_string("(defun b () \"Old.\" 1) (defun b () 2)")
            .unwrap();
        assert_eq!(doc(&ctx, "b"), None);
        // Lisp over Rust.
        ctx.defun("c", || 1);
        ctx.set_doc("c", "Old.").unwrap();
        ctx.eval_string("(defun c () 2)").unwrap();
        assert_eq!(doc(&ctx, "c"), None);
        // Rust over Lisp.
        ctx.eval_string("(defun d () \"Old.\" 1)").unwrap();
        ctx.defun("d", || 2);
        assert_eq!(doc(&ctx, "d"), None);
        // fset.
        ctx.defun("e", || 1);
        ctx.set_doc("e", "Old.").unwrap();
        let function = ctx.eval_string("(lambda () 2)").unwrap();
        ctx.fset("e", function).unwrap();
        assert_eq!(doc(&ctx, "e"), None);
    }

    #[test]
    fn a_defvar_doc_outlives_a_defun() {
        let mut ctx = TulispContext::new();
        ctx.eval_string("(defvar hv nil \"Var doc.\") (defun hv (x) x)")
            .unwrap();
        assert_eq!(doc(&ctx, "hv").as_deref(), Some("Var doc."));
    }

    #[test]
    fn a_defvar_doc_outlives_fset() {
        let mut ctx = TulispContext::new();
        ctx.eval_string("(defvar hv nil \"Var doc.\")").unwrap();
        let lambda = ctx.eval_string("(lambda (x) x)").unwrap();
        ctx.fset("hv", lambda).unwrap();
        assert_eq!(doc(&ctx, "hv").as_deref(), Some("Var doc."));
    }

    #[test]
    fn a_defvar_doc_outlives_a_defmacro() {
        let mut ctx = TulispContext::new();
        ctx.eval_string("(defvar hv nil \"Var doc.\") (defmacro hv (x) x)")
            .unwrap();
        assert_eq!(doc(&ctx, "hv").as_deref(), Some("Var doc."));
    }

    #[test]
    fn a_defvar_doc_comes_back_after_a_documented_defun() {
        let mut ctx = TulispContext::new();
        ctx.eval_string("(defvar hv nil \"Var doc.\") (defun hv (x) \"Fn doc.\" x)")
            .unwrap();
        assert_eq!(doc(&ctx, "hv").as_deref(), Some("Fn doc."));
        ctx.eval_string("(setq hv 5)").unwrap();
        assert_eq!(doc(&ctx, "hv").as_deref(), Some("Var doc."));
    }

    #[test]
    fn a_defvar_doc_does_not_replace_a_rust_functions_doc() {
        let mut ctx = TulispContext::new();
        ctx.defun("rf", |a: i64| a);
        ctx.set_doc("rf", "Rust doc.").unwrap();
        ctx.eval_string("(defvar rf nil \"Var doc.\")").unwrap();
        assert_eq!(doc(&ctx, "rf").as_deref(), Some("Rust doc."));
        assert_eq!(rendered(&ctx, "rf"), "(rf INTEGER)");
    }

    #[test]
    fn a_lambda_keeps_its_docstring() {
        let mut ctx = TulispContext::new();
        ctx.eval_string("(setq f1 (lambda (x) \"Lam doc.\" x))")
            .unwrap();
        assert_eq!(doc(&ctx, "f1").as_deref(), Some("Lam doc."));
        let lambda = ctx.eval_string("(lambda (x) \"Lam doc.\" x)").unwrap();
        ctx.fset("f2", lambda).unwrap();
        assert_eq!(doc(&ctx, "f2").as_deref(), Some("Lam doc."));
    }

    #[test]
    fn a_function_set_under_another_name_keeps_its_docstring() {
        let mut ctx = TulispContext::new();
        ctx.eval_string("(defun orig (a) \"Orig doc.\" a)").unwrap();
        let function = ctx.intern("orig").global().expect("orig's function");
        ctx.fset("ali", function).unwrap();
        assert_eq!(doc(&ctx, "ali").as_deref(), Some("Orig doc."));
    }

    // fset of the value a name already holds keeps its docstring and signature.
    #[test]
    fn fset_of_the_same_value_keeps_its_entry() {
        let mut ctx = TulispContext::new();
        ctx.defun("q", |a: i64| a);
        ctx.set_doc("q", "NewQ.\n\n(fn NUM)").unwrap();
        let own = ctx.intern("q").global().expect("q's function");
        ctx.fset("q", own).unwrap();
        assert_eq!(doc(&ctx, "q").as_deref(), Some("NewQ."));
        assert_eq!(rendered(&ctx, "q"), "(q NUM)");
    }

    // fset of another value removes the name's entry from the table.
    #[test]
    fn fset_of_another_value_removes_the_entry() {
        let mut ctx = TulispContext::new();
        ctx.defun("q", |a: i64| a);
        ctx.set_doc("q", "NewQ.").unwrap();
        let lambda = ctx.eval_string("(lambda (x) x)").unwrap();
        ctx.fset("q", lambda).unwrap();
        assert!(!ctx.function_docs.contains_key("q"));
    }

    // A capturing `defun` that a program makes as it runs drops the entry of
    // the function it replaces.
    #[test]
    fn a_capturing_defun_made_at_run_time_drops_the_entry() {
        let mut ctx = TulispContext::new();
        ctx.eval_string("(defun make-cf () (let ((x 1)) (defun cf () \"Lisp doc.\" x)))")
            .unwrap();
        ctx.defun("cf", |a: i64| a);
        ctx.set_doc("cf", "Rust doc.").unwrap();
        ctx.eval_string("(make-cf)").unwrap();
        assert!(!ctx.function_docs.contains_key("cf"));
        assert_eq!(doc(&ctx, "cf").as_deref(), Some("Lisp doc."));
    }

    // A Lisp `defun` drops the function entry of the name it defines.
    #[test]
    fn defun_drops_the_function_entry() {
        let mut ctx = TulispContext::new();
        ctx.eval_string("(defun b () \"Old.\" 1) (defun b () 2)")
            .unwrap();
        assert_eq!(doc(&ctx, "b"), None);
        assert!(!ctx.function_docs.contains_key("b"));
        ctx.defun("rb", |a: i64| a);
        ctx.set_doc("rb", "Rust doc.").unwrap();
        assert!(ctx.function_docs.contains_key("rb"));
        ctx.eval_string("(defun rb () 2)").unwrap();
        assert_eq!(doc(&ctx, "rb"), None);
        assert!(!ctx.function_docs.contains_key("rb"));
    }

    // A Rust parameter's type name is kept as the registration gave it.
    #[test]
    fn a_type_name_is_kept_as_given() {
        let mut ctx = TulispContext::new();
        ctx.defun("tn", |a: i64| a);
        let signature = ctx.describe("tn").and_then(|info| info.signature);
        let params = signature.expect("a signature").params;
        assert!(matches!(
            params[0].type_name,
            Some(std::borrow::Cow::Borrowed("integer"))
        ));
    }
}
