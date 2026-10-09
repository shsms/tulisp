//! What a context knows about each name, for editor tools.

use std::borrow::Cow;

use crate::symbols::{DocOwner, ParamPosition, Signature, SignatureParam, SymbolInfo, SymbolKind};
use crate::{TulispContext, TulispObject, TulispValue};

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
    /// The docstring, and whose it is: a usage line at the end of a built-in's
    /// gives the signature; at the end of one a Rust definition gave, it is
    /// text.
    pub(crate) doc: Option<(Cow<'static, str>, DocOwner)>,
}

impl FunctionDoc {
    /// Whether the entry describes a value of KIND with IDENTITY.
    fn describes(&self, kind: SymbolKind, identity: usize) -> bool {
        self.kind == kind && self.identity == identity
    }
}

impl TulispContext {
    /// Sets, or with `None` removes, the function entry for SYM. A variable's
    /// docstring stays. With `Some`, SYM must be interned: describe finds
    /// entries through the obarray, which keeps its symbols, and so their
    /// addresses, for good. `None` takes any symbol.
    pub(crate) fn set_function_doc(&mut self, sym: &TulispObject, entry: Option<FunctionDoc>) {
        let addr = sym.addr_as_usize();
        match entry {
            Some(entry) => {
                debug_assert!(self.is_interned(sym));
                self.function_docs.insert(addr, entry);
            }
            None => {
                self.function_docs.remove(&addr);
            }
        }
    }

    /// SYM's function doc, when it describes a value of KIND with IDENTITY.
    fn function_doc(
        &self,
        sym: &TulispObject,
        kind: SymbolKind,
        identity: usize,
    ) -> Option<&FunctionDoc> {
        self.function_docs
            .get(&sym.addr_as_usize())
            .filter(|entry| entry.describes(kind, identity))
    }

    /// Gives the Rust function NAME was just defined with, whose entry
    /// `define_function` made, its parameters' NAMES and its DOC, each when
    /// given. NAMES has one name for each parameter.
    pub(crate) fn document_function(
        &mut self,
        name: &str,
        names: Option<&[&str]>,
        doc: Option<&str>,
    ) {
        if names.is_none() && doc.is_none() {
            return;
        }
        let Some(entry) = self
            .obarray
            .get(name)
            .and_then(|sym| self.function_docs.get_mut(&sym.addr_as_usize()))
        else {
            return;
        };
        if let Some(names) = names
            && let Some(signature) = entry.signature.as_mut()
        {
            for (param, name) in signature.params.iter_mut().zip(names) {
                param.name = Some(name.to_string());
            }
        }
        if let Some(doc) = doc {
            entry.doc = Some((Cow::Owned(doc.to_string()), DocOwner::DefinedFunction));
        }
    }

    /// Attaches DOC to what NAME holds, or to nothing for a variable declared
    /// with `defvar` and never set. A function's docstring goes in its entry,
    /// keeping the entry's signature when the entry describes the value; any
    /// other value's is the variable's docstring. Skips a name that
    /// [`describe`](Self::describe) gives `None` for.
    fn attach_doc(&mut self, name: &str, doc: Cow<'static, str>) {
        let Some((addr, value)) = self
            .obarray
            .get(name)
            .and_then(|sym| Some((sym.addr_as_usize(), described_value(name, sym)?)))
        else {
            return;
        };
        let (kind, identity) = held_kind_and_identity(value.as_ref());
        let Some(identity) = identity else {
            self.variable_docs.insert(addr, doc);
            return;
        };
        let blank_entry = || FunctionDoc {
            kind,
            identity,
            signature: None,
            doc: None,
        };
        let entry = self.function_docs.entry(addr).or_insert_with(blank_entry);
        if !entry.describes(kind, identity) {
            *entry = blank_entry();
        }
        entry.doc = Some((doc, DocOwner::Function));
    }

    /// Attaches a built-in's docstring, without copying it. Skips a name that
    /// [`describe`](Self::describe) gives `None` for.
    pub(crate) fn set_builtin_doc(&mut self, name: &str, doc: &'static str) {
        self.attach_doc(name, Cow::Borrowed(doc));
    }

    /// Test-only: the signature [`describe`](Self::describe) gives NAME, with
    /// no docstring's usage line in the way: the one its function entry holds,
    /// else the value's own.
    #[cfg(test)]
    pub(crate) fn arity_signature(&self, name: &str) -> Option<Signature> {
        let sym = self.obarray.get(name)?;
        let value = sym.global()?;
        let (kind, identity) = held_kind_and_identity(Some(&value));
        identity
            .and_then(|identity| self.function_doc(sym, kind, identity))
            .and_then(|entry| entry.signature.clone())
            .or_else(|| derived_signature(&value.inner_ref().0))
    }

    /// Records a `defvar` docstring for SYM, when SYM is interned.
    pub(crate) fn set_variable_doc(&mut self, sym: &TulispObject, doc: String) {
        if self.is_interned(sym) {
            self.variable_docs
                .insert(sym.addr_as_usize(), Cow::Owned(doc));
        }
    }

    /// Whether SYM is the symbol interned under its name.
    fn is_interned(&self, sym: &TulispObject) -> bool {
        sym.inner_ref().0.symbol_name().is_some_and(|name| {
            self.obarray
                .get(name)
                .is_some_and(|interned| interned.eq_ptr(sym))
        })
    }

    /// What NAME holds, its signature and its docstring, for editor tools.
    /// `None` when NAME has no value and was not declared with `defvar`, or is
    /// a keyword. It does not intern NAME.
    ///
    /// The kind comes from the value. The docstring is the function entry's,
    /// when the entry describes the value; else the one the value itself holds;
    /// else the variable's, while NAME holds a variable or was declared with
    /// `defvar`.
    ///
    /// A usage line at the end of a function's docstring, a flat list of names
    /// like `(fn A &optional B)` after a blank line or as the whole docstring,
    /// gives the signature, and is cut from the docstring; a variable's
    /// docstring is kept whole, as `describe-variable` shows it, and so is one
    /// that [`defun`](Self::defun), [`defspecial`](Self::defspecial) or
    /// [`defmacro`](Self::defmacro) gave, whose parameters come from the
    /// definition. When no usage line gives the signature, it is the one the
    /// function entry holds, else the value's own. A Rust function's parameter
    /// types are kept under the name it was defined with, so another name given
    /// the same function shows plain parameter names.
    pub fn describe(&self, name: &str) -> Option<SymbolInfo> {
        let sym = self.obarray.get(name)?;
        let value = described_value(name, sym)?;
        let (kind, identity) = held_kind_and_identity(value.as_ref());
        let entry = identity.and_then(|identity| self.function_doc(sym, kind, identity));
        let doc = entry
            .and_then(|entry| entry.doc.as_ref())
            .map(|(doc, owner)| (Cow::Borrowed(doc.as_ref()), *owner))
            .or_else(|| {
                value
                    .as_ref()
                    .and_then(|value| derived_doc(&value.inner_ref().0))
                    .map(|doc| (Cow::Owned(doc), DocOwner::Function))
            })
            .or_else(|| {
                (kind == SymbolKind::Variable || sym.is_special())
                    .then(|| self.variable_docs.get(&sym.addr_as_usize()))
                    .flatten()
                    .map(|doc| (Cow::Borrowed(doc.as_ref()), DocOwner::Variable))
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
    use crate::symbols::SymbolKind;
    use crate::{Form, Rest, TulispContext, TulispObject};

    /// Whether NAME has a function entry.
    fn has_function_doc(ctx: &TulispContext, name: &str) -> bool {
        ctx.obarray
            .get(name)
            .is_some_and(|sym| ctx.function_docs.contains_key(&sym.addr_as_usize()))
    }

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
        assert!(has_function_doc(&ctx, "gone"));
        ctx.fmakunbound("gone").unwrap();
        assert!(!has_function_doc(&ctx, "gone"));
    }

    // A docstring attached later replaces the entry left from the Rust
    // function.
    #[test]
    fn a_lambda_set_over_a_rust_function_has_its_own_signature_and_doc() {
        let mut ctx = TulispContext::new();
        ctx.defun("f", |a: i64| a);
        ctx.eval_string("(setq f (lambda (x) x))").unwrap();
        assert_eq!(rendered(&ctx, "f"), "(f X)");
        ctx.set_builtin_doc("f", "New.");
        assert_eq!(doc(&ctx, "f").as_deref(), Some("New."));
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
    fn defun_takes_a_docstring() {
        let mut ctx = TulispContext::new();
        ctx.defun(("f", "Return A."), |a: i64| a);
        assert_eq!(doc(&ctx, "f").as_deref(), Some("Return A."));
        assert_eq!(rendered(&ctx, "f"), "(f INTEGER)");
    }

    #[test]
    fn defun_takes_parameter_names() {
        let mut ctx = TulispContext::new();
        ctx.defun(
            ("f", ["host", "port", "opts"], "Connect."),
            |a: String, b: Option<i64>, c: Rest<i64>| format!("{a}{b:?}{c:?}"),
        );
        assert_eq!(doc(&ctx, "f").as_deref(), Some("Connect."));
        assert_eq!(rendered(&ctx, "f"), "(f HOST &optional PORT &rest OPTS)");
        // The types stay, for tools that show them.
        let info = ctx.describe("f").unwrap();
        let types: Vec<_> = info
            .signature
            .unwrap()
            .params
            .into_iter()
            .map(|param| param.type_name)
            .collect();
        assert_eq!(
            types,
            [
                Some("string".into()),
                Some("integer".into()),
                Some("integer".into())
            ]
        );
    }

    #[test]
    fn defun_with_a_context_parameter_names_the_others() {
        let mut ctx = TulispContext::new();
        ctx.defun(
            ("f", ["form"], "Eval FORM."),
            |ctx: &mut TulispContext, form: TulispObject| ctx.eval(&form),
        );
        assert_eq!(rendered(&ctx, "f"), "(f FORM)");
    }

    #[test]
    fn defspecial_takes_a_docstring_and_parameter_names() {
        let mut ctx = TulispContext::new();
        ctx.defspecial(("s", "Run BODY."), |body: Rest<Form>| body.len() as i64);
        assert_eq!(doc(&ctx, "s").as_deref(), Some("Run BODY."));
        ctx.defspecial(
            ("st", ["test", "body"], "Run BODY when TEST."),
            |_test: Form, body: Rest<Form>| body.len() as i64,
        );
        assert_eq!(doc(&ctx, "st").as_deref(), Some("Run BODY when TEST."));
        assert_eq!(rendered(&ctx, "st"), "(st TEST &rest BODY)");
    }

    #[test]
    fn a_name_can_be_a_string_or_a_reference_to_one() {
        let mut ctx = TulispContext::new();
        ctx.defun(String::from("f"), |a: i64| a);
        let owned = String::from("g");
        ctx.defun(&owned, |a: i64| a);
        let name: &&str = &"h";
        ctx.defun(name, |a: i64| a);
        let boxed: Box<str> = "i".into();
        ctx.defun(&boxed, |a: i64| a);
        let mut borrowed = String::from("k");
        ctx.defun(&mut borrowed, |a: i64| a);
        for name in ["f", "g", "h", "i", "k"] {
            assert_eq!(rendered(&ctx, name), format!("({name} INTEGER)"));
            assert_eq!(doc(&ctx, name), None);
        }
        let doc_text = String::from("Doc.");
        ctx.defun((String::from("j"), &doc_text), |a: i64| a);
        ctx.defun((&owned, ["num"], doc_text.clone()), |a: i64| a);
        assert_eq!(doc(&ctx, "j").as_deref(), Some("Doc."));
        assert_eq!(rendered(&ctx, "g"), "(g NUM)");
    }

    #[test]
    fn defmacro_takes_a_docstring_and_parameter_names() {
        let mut ctx = TulispContext::new();
        ctx.defmacro(
            ("m", "Expand to FORM."),
            |form: TulispObject, _rest: Rest<TulispObject>| form,
        );
        assert_eq!(doc(&ctx, "m").as_deref(), Some("Expand to FORM."));
        assert_eq!(rendered(&ctx, "m"), "(m ARG &rest ARG)");
        ctx.defmacro(
            ("n", ["form", "body"], "Expand to FORM."),
            |form: TulispObject, _body: Rest<TulispObject>| form,
        );
        assert_eq!(rendered(&ctx, "n"), "(n FORM &rest BODY)");
        let info = ctx.describe("n").unwrap();
        assert_eq!(info.kind, SymbolKind::Macro);
        assert_eq!(info.doc.as_deref(), Some("Expand to FORM."));
    }

    #[test]
    fn defvar_takes_a_docstring() {
        let mut ctx = TulispContext::new();
        ctx.defvar(("v", "The v."), 1).unwrap();
        assert_eq!(doc(&ctx, "v").as_deref(), Some("The v."));
        // As in Lisp, a defvar of a name with a value keeps the value and
        // replaces the docstring.
        ctx.defvar(("v", "Still v."), 2).unwrap();
        assert_eq!(doc(&ctx, "v").as_deref(), Some("Still v."));
        assert_eq!(ctx.eval_string("v").unwrap().to_string(), "1");
        // A name alone leaves the docstring.
        ctx.defvar("v", 3).unwrap();
        assert_eq!(doc(&ctx, "v").as_deref(), Some("Still v."));
    }

    // A built-in's docstring can name its parameters, as Emacs's do.
    #[test]
    fn a_usage_line_names_the_parameters() {
        let mut ctx = TulispContext::new();
        ctx.defun("f", |a: String, b: Option<i64>| format!("{a}{b:?}"));
        ctx.set_builtin_doc("f", "Connect.\n\n(fn HOST &optional PORT)");
        assert_eq!(doc(&ctx, "f").as_deref(), Some("Connect."));
        assert_eq!(rendered(&ctx, "f"), "(f HOST &optional PORT)");
    }

    #[test]
    fn a_docstring_can_be_only_a_usage_line() {
        let mut ctx = TulispContext::new();
        ctx.defun("f", |a: i64| a);
        ctx.set_builtin_doc("f", "(fn A)");
        assert_eq!(doc(&ctx, "f"), None);
        assert_eq!(rendered(&ctx, "f"), "(f A)");
    }

    #[test]
    fn a_malformed_usage_line_is_text() {
        let mut ctx = TulispContext::new();
        ctx.defun("f", |a: i64| a);
        ctx.set_builtin_doc("f", "Text.\n\n(fn (A))");
        assert_eq!(doc(&ctx, "f").as_deref(), Some("Text.\n\n(fn (A))"));
        assert_eq!(rendered(&ctx, "f"), "(f INTEGER)");
    }

    #[test]
    fn usage_lines_take_emacs_notations() {
        let mut ctx = TulispContext::new();
        ctx.defun("f", |a: i64| a);
        ctx.set_builtin_doc("f", "X.\n\n(fn VARLIST BODY...)");
        assert_eq!(rendered(&ctx, "f"), "(f VARLIST &rest BODY)");
        ctx.set_builtin_doc("f", "X.\n\n(fn A [B])");
        assert_eq!(rendered(&ctx, "f"), "(f A &optional B)");
    }

    // The definition gives the parameters, so a usage line in the docstring it
    // gives is text.
    #[test]
    fn a_usage_line_in_a_defined_functions_docstring_is_text() {
        let mut ctx = TulispContext::new();
        ctx.defun(("f", "Return A.\n\n(fn NUM)"), |a: i64| a);
        assert_eq!(doc(&ctx, "f").as_deref(), Some("Return A.\n\n(fn NUM)"));
        assert_eq!(rendered(&ctx, "f"), "(f INTEGER)");
        ctx.defmacro(("m", "(fn FORM)"), |form: TulispObject| form);
        assert_eq!(doc(&ctx, "m").as_deref(), Some("(fn FORM)"));
        assert_eq!(rendered(&ctx, "m"), "(m ARG)");
    }

    #[test]
    fn a_variable_keeps_its_defvar_docstring() {
        let mut ctx = TulispContext::new();
        ctx.eval_string("(defvar v 1 \"The v.\")").unwrap();
        assert_eq!(doc(&ctx, "v").as_deref(), Some("The v."));
        ctx.eval_string("(setq v 2)").unwrap();
        assert_eq!(doc(&ctx, "v").as_deref(), Some("The v."));
    }

    // Emacs splits a usage line off a function's docstring only.
    #[test]
    fn a_variable_docstring_keeps_its_usage_line() {
        let mut ctx = TulispContext::new();
        ctx.eval_string("(defvar v 1 \"The v.\n\n(fn A)\")")
            .unwrap();
        let info = ctx.describe("v").unwrap();
        assert_eq!(info.signature, None);
        assert_eq!(info.doc.as_deref(), Some("The v.\n\n(fn A)"));
        // Also when the variable holds a function: the signature is the
        // function's own.
        ctx.eval_string("(defvar w (lambda (x) x) \"The w.\n\n(fn A)\")")
            .unwrap();
        let info = ctx.describe("w").unwrap();
        assert_eq!(info.kind, SymbolKind::Function);
        assert_eq!(info.signature.unwrap().render("w"), "(w X)");
        assert_eq!(info.doc.as_deref(), Some("The w.\n\n(fn A)"));
        // And when the doc comes from a Rust `defvar`.
        ctx.defvar(("u", "The u.\n\n(fn A)"), 2).unwrap();
        assert_eq!(doc(&ctx, "u").as_deref(), Some("The u.\n\n(fn A)"));
    }

    // Only an interned symbol, which `describe` can find, gets a docstring.
    #[test]
    fn an_uninterned_defvar_keeps_no_docstring() {
        let mut ctx = TulispContext::new();
        let before = ctx.variable_docs.len();
        ctx.eval_string("(eval (list 'defvar (make-symbol \"u\") 1 \"Doc.\"))")
            .unwrap();
        assert_eq!(ctx.variable_docs.len(), before);
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
        ctx.defun(("a", "Old."), || 1);
        ctx.defun("a", || 2);
        assert_eq!(doc(&ctx, "a"), None);
        // Lisp over Lisp, in one program.
        ctx.eval_string("(defun b () \"Old.\" 1) (defun b () 2)")
            .unwrap();
        assert_eq!(doc(&ctx, "b"), None);
        // Lisp over Rust.
        ctx.defun(("c", "Old."), || 1);
        ctx.eval_string("(defun c () 2)").unwrap();
        assert_eq!(doc(&ctx, "c"), None);
        // Rust over Lisp.
        ctx.eval_string("(defun d () \"Old.\" 1)").unwrap();
        ctx.defun("d", || 2);
        assert_eq!(doc(&ctx, "d"), None);
        // fset.
        ctx.defun(("e", "Old."), || 1);
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
        ctx.defun(("rf", "Rust doc."), |a: i64| a);
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

    // fset of the very object a name already holds keeps its docstring and
    // signature.
    #[test]
    fn fset_of_the_same_object_keeps_its_entry() {
        let mut ctx = TulispContext::new();
        ctx.defun(("q", ["num"], "NewQ."), |a: i64| a);
        let own = ctx.intern("q").global().expect("q's function");
        ctx.fset("q", own).unwrap();
        assert_eq!(doc(&ctx, "q").as_deref(), Some("NewQ."));
        assert_eq!(rendered(&ctx, "q"), "(q NUM)");
    }

    // fset of another value removes the name's entry from the table.
    #[test]
    fn fset_of_another_value_removes_the_entry() {
        let mut ctx = TulispContext::new();
        ctx.defun(("q", "NewQ."), |a: i64| a);
        let lambda = ctx.eval_string("(lambda (x) x)").unwrap();
        ctx.fset("q", lambda).unwrap();
        assert!(!has_function_doc(&ctx, "q"));
    }

    // fset of another function made from the same code drops the entry.
    #[test]
    fn fset_of_another_closure_of_the_same_code_drops_the_entry() {
        let mut ctx = TulispContext::new();
        ctx.eval_string("(defun mk (n) (lambda () n))").unwrap();
        let first = ctx.eval_string("(mk 1)").unwrap();
        ctx.fset("f", first).unwrap();
        ctx.set_builtin_doc("f", "Set doc.");
        assert_eq!(doc(&ctx, "f").as_deref(), Some("Set doc."));
        let second = ctx.eval_string("(mk 2)").unwrap();
        ctx.fset("f", second).unwrap();
        assert_eq!(doc(&ctx, "f"), None);
        assert!(!has_function_doc(&ctx, "f"));
    }

    // A capturing `defun` made again as the program runs keeps a docstring
    // attached to it later.
    #[test]
    fn a_capturing_defun_made_again_keeps_an_attached_doc() {
        let mut ctx = TulispContext::new();
        ctx.eval_string("(defun make-cf () (let ((x 1)) (defun cf () \"Lisp doc.\" x)))")
            .unwrap();
        ctx.eval_string("(make-cf)").unwrap();
        ctx.set_builtin_doc("cf", "Set doc.");
        ctx.eval_string("(make-cf)").unwrap();
        assert_eq!(doc(&ctx, "cf").as_deref(), Some("Set doc."));
    }

    // A capturing `defun` that a program makes as it runs drops the entry of
    // the function it replaces.
    #[test]
    fn a_capturing_defun_made_at_run_time_drops_the_entry() {
        let mut ctx = TulispContext::new();
        ctx.eval_string("(defun make-cf () (let ((x 1)) (defun cf () \"Lisp doc.\" x)))")
            .unwrap();
        ctx.defun(("cf", "Rust doc."), |a: i64| a);
        ctx.eval_string("(make-cf)").unwrap();
        assert!(!has_function_doc(&ctx, "cf"));
        assert_eq!(doc(&ctx, "cf").as_deref(), Some("Lisp doc."));
    }

    // A closure of other code is not the same defun made again.
    #[test]
    fn a_capturing_defun_made_over_another_closure_drops_the_entry() {
        let mut ctx = TulispContext::new();
        ctx.eval_string("(defun make-cf () (let ((x 1)) (defun cf () \"Lisp doc.\" x)))")
            .unwrap();
        let other = ctx.eval_string("(let ((y 2)) (lambda () y))").unwrap();
        ctx.fset("cf", other).unwrap();
        ctx.set_builtin_doc("cf", "Lambda doc.");
        assert!(has_function_doc(&ctx, "cf"));
        ctx.eval_string("(make-cf)").unwrap();
        assert!(!has_function_doc(&ctx, "cf"));
        assert_eq!(doc(&ctx, "cf").as_deref(), Some("Lisp doc."));
    }

    // A Lisp `defun` drops the function entry of the name it defines.
    #[test]
    fn defun_drops_the_function_entry() {
        let mut ctx = TulispContext::new();
        ctx.eval_string("(defun b () \"Old.\" 1) (defun b () 2)")
            .unwrap();
        assert_eq!(doc(&ctx, "b"), None);
        assert!(!has_function_doc(&ctx, "b"));
        ctx.defun(("rb", "Rust doc."), |a: i64| a);
        assert!(has_function_doc(&ctx, "rb"));
        ctx.eval_string("(defun rb () 2)").unwrap();
        assert_eq!(doc(&ctx, "rb"), None);
        assert!(!has_function_doc(&ctx, "rb"));
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
