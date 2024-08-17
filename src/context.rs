use std::{cell::RefCell, collections::HashMap, fs, rc::Rc};

use crate::{
    builtin,
    bytecode::{self, compile, Bytecode, Compiler},
    error::Error,
    eval::{eval, eval_and_then, eval_basic, funcall, DummyEval},
    list,
    parse::parse,
    TulispObject, TulispValue,
};

use crate::bytecode::VMCompilers;

macro_rules! intern_from_obarray {
    ($( #[$meta:meta] )*
     $vis:vis struct $struct_name:ident {
         $($name:ident : $symbol:literal),+ $(,)?
     }) => {
        $( #[$meta] )*
        $vis struct $struct_name {
            $(pub $name: $crate::TulispObject),+
        }

        impl $struct_name {
            fn from_obarray(obarray: &mut std::collections::HashMap<String, $crate::TulispObject>) -> Self {
                $struct_name {
                    $($name: intern_from_obarray!(@intern obarray, $symbol)),+
                }
            }
        }
    };

    (@intern $obarray:ident, $name:literal) => {
        if let Some(sym) = $obarray.get($name) {
            sym.clone_without_span()
        } else {
            let name = $name.to_string();
            let constant = name.starts_with(':');
            let sym = TulispObject::symbol(name.clone(), constant);
            $obarray.insert(name, sym.clone());
            sym
        }
    }
}

#[derive(Debug, Default, Clone)]
pub(crate) struct Scope {
    pub scope: Vec<TulispObject>,
}

impl Scope {
    pub fn set(&mut self, symbol: TulispObject, value: TulispObject) -> Result<(), Error> {
        symbol.set_scope(value)?;
        self.scope.push(symbol);
        Ok(())
    }

    pub fn remove_all(&self) -> Result<(), Error> {
        for item in &self.scope {
            item.unset()?;
        }
        Ok(())
    }
}

intern_from_obarray! {
    #[derive(Clone)]
    pub(crate) struct Keywords {
        amp_optional: "&optional",
        amp_rest: "&rest",
        lambda: "lambda",
    }
}

/// Represents an instance of the _Tulisp_ interpreter.
///
/// Owns the
/// [`obarray`](https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Symbols.html)
/// which keeps track of all interned `Symbol`s.
///
/// All evaluation of _Tulisp_ programs need to be done on a `TulispContext`
/// instance.
pub struct TulispContext {
    obarray: HashMap<String, TulispObject>,
    pub(crate) filenames: Vec<String>,
    pub(crate) compiler: Option<Compiler>,
    pub(crate) keywords: Keywords,
    pub(crate) vm: Rc<RefCell<bytecode::Machine>>,
}

impl Default for TulispContext {
    fn default() -> Self {
        Self::new()
    }
}

impl TulispContext {
    /// Creates a TulispContext with an empty global scope.
    pub fn new() -> Self {
        let mut obarray = HashMap::new();
        let keywords = Keywords::from_obarray(&mut obarray);
        let mut ctx = Self {
            obarray,
            filenames: vec!["<eval_string>".to_string()],
            compiler: None,
            keywords,
            vm: Rc::new(RefCell::new(bytecode::Machine::new())),
        };
        builtin::functions::add(&mut ctx);
        builtin::macros::add(&mut ctx);
        let vm_compilers = VMCompilers::new(&mut ctx);
        ctx.compiler = Some(Compiler::new(vm_compilers));
        ctx
    }

    /// Returns an interned symbol with the given name.
    ///
    /// Read more about creating and interning symbols
    /// [here](https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Symbols.html).
    pub fn intern(&mut self, name: &str) -> TulispObject {
        if let Some(sym) = self.obarray.get(name) {
            sym.clone_without_span()
        } else {
            let name = name.to_string();
            let constant = name.starts_with(':');
            let sym = TulispObject::symbol(name.clone(), constant);
            self.obarray.insert(name, sym.clone());
            sym
        }
    }

    pub(crate) fn intern_soft(&mut self, name: &str) -> Option<TulispObject> {
        self.obarray.get(name).map(|x| x.clone_without_span())
    }

    #[inline(always)]
    pub fn add_special_form(
        &mut self,
        name: &str,
        func: impl Fn(&mut TulispContext, &TulispObject) -> Result<TulispObject, Error> + 'static,
    ) {
        self.intern(name)
            .set_global(TulispValue::Func(Rc::new(func)).into_ref(None))
            .unwrap();
    }

    #[inline(always)]
    pub fn add_macro(
        &mut self,
        name: &str,
        func: impl Fn(&mut TulispContext, &TulispObject) -> Result<TulispObject, Error> + 'static,
    ) {
        self.intern(name)
            .set_global(TulispValue::Macro(Rc::new(func)).into_ref(None))
            .unwrap();
    }

    /// Evaluates the given value and returns the result.
    pub fn eval(&mut self, value: &TulispObject) -> Result<TulispObject, Error> {
        eval(self, value)
    }

    /// Evaluates the given value, run the given function on the result of the
    /// evaluation, and returns the result of the function.
    pub fn eval_and_then<T>(
        &mut self,
        expr: &TulispObject,
        f: impl FnOnce(&TulispObject) -> Result<T, Error>,
    ) -> Result<T, Error> {
        eval_and_then(self, expr, f)
    }

    /// Calls the given function with the given arguments, and returns the
    /// result.
    pub fn funcall(
        &mut self,
        func: &TulispObject,
        args: &TulispObject,
    ) -> Result<TulispObject, Error> {
        let func = self.eval(func)?;
        funcall::<DummyEval>(self, &func, args)
    }

    /// Maps the given function over the given sequence, and returns the result.
    pub fn map(&mut self, func: &TulispObject, seq: &TulispObject) -> Result<TulispObject, Error> {
        let func = self.eval(func)?;
        let ret = TulispObject::nil();
        for item in seq.base_iter() {
            ret.push(funcall::<DummyEval>(self, &func, &list!(item)?)?)?;
        }
        Ok(ret)
    }

    /// Filters the given sequence using the given function, and returns the
    /// result.
    pub fn filter(
        &mut self,
        func: &TulispObject,
        seq: &TulispObject,
    ) -> Result<TulispObject, Error> {
        let func = self.eval(func)?;
        let ret = TulispObject::nil();
        for item in seq.base_iter() {
            if funcall::<DummyEval>(self, &func, &list!(item.clone())?)?.is_truthy() {
                ret.push(item)?;
            }
        }
        Ok(ret)
    }

    /// Reduces the given sequence using the given function, and returns the
    /// result.
    pub fn reduce(
        &mut self,
        func: &TulispObject,
        seq: &TulispObject,
        initial_value: &TulispObject,
    ) -> Result<TulispObject, Error> {
        let func = self.eval(func)?;
        let mut ret = initial_value.clone();
        for item in seq.base_iter() {
            ret = funcall::<DummyEval>(self, &func, &list!(ret, item)?)?;
        }
        Ok(ret)
    }

    /// Parses and evaluates the given string, and returns the result.
    pub fn eval_string(&mut self, string: &str) -> Result<TulispObject, Error> {
        let vv = parse(self, 0, string)?;
        self.eval_progn(&vv)
    }

    /// Evaluates each item in the given sequence, and returns the value of the
    /// last one.
    pub fn eval_progn(&mut self, seq: &TulispObject) -> Result<TulispObject, Error> {
        let mut ret = None;
        let mut result = None;
        for val in seq.base_iter() {
            eval_basic(self, &val, &mut result)?;
            ret = Some(result.take().unwrap_or(val))
        }
        Ok(ret.unwrap_or_else(TulispObject::nil))
    }

    /// Evaluates each item in the given sequence, and returns the value of
    /// each.
    pub fn eval_each(&mut self, seq: &TulispObject) -> Result<TulispObject, Error> {
        let ret = TulispObject::nil();
        for val in seq.base_iter() {
            ret.push(eval(self, &val)?)?;
        }
        Ok(ret)
    }

    /// Parses and evaluates the contents of the given file and returns the
    /// value.
    pub fn eval_file(&mut self, filename: &str) -> Result<TulispObject, Error> {
        let vv = self.parse_file(filename)?;
        self.eval_progn(&vv)
    }

    pub fn vm_eval_string(&mut self, string: &str) -> Result<TulispObject, Error> {
        let vv = parse(self, 0, string)?;
        let bytecode = compile(self, &vv)?;
        let vm = self.vm.clone();
        let res = vm.borrow_mut().run(self, bytecode);
        drop(vm);
        res
    }

    pub fn vm_eval_file(&mut self, filename: &str) -> Result<TulispObject, Error> {
        let start = std::time::Instant::now();
        let vv = self.parse_file(filename)?;
        println!("Parsing took: {:?}", start.elapsed());
        let start = std::time::Instant::now();
        let bytecode = compile(self, &vv)?;
        println!("Compiling took: {:?}", start.elapsed());
        // println!("{}", bytecode);
        let start = std::time::Instant::now();
        let vm = self.vm.clone();
        let res = vm.borrow_mut().run(self, bytecode);
        drop(vm);
        println!("Running took: {:?}", start.elapsed());
        res
    }

    pub(crate) fn get_filename(&self, file_id: usize) -> String {
        self.filenames[file_id].clone()
    }

    pub(crate) fn parse_file(&mut self, filename: &str) -> Result<TulispObject, Error> {
        let contents = fs::read_to_string(filename).map_err(|e| {
            Error::new(
                crate::ErrorKind::Undefined,
                format!("Unable to read file: {filename}. Error: {e}"),
            )
        })?;
        let idx = if let Some(idx) = self.filenames.iter().position(|x| x == filename) {
            idx
        } else {
            self.filenames.push(filename.to_owned());
            self.filenames.len() - 1
        };

        let string: &str = &contents;
        parse(self, idx, string)
    }

    #[allow(dead_code)]
    pub(crate) fn compile_string(
        &mut self,
        string: &str,
        keep_result: bool,
    ) -> Result<crate::bytecode::Bytecode, Error> {
        let vv = parse(self, 0, string)?;
        let compiler = self.compiler.as_mut().unwrap();
        compiler.keep_result = keep_result;
        compile(self, &vv)
    }

    #[allow(dead_code)]
    pub(crate) fn run_bytecode(&mut self, bytecode: Bytecode) -> Result<TulispObject, Error> {
        let vm = self.vm.clone();
        let res = vm.borrow_mut().run(self, bytecode);
        drop(vm);
        res
    }
}
