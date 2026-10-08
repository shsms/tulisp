//! The docstrings of the built-in functions, macros and special forms. Each is
//! the first line of what it does, a blank line, and a usage line naming its
//! parameters, as Emacs writes them.

use crate::TulispContext;

pub(crate) const DOCS: &[(&str, &str)] = &[
    (
        "%",
        concat!("Return remainder of X divided by Y.", "\n\n", "(fn X Y)"),
    ),
    (
        "*",
        concat!(
            "Return product of any number of arguments, which are numbers.",
            "\n\n",
            "(fn &rest NUMBERS)"
        ),
    ),
    (
        "+",
        concat!(
            "Return sum of any number of arguments, which are numbers.",
            "\n\n",
            "(fn &rest NUMBERS)"
        ),
    ),
    (
        "-",
        concat!(
            "Negate number or subtract numbers and return the result.",
            "\n\n",
            "(fn &rest NUMBERS)"
        ),
    ),
    (
        "->",
        concat!(
            "Pass X through FORMS in turn, inserting each value as the first argument of the next form.",
            "\n\n",
            "(fn X &rest FORMS)"
        ),
    ),
    (
        "->>",
        concat!(
            "Pass X through FORMS in turn, inserting each value as the last argument of the next form.",
            "\n\n",
            "(fn X &rest FORMS)"
        ),
    ),
    (
        "/",
        concat!(
            "Divide number by divisors and return the result.",
            "\n\n",
            "(fn NUMBER &rest DIVISORS)"
        ),
    ),
    (
        "1+",
        concat!("Return NUMBER plus one.", "\n\n", "(fn NUMBER)"),
    ),
    (
        "1-",
        concat!("Return NUMBER minus one.", "\n\n", "(fn NUMBER)"),
    ),
    (
        "<",
        concat!(
            "Return t if each arg, a number, is less than the next arg.",
            "\n\n",
            "(fn &rest NUMBERS)"
        ),
    ),
    (
        "<=",
        concat!(
            "Return t if each arg, a number, is less than or equal to the next.",
            "\n\n",
            "(fn &rest NUMBERS)"
        ),
    ),
    (
        "=",
        concat!(
            "Return t if args, all numbers, are equal.",
            "\n\n",
            "(fn &rest NUMBERS)"
        ),
    ),
    (
        ">",
        concat!(
            "Return t if each arg, a number, is greater than the next arg.",
            "\n\n",
            "(fn &rest NUMBERS)"
        ),
    ),
    (
        ">=",
        concat!(
            "Return t if each arg, a number, is greater than or equal to the next.",
            "\n\n",
            "(fn &rest NUMBERS)"
        ),
    ),
    (
        "abs",
        concat!("Return the absolute value of ARG.", "\n\n", "(fn ARG)"),
    ),
    (
        "add-to-list",
        concat!(
            "Add ELEMENT to the value of LIST-VAR if it isn't there yet, and return the new value. ELEMENT goes at the start, or at the end with APPEND. Elements are compared with `equal', or with COMPARE-FN when given.",
            "\n\n",
            "(fn LIST-VAR ELEMENT &optional APPEND COMPARE-FN)"
        ),
    ),
    (
        "alist-get",
        concat!(
            "Find the first element of ALIST whose `car' equals KEY and return its `cdr'.",
            "\n\n",
            "(fn KEY ALIST &optional DEFAULT REMOVE TESTFN)"
        ),
    ),
    (
        "and",
        concat!(
            "Eval args until one of them yields nil, then return nil.",
            "\n\n",
            "(fn &rest CONDITIONS)"
        ),
    ),
    (
        "append",
        concat!(
            "Concatenate all the arguments and make the result a list.",
            "\n\n",
            "(fn &rest SEQUENCES)"
        ),
    ),
    (
        "apply",
        concat!(
            "Call the first argument as a function with the rest, using the last argument as a list of arguments.",
            "\n\n",
            "(fn &rest ARGUMENTS)"
        ),
    ),
    (
        "aset",
        concat!(
            "Store the character NEWELT into STRING at index IDX, and return NEWELT.",
            "\n\n",
            "(fn STRING IDX NEWELT)"
        ),
    ),
    (
        "assoc",
        concat!(
            "Return non-nil if KEY is equal to the car of an element of ALIST.",
            "\n\n",
            "(fn KEY ALIST &optional TESTFN)"
        ),
    ),
    (
        "assq",
        concat!(
            "Return non-nil if KEY is `eq' to the car of an element of ALIST. The value is actually the first element of ALIST whose car is KEY.",
            "\n\n",
            "(fn KEY ALIST)"
        ),
    ),
    (
        "atom",
        concat!(
            "Return t if OBJECT is not a cons cell, nil included.",
            "\n\n",
            "(fn OBJECT)"
        ),
    ),
    (
        "boundp",
        concat!(
            "Return t if SYMBOL's value is not void.",
            "\n\n",
            "(fn SYMBOL)"
        ),
    ),
    (
        "caaaar",
        concat!(
            "Return the `car' of the `car' of the `car' of the `car' of X.",
            "\n\n",
            "(fn X)"
        ),
    ),
    (
        "caaadr",
        concat!(
            "Return the `car' of the `car' of the `car' of the `cdr' of X.",
            "\n\n",
            "(fn X)"
        ),
    ),
    (
        "caaar",
        concat!(
            "Return the `car' of the `car' of the `car' of X.",
            "\n\n",
            "(fn X)"
        ),
    ),
    (
        "caadar",
        concat!(
            "Return the `car' of the `car' of the `cdr' of the `car' of X.",
            "\n\n",
            "(fn X)"
        ),
    ),
    (
        "caaddr",
        concat!(
            "Return the `car' of the `car' of the `cdr' of the `cdr' of X.",
            "\n\n",
            "(fn X)"
        ),
    ),
    (
        "caadr",
        concat!(
            "Return the `car' of the `car' of the `cdr' of X.",
            "\n\n",
            "(fn X)"
        ),
    ),
    (
        "caar",
        concat!("Return the car of the car of X.", "\n\n", "(fn X)"),
    ),
    (
        "cadaar",
        concat!(
            "Return the `car' of the `cdr' of the `car' of the `car' of X.",
            "\n\n",
            "(fn X)"
        ),
    ),
    (
        "cadadr",
        concat!(
            "Return the `car' of the `cdr' of the `car' of the `cdr' of X.",
            "\n\n",
            "(fn X)"
        ),
    ),
    (
        "cadar",
        concat!(
            "Return the `car' of the `cdr' of the `car' of X.",
            "\n\n",
            "(fn X)"
        ),
    ),
    (
        "caddar",
        concat!(
            "Return the `car' of the `cdr' of the `cdr' of the `car' of X.",
            "\n\n",
            "(fn X)"
        ),
    ),
    (
        "cadddr",
        concat!(
            "Return the `car' of the `cdr' of the `cdr' of the `cdr' of X.",
            "\n\n",
            "(fn X)"
        ),
    ),
    (
        "caddr",
        concat!(
            "Return the `car' of the `cdr' of the `cdr' of X.",
            "\n\n",
            "(fn X)"
        ),
    ),
    (
        "cadr",
        concat!("Return the car of the cdr of X.", "\n\n", "(fn X)"),
    ),
    (
        "capitalize",
        concat!(
            "Convert OBJ, a string or a character, to capitalized form: the first letter of each word in title case and the rest in lower case. A word is a run of letters and digits.",
            "\n\n",
            "(fn OBJ)"
        ),
    ),
    (
        "car",
        concat!(
            "Return the car of LIST, or nil if LIST is nil.",
            "\n\n",
            "(fn LIST)"
        ),
    ),
    (
        "car-safe",
        concat!(
            "Return the car of OBJECT if it is a cons cell, or else nil.",
            "\n\n",
            "(fn OBJECT)"
        ),
    ),
    (
        "catch",
        concat!(
            "Eval BODY allowing nonlocal exits using `throw'.",
            "\n\n",
            "(fn TAG &rest BODY)"
        ),
    ),
    (
        "cdaaar",
        concat!(
            "Return the `cdr' of the `car' of the `car' of the `car' of X.",
            "\n\n",
            "(fn X)"
        ),
    ),
    (
        "cdaadr",
        concat!(
            "Return the `cdr' of the `car' of the `car' of the `cdr' of X.",
            "\n\n",
            "(fn X)"
        ),
    ),
    (
        "cdaar",
        concat!(
            "Return the `cdr' of the `car' of the `car' of X.",
            "\n\n",
            "(fn X)"
        ),
    ),
    (
        "cdadar",
        concat!(
            "Return the `cdr' of the `car' of the `cdr' of the `car' of X.",
            "\n\n",
            "(fn X)"
        ),
    ),
    (
        "cdaddr",
        concat!(
            "Return the `cdr' of the `car' of the `cdr' of the `cdr' of X.",
            "\n\n",
            "(fn X)"
        ),
    ),
    (
        "cdadr",
        concat!(
            "Return the `cdr' of the `car' of the `cdr' of X.",
            "\n\n",
            "(fn X)"
        ),
    ),
    (
        "cdar",
        concat!("Return the cdr of the car of X.", "\n\n", "(fn X)"),
    ),
    (
        "cddaar",
        concat!(
            "Return the `cdr' of the `cdr' of the `car' of the `car' of X.",
            "\n\n",
            "(fn X)"
        ),
    ),
    (
        "cddadr",
        concat!(
            "Return the `cdr' of the `cdr' of the `car' of the `cdr' of X.",
            "\n\n",
            "(fn X)"
        ),
    ),
    (
        "cddar",
        concat!(
            "Return the `cdr' of the `cdr' of the `car' of X.",
            "\n\n",
            "(fn X)"
        ),
    ),
    (
        "cdddar",
        concat!(
            "Return the `cdr' of the `cdr' of the `cdr' of the `car' of X.",
            "\n\n",
            "(fn X)"
        ),
    ),
    (
        "cddddr",
        concat!(
            "Return the `cdr' of the `cdr' of the `cdr' of the `cdr' of X.",
            "\n\n",
            "(fn X)"
        ),
    ),
    (
        "cdddr",
        concat!(
            "Return the `cdr' of the `cdr' of the `cdr' of X.",
            "\n\n",
            "(fn X)"
        ),
    ),
    (
        "cddr",
        concat!("Return the cdr of the cdr of X.", "\n\n", "(fn X)"),
    ),
    (
        "cdr",
        concat!(
            "Return the cdr of LIST, or nil if LIST is nil.",
            "\n\n",
            "(fn LIST)"
        ),
    ),
    (
        "cdr-safe",
        concat!(
            "Return the cdr of OBJECT if it is a cons cell, or else nil.",
            "\n\n",
            "(fn OBJECT)"
        ),
    ),
    (
        "ceiling",
        concat!(
            "Return the smallest integer no less than ARG.",
            "\n\n",
            "(fn ARG &optional DIVISOR)"
        ),
    ),
    (
        "char-to-string",
        concat!(
            "Convert CHAR to a string containing that character.",
            "\n\n",
            "(fn CHAR)"
        ),
    ),
    (
        "concat",
        concat!(
            "Concatenate all the arguments and make the result a string.",
            "\n\n",
            "(fn &rest SEQUENCES)"
        ),
    ),
    (
        "cond",
        concat!(
            "Try each clause until one succeeds.",
            "\n\n",
            "(fn &rest CLAUSES)"
        ),
    ),
    (
        "condition-case",
        concat!(
            "Regain control when an error is signaled.",
            "\n\n",
            "(fn VAR BODYFORM &rest HANDLERS)"
        ),
    ),
    (
        "cons",
        concat!(
            "Create a new cons, give it CAR and CDR as components, and return it.",
            "\n\n",
            "(fn CAR CDR)"
        ),
    ),
    (
        "consp",
        concat!("Return t if OBJECT is a cons cell.", "\n\n", "(fn OBJECT)"),
    ),
    (
        "current-time",
        concat!(
            "Return the current time, as a (TICKS . HZ) pair counting nanoseconds since 1970-01-01 00:00:00 UTC.",
            "\n\n",
            "(fn)"
        ),
    ),
    (
        "declare",
        concat!(
            "Do not evaluate any arguments, and return nil.",
            "\n\n",
            "(fn &rest SPECS)"
        ),
    ),
    (
        "defconst",
        concat!(
            "Define SYMBOL as a constant variable: set it to the value of INITVALUE, even when it already has a value, and declare it special. DOCSTRING documents it.",
            "\n\n",
            "(fn SYMBOL INITVALUE &optional DOCSTRING)"
        ),
    ),
    (
        "define-error",
        concat!(
            "Define NAME as a new error signal.",
            "\n\n",
            "(fn NAME MESSAGE &optional PARENT)"
        ),
    ),
    (
        "defmacro",
        concat!(
            "Define NAME as a macro.",
            "\n\n",
            "(fn NAME ARGLIST &optional DOCSTRING DECL &rest BODY)"
        ),
    ),
    (
        "defun",
        concat!(
            "Define NAME as a function.",
            "\n\n",
            "(fn NAME ARGLIST &optional DOCSTRING DECL INTERACTIVE &rest BODY)"
        ),
    ),
    (
        "defvar",
        concat!(
            "Define SYMBOL as a variable, and return SYMBOL.",
            "\n\n",
            "(fn SYMBOL &optional INITVALUE DOCSTRING)"
        ),
    ),
    (
        "delete",
        concat!(
            "Delete members of SEQ which are `equal' to ELT, and return the result. A list is changed in place, so use the result: (setq foo (delete element foo)). A string is not changed; the result is a new string.",
            "\n\n",
            "(fn ELT SEQ)"
        ),
    ),
    (
        "delete-dups",
        concat!(
            "Remove the `equal' duplicates from LIST, keeping the first of each, and changing LIST. Return LIST.",
            "\n\n",
            "(fn LIST)"
        ),
    ),
    (
        "delq",
        concat!(
            "Delete members of LIST which are `eq' to ELT, and return the result. LIST is changed in place, so use the result: (setq foo (delq element foo)).",
            "\n\n",
            "(fn ELT LIST)"
        ),
    ),
    (
        "dolist",
        concat!(
            "Evaluate BODY with VAR bound to each element of LIST in turn, then return RESULT, where SPEC is (VAR LIST) or (VAR LIST RESULT).",
            "\n\n",
            "(fn SPEC &rest BODY)"
        ),
    ),
    (
        "dotimes",
        concat!(
            "Evaluate BODY with VAR bound to each integer from 0 up to but not including COUNT, then return RESULT, where SPEC is (VAR COUNT) or (VAR COUNT RESULT).",
            "\n\n",
            "(fn SPEC &rest BODY)"
        ),
    ),
    (
        "downcase",
        concat!(
            "Convert OBJ, a string or a character, to lower case and return that.",
            "\n\n",
            "(fn OBJ)"
        ),
    ),
    (
        "eq",
        concat!(
            "Return t if the two args are the same Lisp object.",
            "\n\n",
            "(fn OBJ1 OBJ2)"
        ),
    ),
    (
        "eql",
        concat!(
            "Return t if the two args are `eq' or are indistinguishable numbers.",
            "\n\n",
            "(fn OBJ1 OBJ2)"
        ),
    ),
    (
        "equal",
        concat!(
            "Return t if two Lisp objects have similar structure and contents.",
            "\n\n",
            "(fn O1 O2)"
        ),
    ),
    (
        "error",
        concat!(
            "Signal an error, making a message by passing FORMAT and ARGS to `format'.",
            "\n\n",
            "(fn FORMAT &rest ARGS)"
        ),
    ),
    (
        "error-message-string",
        concat!(
            "Convert an error value (ERROR-SYMBOL . DATA) to an error message.",
            "\n\n",
            "(fn OBJ)"
        ),
    ),
    (
        "eval",
        concat!(
            "Evaluate FORM and return its value.",
            "\n\n",
            "(fn FORM &optional LEXICAL)"
        ),
    ),
    (
        "expt",
        concat!(
            "Return the exponential ARG1 ** ARG2.",
            "\n\n",
            "(fn ARG1 ARG2)"
        ),
    ),
    (
        "fboundp",
        concat!(
            "Return t if SYMBOL's function definition is not void. In Tulisp a function and a variable share one value, so a variable holding a function counts too.",
            "\n\n",
            "(fn SYMBOL)"
        ),
    ),
    (
        "fceiling",
        concat!(
            "Return the smallest integer no less than ARG, as a float.",
            "\n\n",
            "(fn ARG)"
        ),
    ),
    (
        "ffloor",
        concat!(
            "Return the largest integer no greater than ARG, as a float.",
            "\n\n",
            "(fn ARG)"
        ),
    ),
    (
        "floatp",
        concat!(
            "Return t if OBJECT is a floating point number.",
            "\n\n",
            "(fn OBJECT)"
        ),
    ),
    (
        "floor",
        concat!(
            "Return the largest integer no greater than ARG.",
            "\n\n",
            "(fn ARG &optional DIVISOR)"
        ),
    ),
    (
        "format",
        concat!(
            "Format a string out of a format-string and arguments.",
            "\n\n",
            "(fn STRING &rest OBJECTS)"
        ),
    ),
    (
        "format-seconds",
        concat!(
            "Use format control STRING to format the number SECONDS.",
            "\n\n",
            "(fn STRING SECONDS)"
        ),
    ),
    (
        "fround",
        concat!(
            "Return the nearest integer to ARG, as a float.",
            "\n\n",
            "(fn ARG)"
        ),
    ),
    (
        "ftruncate",
        concat!(
            "Truncate a floating point number to an integral float value.",
            "\n\n",
            "(fn ARG)"
        ),
    ),
    (
        "funcall",
        concat!(
            "Call first argument as a function, passing remaining arguments to it.",
            "\n\n",
            "(fn FUNCTION &rest ARGUMENTS)"
        ),
    ),
    (
        "function",
        concat!(
            "Like `quote', but preferred for objects which are functions.",
            "\n\n",
            "(fn ARG)"
        ),
    ),
    (
        "functionp",
        concat!("Return t if OBJECT is a function.", "\n\n", "(fn OBJECT)"),
    ),
    (
        "gensym",
        concat!(
            "Return a new uninterned symbol.",
            "\n\n",
            "(fn &optional PREFIX)"
        ),
    ),
    (
        "gethash",
        concat!(
            "Look up KEY in TABLE and return its associated value.",
            "\n\n",
            "(fn KEY TABLE &optional DFLT)"
        ),
    ),
    (
        "hash-table-count",
        concat!(
            "Return the number of entries in hash table TABLE.",
            "\n\n",
            "(fn TABLE)"
        ),
    ),
    (
        "identity",
        concat!("Return the ARGUMENT unchanged.", "\n\n", "(fn ARGUMENT)"),
    ),
    (
        "if",
        concat!(
            "If COND yields non-nil, do THEN, else do the ELSE forms.",
            "\n\n",
            "(fn COND THEN &rest ELSE)"
        ),
    ),
    (
        "if-let",
        concat!(
            "Bind variables according to SPEC and evaluate THEN or ELSE.",
            "\n\n",
            "(fn SPEC THEN &rest ELSE)"
        ),
    ),
    (
        "if-let*",
        concat!(
            "Bind variables according to VARLIST and evaluate THEN or ELSE.",
            "\n\n",
            "(fn VARLIST THEN &rest ELSE)"
        ),
    ),
    (
        "ignore",
        concat!(
            "Ignore ARGUMENTS, do nothing, and return nil.",
            "\n\n",
            "(fn &rest ARGUMENTS)"
        ),
    ),
    (
        "ignore-errors",
        concat!(
            "Execute BODY; if an error occurs, return nil. Otherwise, return the value of the last form in BODY.",
            "\n\n",
            "(fn &rest BODY)"
        ),
    ),
    (
        "integerp",
        concat!("Return t if OBJECT is an integer.", "\n\n", "(fn OBJECT)"),
    ),
    (
        "interactive",
        concat!(
            "Ignore an interactive specification in a function body: do not evaluate any arguments, and return nil.",
            "\n\n",
            "(fn &rest ARGS)"
        ),
    ),
    (
        "intern",
        concat!(
            "Return the canonical symbol whose name is STRING.",
            "\n\n",
            "(fn STRING)"
        ),
    ),
    (
        "isnan",
        concat!("Return non-nil if argument X is a NaN.", "\n\n", "(fn X)"),
    ),
    (
        "keywordp",
        concat!("Return t if OBJECT is a keyword.", "\n\n", "(fn OBJECT)"),
    ),
    (
        "lambda",
        concat!(
            "Return an anonymous function.",
            "\n\n",
            "(fn ARGS &optional DOCSTRING INTERACTIVE &rest BODY)"
        ),
    ),
    (
        "last",
        concat!(
            "Return the last link of LIST, whose car is the last element.",
            "\n\n",
            "(fn LIST &optional N)"
        ),
    ),
    (
        "length",
        concat!(
            "Return the length of list or string SEQUENCE.",
            "\n\n",
            "(fn SEQUENCE)"
        ),
    ),
    (
        "let",
        concat!(
            "Bind variables according to VARLIST then eval BODY.",
            "\n\n",
            "(fn VARLIST &rest BODY)"
        ),
    ),
    (
        "let*",
        concat!(
            "Bind variables according to VARLIST then eval BODY.",
            "\n\n",
            "(fn VARLIST &rest BODY)"
        ),
    ),
    (
        "list",
        concat!(
            "Return a newly created list with specified arguments as elements.",
            "\n\n",
            "(fn &rest OBJECTS)"
        ),
    ),
    (
        "listp",
        concat!(
            "Return t if OBJECT is a list, that is, a cons cell or nil.",
            "\n\n",
            "(fn OBJECT)"
        ),
    ),
    (
        "load",
        concat!(
            "Execute a file of Lisp code named FILE.",
            "\n\n",
            "(fn FILE &optional NOERROR NOMESSAGE NOSUFFIX MUST-SUFFIX)"
        ),
    ),
    (
        "macroexpand",
        concat!(
            "Return result of expanding macros at top level of FORM.",
            "\n\n",
            "(fn FORM &optional ENVIRONMENT)"
        ),
    ),
    (
        "macroexpand-1",
        concat!(
            "Perform (at most) one step of macroexpansion.",
            "\n\n",
            "(fn FORM &optional ENVIRONMENT)"
        ),
    ),
    (
        "macroexpand-all",
        concat!(
            "Return result of expanding macros at all levels in FORM.",
            "\n\n",
            "(fn FORM &optional ENVIRONMENT)"
        ),
    ),
    (
        "make-hash-table",
        concat!(
            "Create and return a new hash table.",
            "\n\n",
            "(fn &key TEST SIZE)"
        ),
    ),
    (
        "make-string",
        concat!(
            "Return a newly created string of length LENGTH, with INIT in each element.",
            "\n\n",
            "(fn LENGTH INIT)"
        ),
    ),
    (
        "make-symbol",
        concat!(
            "Return a newly allocated uninterned symbol whose name is NAME.",
            "\n\n",
            "(fn NAME)"
        ),
    ),
    (
        "mapc",
        concat!(
            "Apply FUNCTION to each element of SEQUENCE for side effects only, and return SEQUENCE. A string's elements are its characters.",
            "\n\n",
            "(fn FUNCTION SEQUENCE)"
        ),
    ),
    (
        "mapcar",
        concat!(
            "Apply FUNCTION to each element of LIST, and make a list of the results.",
            "\n\n",
            "(fn FUNCTION LIST)"
        ),
    ),
    (
        "mapconcat",
        concat!(
            "Apply FUNCTION to each element of LIST, and concat the results as strings.",
            "\n\n",
            "(fn FUNCTION LIST &optional SEPARATOR)"
        ),
    ),
    (
        "maphash",
        concat!(
            "Call FUNCTION for each entry in hash table TABLE, with the entry's key and value. FUNCTION may set or remove entries; an entry removed before the walk reaches it is not visited. Return nil.",
            "\n\n",
            "(fn FUNCTION TABLE)"
        ),
    ),
    (
        "max",
        concat!(
            "Return largest of all the arguments, which must be numbers.",
            "\n\n",
            "(fn NUMBER &rest NUMBERS)"
        ),
    ),
    (
        "member",
        concat!(
            "Return non-nil if ELT is an element of LIST, comparing with `equal'.",
            "\n\n",
            "(fn ELT LIST)"
        ),
    ),
    (
        "memq",
        concat!(
            "Return non-nil if ELT is an element of LIST, comparing with `eq'.",
            "\n\n",
            "(fn ELT LIST)"
        ),
    ),
    (
        "memql",
        concat!(
            "Return non-nil if ELT is an element of LIST, comparing with `eql'.",
            "\n\n",
            "(fn ELT LIST)"
        ),
    ),
    (
        "min",
        concat!(
            "Return smallest of all the arguments, which must be numbers.",
            "\n\n",
            "(fn NUMBER &rest NUMBERS)"
        ),
    ),
    ("mod", concat!("Return X modulo Y.", "\n\n", "(fn X Y)")),
    (
        "nconc",
        concat!(
            "Concatenate any number of lists by changing them: the last cell of each list is set to point to the next. Only the last argument is not changed, and it need not be a list.",
            "\n\n",
            "(fn &rest LISTS)"
        ),
    ),
    (
        "not",
        concat!(
            "Return t if OBJECT is nil, and return nil otherwise.",
            "\n\n",
            "(fn OBJECT)"
        ),
    ),
    (
        "nreverse",
        concat!(
            "Reverse the order of the elements of SEQ, and return the result. A list is changed in place, so use the result; a string is not changed, and the result is a new string.",
            "\n\n",
            "(fn SEQ)"
        ),
    ),
    (
        "nth",
        concat!("Return the Nth element of LIST.", "\n\n", "(fn N LIST)"),
    ),
    (
        "nthcdr",
        concat!(
            "Take cdr N times on LIST, return the result.",
            "\n\n",
            "(fn N LIST)"
        ),
    ),
    (
        "null",
        concat!(
            "Return t if OBJECT is nil, and return nil otherwise.",
            "\n\n",
            "(fn OBJECT)"
        ),
    ),
    (
        "number-to-string",
        concat!(
            "Return the decimal representation of NUMBER as a string, as `prin1' prints it.",
            "\n\n",
            "(fn NUMBER)"
        ),
    ),
    (
        "numberp",
        concat!(
            "Return t if OBJECT is a number (floating point or integer).",
            "\n\n",
            "(fn OBJECT)"
        ),
    ),
    (
        "or",
        concat!(
            "Eval args until one of them yields non-nil, then return that value.",
            "\n\n",
            "(fn &rest CONDITIONS)"
        ),
    ),
    (
        "plist-get",
        concat!(
            "Extract a value from a property list.",
            "\n\n",
            "(fn PLIST PROP &optional PREDICATE)"
        ),
    ),
    (
        "plist-put",
        concat!(
            "Change the value in PLIST of PROP to VAL. If PROP is already a property on the list, its value is set to VAL; otherwise PROP and VAL are added at the end, changing PLIST. Return the new plist; use it, as PLIST may have been nil. Properties are compared with `eq', or with PREDICATE when given.",
            "\n\n",
            "(fn PLIST PROP VAL &optional PREDICATE)"
        ),
    ),
    (
        "pop",
        concat!(
            "Return the first element of PLACE's value, and remove it from the list. PLACE must be a variable.",
            "\n\n",
            "(fn PLACE)"
        ),
    ),
    (
        "prin1-to-string",
        concat!(
            "Return a string containing the printed representation of OBJECT.",
            "\n\n",
            "(fn OBJECT &optional NOESCAPE)"
        ),
    ),
    (
        "princ",
        concat!(
            "Output the printed representation of OBJECT, any Lisp object, to standard output.",
            "\n\n",
            "(fn OBJECT)"
        ),
    ),
    (
        "print",
        concat!(
            "Output OBJECT as `princ' does, followed by a newline, and return OBJECT.",
            "\n\n",
            "(fn OBJECT)"
        ),
    ),
    (
        "prog1",
        concat!(
            "Eval FIRST and BODY sequentially; return value from FIRST.",
            "\n\n",
            "(fn FIRST &rest BODY)"
        ),
    ),
    (
        "prog2",
        concat!(
            "Eval FORM1, FORM2 and BODY sequentially; return value from FORM2.",
            "\n\n",
            "(fn FORM1 FORM2 &rest BODY)"
        ),
    ),
    (
        "progn",
        concat!(
            "Eval BODY forms sequentially and return value of last one.",
            "\n\n",
            "(fn &rest BODY)"
        ),
    ),
    (
        "push",
        concat!(
            "Add NEWELT to the front of the list stored in the variable PLACE.",
            "\n\n",
            "(fn NEWELT PLACE)"
        ),
    ),
    (
        "puthash",
        concat!(
            "Associate KEY with VALUE in hash table TABLE.",
            "\n\n",
            "(fn KEY VALUE TABLE)"
        ),
    ),
    (
        "quote",
        concat!(
            "Return the argument, without evaluating it.",
            "\n\n",
            "(fn ARG)"
        ),
    ),
    (
        "remhash",
        concat!(
            "Remove the entry for KEY from hash table TABLE, if there is one. Return nil.",
            "\n\n",
            "(fn KEY TABLE)"
        ),
    ),
    (
        "remove",
        concat!(
            "Return a copy of SEQ with all occurrences of ELT removed, compared with `equal'. SEQ is not changed.",
            "\n\n",
            "(fn ELT SEQ)"
        ),
    ),
    (
        "reverse",
        concat!("Return a reversed copy of LIST.", "\n\n", "(fn LIST)"),
    ),
    (
        "round",
        concat!(
            "Return the nearest integer to ARG.",
            "\n\n",
            "(fn ARG &optional DIVISOR)"
        ),
    ),
    (
        "seq-drop",
        concat!(
            "Return LIST without its first N elements, sharing structure with LIST.",
            "\n\n",
            "(fn LIST N)"
        ),
    ),
    (
        "seq-filter",
        concat!(
            "Return a list of all the elements in LIST for which PRED returns non-nil.",
            "\n\n",
            "(fn PRED LIST)"
        ),
    ),
    (
        "seq-find",
        concat!(
            "Return the first element in LIST for which PRED returns non-nil.",
            "\n\n",
            "(fn PRED LIST &optional DEFAULT)"
        ),
    ),
    (
        "seq-map",
        concat!(
            "Return a list of the results of applying FUNCTION to each element of LIST.",
            "\n\n",
            "(fn FUNCTION LIST)"
        ),
    ),
    (
        "seq-reduce",
        concat!(
            "Reduce the function FUNCTION across LIST, starting with INITIAL-VALUE.",
            "\n\n",
            "(fn FUNCTION LIST INITIAL-VALUE)"
        ),
    ),
    (
        "seq-take",
        concat!(
            "Return a new list of the first N elements of LIST.",
            "\n\n",
            "(fn LIST N)"
        ),
    ),
    (
        "set",
        concat!(
            "Set SYMBOL's value to NEWVAL, and return NEWVAL.",
            "\n\n",
            "(fn SYMBOL NEWVAL)"
        ),
    ),
    (
        "setcar",
        concat!(
            "Set the car of CELL to be NEWCAR, and return NEWCAR.",
            "\n\n",
            "(fn CELL NEWCAR)"
        ),
    ),
    (
        "setcdr",
        concat!(
            "Set the cdr of CELL to be NEWCDR, and return NEWCDR.",
            "\n\n",
            "(fn CELL NEWCDR)"
        ),
    ),
    (
        "setq",
        concat!(
            "Set each SYM to the value of its VAL, taking the arguments as SYM VAL pairs.",
            "\n\n",
            "(fn &rest SYM-VAL-PAIRS)"
        ),
    ),
    (
        "signal",
        concat!(
            "Signal an error with ERROR-SYMBOL and its associated DATA.",
            "\n\n",
            "(fn ERROR-SYMBOL DATA)"
        ),
    ),
    (
        "sort",
        concat!(
            "Sort LIST, stably, and return the sorted list.",
            "\n\n",
            "(fn LIST &key KEY LESSP REVERSE IN-PLACE)"
        ),
    ),
    (
        "sqrt",
        concat!("Return the square root of ARG.", "\n\n", "(fn ARG)"),
    ),
    (
        "string",
        concat!(
            "Concatenate all the argument characters and make the result a string.",
            "\n\n",
            "(fn &rest CHARACTERS)"
        ),
    ),
    (
        "string-empty-p",
        concat!(
            "Check whether STRING is empty. A symbol stands for its name.",
            "\n\n",
            "(fn STRING)"
        ),
    ),
    (
        "string-equal",
        concat!(
            "Return t if two strings have identical contents.",
            "\n\n",
            "(fn S1 S2)"
        ),
    ),
    (
        "string-greaterp",
        concat!(
            "Return non-nil if STRING1 is greater than STRING2 in lexicographic order.",
            "\n\n",
            "(fn STRING1 STRING2)"
        ),
    ),
    (
        "string-join",
        concat!(
            "Join all STRINGS using SEPARATOR.",
            "\n\n",
            "(fn STRINGS &optional SEPARATOR)"
        ),
    ),
    (
        "string-lessp",
        concat!(
            "Return non-nil if STRING1 is less than STRING2 in lexicographic order.",
            "\n\n",
            "(fn STRING1 STRING2)"
        ),
    ),
    (
        "string-prefix-p",
        concat!(
            "Return non-nil if PREFIX is a prefix of STRING. With IGNORE-CASE, a letter matches its other case.",
            "\n\n",
            "(fn PREFIX STRING &optional IGNORE-CASE)"
        ),
    ),
    (
        "string-replace",
        concat!(
            "Replace FROM-STRING with TO-STRING in IN-STRING, every time it appears.",
            "\n\n",
            "(fn FROM-STRING TO-STRING IN-STRING)"
        ),
    ),
    (
        "string-search",
        concat!(
            "Search for the string NEEDLE in the string HAYSTACK, and return where the first match starts, in characters, or nil. The search starts at START-POS, a character position.",
            "\n\n",
            "(fn NEEDLE HAYSTACK &optional START-POS)"
        ),
    ),
    (
        "string-suffix-p",
        concat!(
            "Return non-nil if SUFFIX is a suffix of STRING. With IGNORE-CASE, a letter matches its other case.",
            "\n\n",
            "(fn SUFFIX STRING &optional IGNORE-CASE)"
        ),
    ),
    (
        "string-to-char",
        concat!(
            "Return the first character in STRING, or 0 for an empty string.",
            "\n\n",
            "(fn STRING)"
        ),
    ),
    (
        "string-to-number",
        concat!(
            "Read the number at the start of STRING, after spaces and tabs, and return it; 0 when there is none. With BASE, from 2 to 16, read an integer in that base.",
            "\n\n",
            "(fn STRING &optional BASE)"
        ),
    ),
    (
        "string<",
        concat!(
            "Return non-nil if STRING1 is less than STRING2 in lexicographic order.",
            "\n\n",
            "(fn STRING1 STRING2)"
        ),
    ),
    (
        "string=",
        concat!(
            "Return t if two strings have identical contents.",
            "\n\n",
            "(fn S1 S2)"
        ),
    ),
    (
        "string>",
        concat!(
            "Return non-nil if STRING1 is greater than STRING2 in lexicographic order.",
            "\n\n",
            "(fn STRING1 STRING2)"
        ),
    ),
    (
        "stringp",
        concat!("Return t if OBJECT is a string.", "\n\n", "(fn OBJECT)"),
    ),
    (
        "substring",
        concat!(
            "Return a new string holding the part of STRING from character FROM up to character TO. A negative index counts from the end; FROM defaults to the start and TO to the end.",
            "\n\n",
            "(fn STRING &optional FROM TO)"
        ),
    ),
    (
        "symbol-name",
        concat!("Return SYMBOL's name, a string.", "\n\n", "(fn SYMBOL)"),
    ),
    (
        "symbol-value",
        concat!(
            "Return SYMBOL's value, or signal an error if it is void.",
            "\n\n",
            "(fn SYMBOL)"
        ),
    ),
    (
        "symbolp",
        concat!("Return t if OBJECT is a symbol.", "\n\n", "(fn OBJECT)"),
    ),
    (
        "thread-first",
        concat!(
            "Pass X through FORMS in turn, inserting each value as the first argument of the next form.",
            "\n\n",
            "(fn X &rest FORMS)"
        ),
    ),
    (
        "thread-last",
        concat!(
            "Pass X through FORMS in turn, inserting each value as the last argument of the next form.",
            "\n\n",
            "(fn X &rest FORMS)"
        ),
    ),
    (
        "throw",
        concat!(
            "Throw to the catch for TAG and return VALUE from it.",
            "\n\n",
            "(fn TAG VALUE)"
        ),
    ),
    (
        "time-add",
        concat!(
            "Return the sum of two time values A and B, as a time value.",
            "\n\n",
            "(fn A B)"
        ),
    ),
    (
        "time-equal-p",
        concat!(
            "Return non-nil if A and B are equal time values.",
            "\n\n",
            "(fn A B)"
        ),
    ),
    (
        "time-less-p",
        concat!(
            "Return non-nil if time value A is less than time value B.",
            "\n\n",
            "(fn A B)"
        ),
    ),
    (
        "time-subtract",
        concat!(
            "Return the difference between two time values A and B, as a time value.",
            "\n\n",
            "(fn A B)"
        ),
    ),
    (
        "truncate",
        concat!(
            "Truncate a floating point number to an int.",
            "\n\n",
            "(fn ARG &optional DIVISOR)"
        ),
    ),
    (
        "unless",
        concat!(
            "If COND yields nil, do BODY, else return nil.",
            "\n\n",
            "(fn COND &rest BODY)"
        ),
    ),
    (
        "unwind-protect",
        concat!(
            "Do BODYFORM, protecting with UNWINDFORMS.",
            "\n\n",
            "(fn BODYFORM &rest UNWINDFORMS)"
        ),
    ),
    (
        "upcase",
        concat!(
            "Convert OBJ, a string or a character, to upper case and return that.",
            "\n\n",
            "(fn OBJ)"
        ),
    ),
    (
        "user-error",
        concat!(
            "Signal a user error, making a message by passing FORMAT and ARGS to `format'.",
            "\n\n",
            "(fn FORMAT &rest ARGS)"
        ),
    ),
    (
        "value<",
        concat!(
            "Return non-nil if A precedes B in standard value order.",
            "\n\n",
            "(fn A B)"
        ),
    ),
    (
        "when",
        concat!(
            "If COND yields non-nil, do BODY, else return nil.",
            "\n\n",
            "(fn COND &rest BODY)"
        ),
    ),
    (
        "when-let",
        concat!(
            "Bind variables according to SPEC and conditionally evaluate BODY.",
            "\n\n",
            "(fn SPEC &rest BODY)"
        ),
    ),
    (
        "while",
        concat!(
            "If TEST yields non-nil, eval BODY and repeat.",
            "\n\n",
            "(fn TEST &rest BODY)"
        ),
    ),
    (
        "while-let",
        concat!(
            "Bind variables according to SPEC and evaluate BODY, repeating while every binding is non-nil.",
            "\n\n",
            "(fn SPEC &rest BODY)"
        ),
    ),
    (
        "xor",
        concat!(
            "Return the boolean exclusive-or of COND1 and COND2.",
            "\n\n",
            "(fn COND1 COND2)"
        ),
    ),
    (
        "zerop",
        concat!("Return t if NUMBER is zero.", "\n\n", "(fn NUMBER)"),
    ),
];

/// Attaches the built-ins' docstrings, without copying them.
pub(crate) fn apply(ctx: &mut TulispContext) {
    for &(name, doc) in DOCS {
        ctx.set_builtin_doc(name, doc);
    }
}

#[cfg(test)]
mod tests {
    use super::DOCS;
    use crate::TulispContext;
    use crate::symbols::{ParamPosition, Signature, SymbolKind, split_usage};

    #[test]
    fn docs_are_sorted_and_unique() {
        for pair in DOCS.windows(2) {
            let (a, b) = (pair[0].0, pair[1].0);
            assert!(a < b, "{a} is not before {b}");
        }
    }

    #[test]
    fn every_built_in_has_a_docstring() {
        let ctx = TulispContext::new();
        let mut missing: Vec<&str> = ctx
            .symbols()
            .filter(|(_, info)| info.kind != SymbolKind::Variable && info.doc.is_none())
            .map(|(name, _)| name)
            .collect();
        missing.sort_unstable();
        assert!(
            missing.is_empty(),
            "built-ins with no docstring:\n{}",
            missing.join("\n")
        );
    }

    /// How many required and optional parameters SIGNATURE has, and whether it
    /// takes the rest.
    fn counts(signature: &Signature) -> (usize, usize, bool) {
        let count = |position| {
            signature
                .params
                .iter()
                .filter(|param| param.position == position)
                .count()
        };
        let rest = signature.params.iter().any(|param| {
            matches!(
                param.position,
                ParamPosition::Rest | ParamPosition::Keywords
            )
        });
        (
            count(ParamPosition::Required),
            count(ParamPosition::Optional),
            rest,
        )
    }

    // Each usage line takes the arguments the built-in itself takes, where its
    // value knows its arity: Rust functions, Lisp functions and macros.
    // Built-in special forms and Rust macros record none.
    #[test]
    fn every_usage_line_fits_its_built_in() {
        let ctx = TulispContext::new();
        let mut skipped = Vec::new();
        for &(name, doc) in DOCS {
            assert!(ctx.describe(name).is_some(), "{name} is not a built-in");
            let Some((_, usage)) = split_usage(doc) else {
                panic!("{name}: no usage line in {doc:?}");
            };
            let Some(actual) = ctx.arity_signature(name) else {
                skipped.push(name);
                continue;
            };
            assert_eq!(
                counts(&usage),
                counts(&actual),
                "{name}: {} does not take what {} takes",
                usage.render(name),
                actual.render(name)
            );
        }
        // The built-ins with no arity to check against: the special forms and
        // the Rust macros. A new one must be added here on purpose.
        skipped.sort_unstable();
        assert_eq!(skipped, SKIPPED);
    }

    const SKIPPED: &[&str] = &[
        "->",
        "->>",
        "and",
        "catch",
        "cond",
        "condition-case",
        "declare",
        "defconst",
        "defmacro",
        "defun",
        "defvar",
        "dolist",
        "dotimes",
        "function",
        "if",
        "if-let",
        "if-let*",
        "ignore-errors",
        "interactive",
        "lambda",
        "let",
        "let*",
        "or",
        "pop",
        "progn",
        "quote",
        "setq",
        "thread-first",
        "thread-last",
        "unless",
        "unwind-protect",
        "when",
        "when-let",
        "while",
        "while-let",
    ];
}
