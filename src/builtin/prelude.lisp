;;; Tulisp built-in prelude.
;;;
;;; This file is embedded into the crate via `include_str!` and
;;; evaluated through the VM at `TulispContext::new()` — see
;;; `context::TulispContext::new`. Every form below is therefore
;;; VM-compiled: `defun`s become `CompiledDefun`s and any internal
;;; `(funcall pred …)` compiles to `Instruction::Funcall` and
;;; dispatches on the *current* `Machine` (no re-borrow of `ctx.vm`).
;;;
;;; That is why these particular functions live here rather than in
;;; Rust. Implementing them as `ctx.defun` wrappers that call
;;; `ctx.map` / `ctx.filter` / `eval::funcall` would deadlock when
;;; invoked from inside an outer VM run with a `CompiledDefun`
;;; predicate, because `funcall`'s `CompiledDefun` arm acquires
;;; `ctx.vm.borrow_mut()` again. Compiling the bodies as VM bytecode
;;; sidesteps that path entirely.

(defun seq-map (func seq)
  (let ((out nil))
    (dolist (item seq)
      (setq out (cons (funcall func item) out)))
    (reverse out)))

(defun mapcar (func seq) (seq-map func seq))

(defun seq-filter (func seq)
  (let ((out nil))
    (dolist (item seq)
      (when (funcall func item)
        (setq out (cons item out))))
    (reverse out)))

(defun seq-reduce (func seq initial)
  (let ((acc initial))
    (dolist (item seq)
      (setq acc (funcall func acc item)))
    acc))

(defun seq-find (func seq &optional default)
  (let ((hit nil) (found nil))
    (dolist (item seq)
      (when (and (not found) (funcall func item))
        (setq hit item)
        (setq found t)))
    (if found hit default)))

(defun mapconcat (func seq &optional sep)
  (let ((parts (seq-map func seq))
        (out "")
        (first t)
        (sep (or sep "")))
    (dolist (s parts)
      (if first
          (setq first nil)
        (setq out (concat out sep)))
      (setq out (concat out s)))
    out))

;; Standard Elisp shape: prog1 returns FIRST after evaluating BODY
;; for side effects; prog2 does the same but with SECOND. Both bind
;; the to-return value to a gensym'd symbol so a BODY that itself
;; uses `prog1-…` / `prog2-…` style names doesn't collide.
(defmacro prog1 (first &rest body)
  (let ((sym (gensym "prog1-")))
    `(let ((,sym ,first))
       ,@body
       ,sym)))

(defmacro prog2 (first second &rest body)
  `(progn ,first (prog1 ,second ,@body)))

(defun sort (seq pred)
  ;; Simple insertion sort — O(n^2), fine for typical Lisp-side use.
  ;; `pred` is called as `(pred a b)` and returns non-nil when `a`
  ;; should sort before `b`, matching Emacs' `sort` contract.
  (let ((out nil))
    (dolist (item seq)
      (let ((inserted nil) (new nil))
        (dolist (x out)
          (if (and (not inserted) (funcall pred item x))
              (progn (setq new (cons item new))
                     (setq new (cons x new))
                     (setq inserted t))
            (setq new (cons x new))))
        (unless inserted
          (setq new (cons item new)))
        (setq out (reverse new))))
    out))
