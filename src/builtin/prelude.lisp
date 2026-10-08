;;; Tulisp built-in prelude.
;;;
;;; This file is embedded into the crate via `include_str!` and
;;; evaluated through the VM at `TulispContext::new()` — see
;;; `context::TulispContext::new`. Every form below is therefore
;;; VM-compiled: `defun`s become `CompiledDefun`s and any internal
;;; `(funcall pred …)` compiles to `Instruction::Funcall` and
;;; dispatches on the *current* `Machine`.
;;;
;;; That is why these particular functions live here rather than in
;;; Rust: with the bodies compiled as bytecode, the loop over the
;;; elements is bytecode too, and each call of the predicate is a
;;; `Funcall` instruction on the machine already running, not a call
;;; from a Rust loop through `ctx.funcall`.
;;;
;;; `dolist` runs forever on a list that loops back, as in Emacs, so
;;; the functions below that walk all of SEQ first call `length`,
;;; which signals an error for such a list, as Emacs's `mapcar` does.
;;; A string is walked as the list of its characters.

(defun seq-map (func seq)
  (if (stringp seq) (setq seq (append seq nil)) (length seq))
  (let ((out nil))
    (dolist (item seq)
      (setq out (cons (funcall func item) out)))
    (reverse out)))

(defun mapcar (func seq) (seq-map func seq))

(defun seq-filter (func seq)
  (if (stringp seq) (setq seq (append seq nil)) (length seq))
  (let ((out nil))
    (dolist (item seq)
      (when (funcall func item)
        (setq out (cons item out))))
    (reverse out)))

(defun seq-reduce (func seq initial)
  (if (stringp seq) (setq seq (append seq nil)) (length seq))
  (let ((acc initial))
    (dolist (item seq)
      (setq acc (funcall func acc item)))
    acc))

(defun seq-find (func seq &optional default)
  (if (stringp seq) (setq seq (append seq nil)) (length seq))
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
