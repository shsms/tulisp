(defmacro inc (var)
  `(setq ,var (+ 1 ,var)))

(defun test () 10)

(let ((vv (test)))
  (print (macroexpand '(inc vv)))
  (print (inc vv))
  (print `(+ 1 (inc vv)))
  (print `(+ 1 ,(inc vv)))
  (print '`(+ 1 ,(inc vv))))

(let ((vv (test)))
  (print (macroexpand '10)))
