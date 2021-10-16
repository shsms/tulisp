(defmacro inc (var)
  (list 'setq var (list '+ 1 var)))

(let ((vv 10))
  (print (macroexpand '(inc vv)))
  (print (inc vv))
  (print `(+ 1 (inc vv)))
  (print `(+ 1 ,(inc vv)))
  (print '`(+ 1 ,(inc vv)))
  (print (eval '`(+ 1 ,(inc vv)))))


(print (macroexpand '10))
(print (macroexpand '(thread-last
      5
      (+ 20)
      (/ 25)
      -
      (+ 40))))
