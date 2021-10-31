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
(print (eval (macroexpand '(thread-last
                            5
                            (+ 20)
                            (/ 25)
                            -
                            (+ 40)))))

(print (macroexpand '(thread-first
      5
      (+ 20)
      (/ 25)
      -
      (+ 40))))

(print (thread-first
         5
         (+ 20)
         (/ 25)
         -
         (+ 40)))

(princ (/ 5 10))
