(defun fib-impl (n f1 f2)
  (if (equal 0 n) f1
      (fib-impl (- n 1) f2 (+ f1 f2))))

(defun fib (n)
  "Tail recursive Fib"
  (fib-impl n 0 1))

(princ (fib 30))
