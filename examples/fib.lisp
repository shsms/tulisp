(defun fib (n)
  (cond
   ((<= n 2) 1)
   (t (+ (fib (- n 1))
         (fib (- n 2))))))

(print (fib 25))
