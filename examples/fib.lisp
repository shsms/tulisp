(defun fib (n)
  (cond
   ((<= n 2) 1)
   (t (+ (fib (- n 1))
         (fib (- n 2))))))

(let ((tgt 30))
  (print (format "\n  (fib %d)\n  %d\n" tgt (fib tgt))))
