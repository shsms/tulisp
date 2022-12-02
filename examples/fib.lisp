(defun fib (n)
  (if (<= n 2)
      1
      (+ (fib (- n 1))
         (fib (- n 2)))))

(let ((tgt 30))
  (print (format "\n  (fib %d)\n  %d\n" tgt (fib tgt))))
