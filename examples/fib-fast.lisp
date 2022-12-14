(defun fib-fast (n)
  (if (<= n 1)
      n
      (let* ((n1 1)
             (n2 1)
             (tmp)
             (c 2))
        (while (< c n)
          (setq tmp (+ n1 n2))
          (setq n1 n2)
          (setq n2 tmp)
          (setq c (+ 1 c)))
        n2)))

(print (fib-fast 30))
