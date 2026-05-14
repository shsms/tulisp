(defun build (counter lis)
  (if (equal counter 0) lis
      (build (- counter 1) (cons counter lis))))

(print (build 1000000 '()))

