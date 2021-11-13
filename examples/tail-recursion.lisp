(defun build (counter lis)
  (if (equal counter 0) lis
      (build (- counter 1) (cons counter lis))))

(princ (build 100000 '()))
