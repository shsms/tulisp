(defun fizz-buzz (x)
  (if (equal 0 (mod x 15))
      "FizzBuzz"
    (if (equal 0 (mod x 3))
        "Fizz"
      (if (equal 0 (mod x 5))
          "Buzz"
        x))))

(let ((count 1))
  (while (<= count 100)
    (print (fizz-buzz count))
    (setq count (+ 1 count))))
