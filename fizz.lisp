(defmacro strap (s &rest l)
  "Helper macro for appending strings. 
   Usage: (strap \"abc\" \"def\") => \"abcdef\""
  `(concatenate 'string ,s ,@l))

(defun fizzbuzz ()
  (do ((n 1 (1+ n)))
      ((> n 100))
    (let ((word "")) 
      (if (= 0 (mod n 3)) (setf word (strap word "Fizz")))
      (if (= 0 (mod n 5)) (setf word (strap word "Buzz")))
      (format t "~a ~a~%" n word)
      (setf word " "))))

