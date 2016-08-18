(defmacro with-gensyms (syms &body body)
  `(let ,(loop for sym in syms collect `(,sym (gensym)))
     ,@body))

(defmacro strap (s &rest l)
  `(concatenate 'string ,s ,@l))

(defmacro range (max)
  `(loop for n from 0 below ,max collect n))

(defmacro alambda (args &body body)
  `(labels ((self ,args ,@body))
     #'self))

(defmacro associate (a b) 
  `(if (= (length ,a) (length ,b))
     (mapcar #'cons ,a ,b)))

(defun prompt (string)
  (write-line "")
  (write-string string) (force-output) (read))

(defun group (list n)
  (if (zerop n) (error "zero length"))
  (labels
    ((rec (list acc)
       (let ((rest (nthcdr n list)))
         (if (consp rest)
           (rec rest (cons (subseq list 0 n) acc))
           (nreverse (cons list acc))))))
    (if list (rec list nil) nil)))

