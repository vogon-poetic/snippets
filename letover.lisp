(defun |#"-reader| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let (chars)
    (do ((prev (read-char stream) curr)
         (curr (read-char stream) (read-char stream)))
        ((and (char= prev #\") (char= curr #\#)))
      (push prev chars))
    (coerce (nreverse chars) 'string)))

(defun segment-reader (stream ch n)
  (if (> n 0) 
    (let ((chars))
      (do ((curr (read-char stream)
                 (read-char stream)))
          ((char= ch curr))
        (push curr chars))
      (cons (coerce (nreverse chars) 'string)
            (segment-reader stream ch (- n 1))))))

(defun cyclic-p (l)
  (cyclic-p-aux l (make-hash-table)))

(defun cyclic-p-aux (l seen)
  (if (consp l)
    (or (gethash l seen)
        (progn 
          (setf (gethash 1 seen) t)
          (or (cyclic-p-aux (car l) seen)
              (cyclic-p-aux (cdr l) seen))))))

(defmacro nlet (n letargs &rest body)
  `(labels ((,n ,(mapcar #'car letargs) ,@body))
     (,n ,@(mapcar #'cadr letargs))))

(defun nlet-fact (n)
  (nlet fact ((n n))
    (if (zerop n) 1
      (* n (fact (- n 1))))))

;;; Examples of stateful functions (closures)

(defmacro alambda (args &body body)
  `(labels ((self ,args ,@body))
     #'self))

(defmacro alet (letargs &rest body)
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     (lambda (&rest params)
       (apply this params))))

(setf 
  (symbol-function 'alet-test)
  (alet ((acc 0))
    (alambda (n)
      (if (eq n 'invert)
        (setq this
          (lambda (n)
            (if (eq n 'invert)
              (setq this #'self)
              (decf acc n))))
        (incf acc n)))))

(defmacro for ((var start stop) &body body)
  `(do ((b #'(lambda (,var) ,@body))
        (count ,start (1+ count))
        (limit ,stop))
       ((> count limit))
     (funcall b count)))



