(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro alambda (args &body body)
  `(labels ((self ,args ,@body))
     #'self))



