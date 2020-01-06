
(defmacro our-progn (&body body)
  (format t "macro expansion: our-progn~%")
  `(progn ,@body))

(defmacro our-if (test then &optional else)
  (format t "macro expansion: our-if~%")
  `(if ,test ,then ,else))

(defmacro our-when (test &body body)
  (format t "macro expansion: our-when~%")
  `(our-if ,test (our-progn ,@body)))

(macroexpand-1 '(our-when t (format nil "abc")))  ; first macro call is expanded
(macroexpand '(our-when t (format nil "abc")))  ; macro is expanded until the expression is not a macro call
(defparameter f (lambda () (our-when t (format nil "abc"))))  ; macro is expanded until the expression contains no macro call
(funcall f)  ; no macro is expanded

