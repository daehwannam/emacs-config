
(defun get-list-inserted-after (lst index newelt)
  (list-insert-after (cl-copy-list lst) index newelt))

(defun list-insert-after (lst index newelt)
  (push newelt (cdr (nthcdr index lst)))
  lst)


(defun cl-concatenate (type &rest sequences)
  "Concatenate, into a sequence of type TYPE, the argument SEQUENCEs.
\n(fn TYPE SEQUENCE...)"
  (pcase type
    (`vector (apply #'vconcat sequences))
    (`string (apply #'concat sequences))
    (`list (apply #'append (append sequences '(nil))))
    (_ (error "Not a sequence type name: %S" type))))

