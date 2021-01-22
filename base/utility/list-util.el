
(defun get-list-inserted-after (lst index newelt)
  (list-insert-after (cl-copy-list lst) index newelt))

(defun list-insert-after (lst index newelt)
  (push newelt (cdr (nthcdr index lst)))
  lst)
