
(defpackage "VVV" (:use "COMMON-LISP") (:nicknames "V") (:export "PRINT-NAME"))
(in-package "VVV")

;; (export 'print-name)

(defun print-name ()
  (format t "this is vvv~%"))

(print-name)

(format t (package-name *package*))
(format t "~%")

(provide "vvv") ; it prevent to reloading this file
