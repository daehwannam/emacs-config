
(defpackage "UUU" (:use "COMMON-LISP") (:nicknames "U"))
(in-package "UUU")

(require "vvv" "vvv")

(defun print-name ()
  (format t "this is uuu~%"))

(print-name)

(format t (package-name *package*))
(format t "~%")

(provide "uuu") ; it prevent to reloading this file
