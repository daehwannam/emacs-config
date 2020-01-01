
(defpackage "TTT" (:use "COMMON-LISP") (:nicknames "T"))
(in-package "TTT")

(require "uuu" "dir/uuu")

(defun print-name ()
  (format t "this is ttt~%"))

(print-name)
(uuu::print-name)
(vvv:print-name)

;; (import 'vvv:print-name)  ;; name-conflicts between VVV:PRINT-NAME, TTT::PRINT-NAME
;; (print-name)  ;; if the above import suceeds, it is vvv:print-name

(format t (package-name *package*))
(format t "~%")

(provide "ttt") ; it prevent to reloading this file

