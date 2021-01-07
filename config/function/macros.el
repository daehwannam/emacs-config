
(defmacro comment (&rest args)
  `nil)


(defmacro assert (test-form)
  ;; https://emacs.stackexchange.com/a/22082
  `(when (not ,test-form)
     (error "Assertion failed: %s" (format "%s" ',test-form))))
