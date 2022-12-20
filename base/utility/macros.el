
(defmacro comment (&rest args)
  `nil)

(defmacro assert (test-form)
  ;; https://emacs.stackexchange.com/a/22082
  `(when (not ,test-form)
     (error "Assertion failed: %s" (format "%s" ',test-form))))

(progn
  (defmacro define-self-insert-commands-unless-bound (keymap &optional prefix)
    (append
     '(progn)
     (mapcar (lambda (ch)
	       `(unless (lookup-key ,keymap (kbd (char-to-string ,ch)))
		  (define-key ,keymap (kbd (char-to-string ,ch))
		    (lambda () (interactive) (insert (or ,prefix "") (char-to-string ,ch))))))
	     (append (number-sequence ?A ?Z) (number-sequence ?a ?z))))))
