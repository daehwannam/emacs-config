
;; https://www.emacswiki.org/emacs/LoadingLispFiles

(defun dhnam/load-directory (dir)
  (let ((load-it (lambda (f)
		   (load-file (concat (file-name-as-directory dir) f)))
		 ))
    (mapc load-it (directory-files dir nil "\\.el$"))))


(provide 'dhnam-load-directory)
