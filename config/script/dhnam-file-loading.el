
(require 'dhnam-directory-files-recursively nil t)

(defun dhnam/load-directory (dir)
  ;; https://www.emacswiki.org/emacs/LoadingLispFiles
  (let ((load-it (lambda (f)
		           (load-file (concat (file-name-as-directory dir) f)))))
    (mapc load-it (directory-files dir nil "\\.el$"))))

(defun dhnam/require-directory (dir)
  (let* ((post-fix-len (length ".el"))
         (load-it (lambda (file-name)
                    (require (intern (substring file-name 0 (- post-fix-len)))))))
    (mapcar load-it (directory-files dir nil "\\.el$"))))

(provide 'dhnam-file-loading)
