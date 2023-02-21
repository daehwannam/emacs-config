
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

(defun dhnam/require-directory-with-prefix (dir prefix)
  (let* ((post-fix-len (length ".el"))
         (dir-path (file-name-as-directory dir))
         (load-it (lambda (file-name)
                    (require (intern (concat prefix (substring file-name 0 (- post-fix-len))))
                             (concat dir-path file-name)))))
    (mapcar load-it (directory-files dir nil "\\.el$"))))

(defun dhnam/require-directory-with-prefix (dir prefix)
  (let* ((post-fix-len (length ".el"))
         (dir-path (file-name-as-directory dir))
         (load-it (lambda (file-name)
                    (require (intern (concat prefix (substring file-name 0 (- post-fix-len))))
                             (concat dir-path file-name)))))
    (mapcar load-it (directory-files dir nil "\\.el$"))))


(defun dhnam/require-from-init (file-name)
  (require (intern (concat "dhnam-" file-name)) (concat "~/.emacs.d/config/init/" file-name ".el")))

(provide 'dhnam-file-loading)

