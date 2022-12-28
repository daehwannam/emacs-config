
;;; wgrep
(require 'wgrep nil t)

(defun dhnam/grep-file (command-args)
  ;; modified from `grep'
  (interactive
   (progn
     (grep-compute-defaults)
     (let ((default (grep-default-command)))
       (list (read-shell-command "Run grep (like this): "
                                 (if current-prefix-arg default "grep --color -nH -i -m 1 -e ")
                                 'grep-history
                                 (if current-prefix-arg nil default))))))

  ;; Setting process-setup-function makes exit-message-function work
  ;; even when async processes aren't supported.
  (compilation-start (if (and grep-use-null-device null-device)
			 (concat command-args " " null-device)
		       command-args)
		     'grep-mode))

;; (global-set-key (kbd "C-x C-M-s") 'rgrep)
(key-chord-define-global "rj" 'rgrep)

(when (package-installed-p 'pdfgrep)
  (require 'pdfgrep)
  (pdfgrep-mode))

(provide 'dhnam-grep)
