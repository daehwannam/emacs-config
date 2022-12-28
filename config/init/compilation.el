
;; https://emacs.stackexchange.com/a/33908

(defun dhnam/compile-goto-error-same-window ()
  (interactive)
  (let ((display-buffer-overriding-action
         '((display-buffer-reuse-window
            display-buffer-same-window)
           (inhibit-same-window . nil))))
    (call-interactively #'compile-goto-error)))

(defun dhnam/compilation-mode-hook ()
  (local-set-key (kbd "o") #'dhnam/compile-goto-error-same-window))

(add-hook 'compilation-mode-hook #'dhnam/compilation-mode-hook)

(provide 'dhnam-compilation)
