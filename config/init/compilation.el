
(require 'dhnam-compilation)

(defun dhnam/compilation-mode-hook ()
  (local-set-key (kbd "o") #'dhnam/compile-goto-error-same-window))

(add-hook 'compilation-mode-hook #'dhnam/compilation-mode-hook)

(with-eval-after-load 'grep
  (define-key grep-mode-map (kbd "M-RET") 'dhnam/compile-goto-error-in-vlf))

(provide 'init-compilation)
