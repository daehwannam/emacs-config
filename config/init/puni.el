
(when (package-installed-p 'puni)
  (require 'puni)

  (require 'dhnam-puni)

  (add-hook 'prog-mode-hook
            (dhnam/hook-except-modes 'puni-mode '(emacs-lisp-mode lisp-mode)))

  (progn
    ;; (define-key puni-mode-map (kbd "M-k") 'dhnam/puni-copy/puni-forward-sexp)
    (define-key puni-mode-map (kbd dhnam/xcape-left-alt) 'dhnam-puni-iokl/body)
    (define-key puni-mode-map (kbd "C-k") 'dhnam/puni-kill-line)
    (define-key puni-mode-map (kbd "M-k") 'dhnam/puni-copy-line)
    (define-key puni-mode-map (kbd "M-(") 'puni-wrap-round))

  (comment
    (define-key global-map (kbd "M-i") 'puni-syntactic-backward-punct)
    (define-key global-map (kbd "M-o") 'puni-syntactic-forward-punct)))

(provide 'init-puni)
