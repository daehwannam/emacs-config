
(when (package-installed-p 'puni)
  (require 'puni)
  (comment (require 'smartparens))

  (require 'dhnam-puni)

  (add-hook 'prog-mode-hook
            (dhnam/hook-except-modes 'puni-mode '(emacs-lisp-mode lisp-mode)))

  (progn
    ;; (define-key puni-mode-map (kbd "M-k") 'dhnam/puni-copy/puni-forward-sexp)
    (comment
      (define-key puni-mode-map (kbd dhnam/xcape-left-alt) 'dhnam-puni-iokl/body)
      (define-key global-map (kbd dhnam/xcape-left-alt) 'dhnam-puni-iokl/body))

    (let ((map puni-mode-map))
      (comment
        (define-key map (kbd "C-M-f") 'sp-forward-sexp)
        (define-key map (kbd "C-M-b") 'sp-backward-sexp))
      (comment
        (define-key map (kbd "C-M-f") 'puni-syntactic-forward-punct)
        (define-key map (kbd "C-M-b") 'puni-syntactic-backward-punct))
      (comment (define-key map (kbd "C-M-k") 'sp-kill-sexp))
      (define-key puni-mode-map (kbd "C-M-k") 'dhnam/puni-kill-sexp)
      (define-key map (kbd "C-k") 'dhnam/puni-kill-line)
      (define-key map (kbd "M-k") 'dhnam/puni-copy-line)
      (define-key map (kbd "M-(") 'puni-wrap-round)
      (define-key map (kbd "M-)") nil)))

  (comment
    (define-key global-map (kbd "M-i") 'puni-syntactic-backward-punct)
    (define-key global-map (kbd "M-o") 'puni-syntactic-forward-punct))

  (comment
    (with-eval-after-load 'comint
      (let ((map comint-mode-map))
        (comment
          (define-key map (kbd "C-M-f") 'puni-syntactic-forward-punct)
          (define-key map (kbd "C-M-b") 'puni-syntactic-backward-punct))
        (comment
          (define-key map (kbd "C-M-f") 'sp-forward-sexp)
          (define-key map (kbd "C-M-b") 'sp-backward-sexp))
        (comment (define-key map (kbd "C-M-k") 'sp-kill-sexp))
        (progn
          (define-key map (kbd "C-M-f") 'puni-forward-sexp)
          (define-key map (kbd "C-M-b") 'puni-backward-sexp))
        (define-key map (kbd "C-M-k") 'dhnam/puni-kill-sexp))))

  (progn
    (let ((map global-map))
      (progn
        (define-key map (kbd "C-M-f") 'puni-forward-sexp)
        (define-key map (kbd "C-M-b") 'puni-backward-sexp))
      (define-key map (kbd "C-M-k") 'dhnam/puni-kill-sexp)
      (define-key map (kbd "M-(") 'puni-wrap-round))))

(provide 'init-puni)
