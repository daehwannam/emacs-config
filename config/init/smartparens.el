

(comment
  (when (package-installed-p 'smartparens)
    (require 'smartparens-config)
    (require 'dhnam-smartparens)

    (add-hook 'prog-mode-hook
              (lambda ()
                (let ((excepted-modes '(emacs-lisp-mode lisp-mode)))
                  (dhnam/with-eval-except-modes excepted-modes
                    (turn-on-smartparens-strict-mode)))))

    (with-eval-after-load 'smartparens
      (progn
        ;; Smartparens key binding examples
        ;; https://ebzzry.com/en/emacs-pairs/#keys
        ;; https://github.com/Fuco1/.emacs.d/blob/master/files/smartparens.el

        (define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
        (define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)

        (define-key smartparens-mode-map (kbd "C-M-u") 'sp-backward-up-sexp)
        (define-key smartparens-mode-map (kbd "C-M-d") 'sp-down-sexp)

        (define-key smartparens-mode-map (kbd "C-M-a") 'sp-backward-down-sexp)
        (define-key smartparens-mode-map (kbd "C-M-e") 'sp-up-sexp)

        (comment (define-key smartparens-mode-map (kbd "C-S-d") 'sp-beginning-of-sexp))
        (comment (define-key smartparens-mode-map (kbd "C-S-a") 'sp-end-of-sexp))

        (comment (define-key smartparens-mode-map (kbd "C-M-t") 'sp-transpose-sexp))

        (define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
        (define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)

        ;; (define-key smartparebns-mode-map (kbd "M-<delete>") 'sp-unwrap-sexp)
        ;; (define-key smartparens-mode-map (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)

        ;; (define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
        ;; (define-key smartparens-mode-map (kbd "C-<left>") 'sp-forward-barf-sexp)
        ;; (define-key smartparens-mode-map (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
        ;; (define-key smartparens-mode-map (kbd "C-M-<right>") 'sp-backward-barf-sexp)

        ;; (define-key smartparens-mode-map (kbd "M-D") 'sp-splice-sexp)
        ;; (define-key smartparens-mode-map (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
        ;; (define-key smartparens-mode-map (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
        ;; (define-key smartparens-mode-map (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)

        (comment (define-key smartparens-mode-map (kbd "C-]") 'sp-select-next-thing-exchange))
        (comment (define-key smartparens-mode-map (kbd "C-<left_bracket>") 'sp-select-previous-thing))
        (comment (define-key smartparens-mode-map (kbd "C-M-]") 'sp-select-next-thing))

        (define-key smartparens-mode-map (kbd "M-F") 'sp-forward-symbol)
        (define-key smartparens-mode-map (kbd "M-B") 'sp-backward-symbol)

        (comment (define-key smartparens-mode-map (kbd "C-\"") 'sp-change-inner))
        (comment (define-key smartparens-mode-map (kbd "M-i") 'sp-change-enclosing))

        (define-key smartparens-strict-mode-map [remap dhnam/copy-line] 'dhnam/sp-copy-hybrid-sexp))
      

      (define-key smartparens-mode-map (kbd dhnam/xcape-left-alt) 'dhnam-iokl-smartparens/body))))

(provide 'init-smartparens)
