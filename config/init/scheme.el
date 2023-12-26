
(when (package-installed-p 'geiser)
  ;; Note:
  ;; To use guile, the `geiser-guile' package should be installed.

  (comment
    (setq geiser-default-implementation 'guile)
    (comment
      ;; `geiser-active-implementations' is automatically initialized
      (setq geiser-active-implementations '(guile))))

  (with-eval-after-load 'geiser-mode
    (comment
      (define-key geiser-mode-map (kbd "C-c C-p") 'geiser-mode-switch-to-repl)))

  (when (package-installed-p 'ac-geiser)
    ;; ac-geiser
    ;; https://github.com/xiaohanyu/ac-geiser

    (progn
      ;; The default configuration from https://github.com/xiaohanyu/ac-geiser
      (require 'ac-geiser)
      (add-hook 'geiser-mode-hook 'ac-geiser-setup)
      (add-hook 'geiser-repl-mode-hook 'ac-geiser-setup)
      (eval-after-load "auto-complete"
        '(add-to-list 'ac-modes 'geiser-repl-mode)))

    (with-eval-after-load 'geiser-mode
      (define-key geiser-mode-map (kbd "C-M-i") 'company-complete))))


(provide 'init-scheme)
