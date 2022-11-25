
(use-existing-pkg company
  :config
  (progn
    (comment (add-hook 'after-init-hook 'global-company-mode))
    (add-hook 'emacs-lisp-mode-hook 'company-mode))

  (progn
    ;; allow to type non-matching characters
    ;; https://emacs.stackexchange.com/a/18696
    (setq company-require-match nil)))
