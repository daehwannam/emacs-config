
(use-existing-pkg company
  :config
  (progn
    (comment (add-hook 'after-init-hook 'global-company-mode))
    (add-hook 'emacs-lisp-mode-hook 'company-mode)))
