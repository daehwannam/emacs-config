
(use-existing-pkg company
  :bind
  (nil
   :map company-mode-map

   ;; "C-M-i" is originally mapped to `completion-at-point'
   ("C-M-i" . company-complete))

  :config
  (progn
    ;; allow to type non-matching characters
    ;; https://emacs.stackexchange.com/a/18696
    (setq company-require-match nil))

  :init
  (progn
    (add-hook 'after-init-hook 'global-company-mode)
    (comment (add-hook 'emacs-lisp-mode-hook 'company-mode)))
  (progn
    ;; disable automatic pop-up
    ;; https://stackoverflow.com/questions/69439076/how-to-prevent-automatic-auto-completion-window-pop-up-after-class-member-access
    (setq company-idle-delay nil))
  (comment (global-set-key (kbd "C-M-i") 'company-complete)))

(provide 'init-company)
