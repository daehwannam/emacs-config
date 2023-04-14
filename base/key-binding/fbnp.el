
;;; Emacs's key-bindning style


(comment
  (define-key global-map (kbd "C-h") 'backward-char)
  (define-key global-map (kbd "M-h") 'backward-word)
  (define-key global-map (kbd "C-M-h") (if (package-installed-p 'puni) 'puni-backward-sexp 'backward-sexp))

  (fset 'help-map help-map)
  (define-key global-map (kbd "C-v") 'help-map)
  (define-key global-map (kbd "M-v") 'mark-paragraph)
  (define-key global-map (kbd "C-M-v") 'mark-defun)

  (define-key global-map (kbd "C-b") 'dhnam/scroll-up-small)
  (define-key global-map (kbd "M-b") 'dhnam/scroll-down-small)
  (define-key global-map (kbd "C-M-b") 'scroll-other-window))
