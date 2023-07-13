
(require 'dhnam-electric-pair)

(progn
  ;; electric-pair-mode
  (add-hook 'prog-mode-hook
            (dhnam/hook-except-modes 'electric-pair-local-mode '(emacs-lisp-mode lisp-mode)))

  (mapc (lambda (hook)
          (add-hook hook 'electric-pair-local-mode))
        '(inferior-python-mode-hook LaTeX-mode-hook org-mode-hook))

  (mapc (lambda (hook)
          (add-hook hook 'dhnam/set-python-electric-pairs))
        '(python-mode-hook inferior-python-mode-hook))

  (add-hook 'org-mode-hook 'dhnam/set-org-electric-pair-inhibit-predicate))


(provide 'init-electric-pair)
