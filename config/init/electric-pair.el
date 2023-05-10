
(require 'dhnam-electric-pair)

(progn
  ;; electric-pair-mode
  (add-hook 'prog-mode-hook
            (dhnam/hook-except-modes 'electric-pair-local-mode '(emacs-lisp-mode lisp-mode)))

  (mapc (lambda (hook)
          (add-hook hook 'electric-pair-local-mode))
        '(inferior-python-mode-hook LaTeX-mode-hook))

  (mapc (lambda (hook)
          (add-hook hook 'dhnam/python-add-electric-pairs))
        '(python-mode-hook inferior-python-mode-hook)))


(provide 'init-electric-pair)
