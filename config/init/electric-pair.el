
(require 'dhnam-electric-pair)

(progn
  ;; electric-pair-mode

  (progn
    ;; Add electric-pair-local-mode to the hook for other modes

    ;; Modes that belongs to prog-mode
    (add-hook 'prog-mode-hook
              (dhnam/hook-except-modes 'electric-pair-local-mode '(emacs-lisp-mode lisp-mode)))

    ;; Modes that don't belong to prog-mode
    (mapc (lambda (hook)
            (add-hook hook 'electric-pair-local-mode))
          '(inferior-python-mode-hook LaTeX-mode-hook org-mode-hook)))

  (progn
    ;; Update pairs for Python
    (mapc (lambda (hook)
            (add-hook hook 'dhnam/set-python-electric-pairs))
          '(python-mode-hook inferior-python-mode-hook)))

  (add-hook 'org-mode-hook 'dhnam/set-org-electric-pair-inhibit-predicate))

(comment
  (with-eval-after-load 'elec-pair
   (progn)))


(provide 'init-electric-pair)
