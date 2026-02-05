
(require 'dhnam-documentation)

(global-set-key (kbd "C-c / /") 'poporg-dwim)
(global-set-key (kbd "C-c / r") 'dhnam/pop-rst-dwim)

(with-eval-after-load 'poporg
  (define-key poporg-mode-map (kbd "<key-chord> qw k")  'dhnam/poporg-kill-buffer)
  (define-key poporg-mode-map (kbd "<key-chord> wq k")  'dhnam/poporg-kill-buffer)
  (when dhnam/exwm-cmd-line-arg-passed
    (define-key poporg-mode-map (kbd "s-k") 'dhnam/poporg-kill-buffer-exit)))

(when (fboundp 'hl-todo-mode)
  (add-hook 'prog-mode-hook 'hl-todo-mode)

  (setq hl-todo-keyword-faces
        '(("TODO"       . "#cc9393")
          ("DONE"       . "#afd8af")
          ("FIXME"      . "#cc9393")
          ("FIXED"      . "#afd8af")
          ("BUG"        . "#f08080")
          ("DEBUG"      . "#dc8cc3")
          ("DEBUGGED"   . "#afd8af")
          ("XXX"        . "#cc9393")
          ("NOTE"       . "#d0bf8f")
          ("HACK"       . "#d0bf8f")
          ("OPTIMIZE"   . "#d0bf8f")
          ("FUTURE"     . "#00bfff"))))

(progn
  (require 'todo-comment-table)

  (global-set-key (kbd "C-c M-m") 'tct/toggle-todo)
  (define-key prog-mode-map (kbd "C-c M-m") 'tct/toggle-todo-in-program)
  (global-set-key (kbd "C-c M-u") 'tct/summarize-todo)

  (setq tct/non-paired-keywords (tct/get-non-paired-keywords (mapcar #'car hl-todo-keyword-faces))))
  

(provide 'init-documentation)
