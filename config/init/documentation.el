
(require 'dhnam-documentation)

(global-set-key (kbd "C-c / /") 'poporg-dwim)
(global-set-key (kbd "C-c / r") 'dhnam/pop-rst-dwim)

(with-eval-after-load 'poporg
  (define-key poporg-mode-map (kbd "<key-chord> qw k")  'dhnam/poporg-kill-buffer)
  (define-key poporg-mode-map (kbd "<key-chord> wq k")  'dhnam/poporg-kill-buffer)
  (when dhnam/exwm-cmd-line-arg-passed
    (define-key poporg-mode-map (kbd "s-k") 'dhnam/poporg-kill-buffer-exit)))
  

(provide 'init-documentation)
