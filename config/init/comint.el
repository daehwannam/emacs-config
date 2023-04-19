(require 'comint)

(with-eval-after-load 'compile
  (add-hook 'comint-mode-hook 'compilation-shell-minor-mode)

  (progn
    (define-key comint-mode-map (kbd "M-P") 'comint-previous-matching-input-from-input)
    (define-key comint-mode-map (kbd "M-N") 'comint-next-matching-input-from-input))

  (progn
    (define-key compilation-shell-minor-mode-map (kbd "C-M-n") nil)
    (define-key compilation-shell-minor-mode-map (kbd "C-M-p") nil)))

(provide 'init-comint)
