(require 'comint)
(require 'dhnam-comint)

(with-eval-after-load 'compile
  (add-hook 'comint-mode-hook 'compilation-shell-minor-mode)

  (progn
    (define-key comint-mode-map (kbd "M-P") 'comint-previous-matching-input-from-input)
    (define-key comint-mode-map (kbd "M-N") 'comint-next-matching-input-from-input))

  (progn
    (define-key compilation-shell-minor-mode-map (kbd "C-M-n") nil)
    (define-key compilation-shell-minor-mode-map (kbd "C-M-p") nil)))

(defun dhnam/after-comint-with-command ()
  (local-set-key (kbd "C-c r") #'dhnam/comint-with-command-again))

(comment
  ;; Useful commands
  ;; - `comint-delete-output' (C-c C-o)
  )

(provide 'init-comint)
