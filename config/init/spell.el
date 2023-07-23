
(let ((executable-name (or (and (executable-find "aspell") "aspell")
                           (and (executable-find "hunspell") "hunspell")
                           (and (executable-find "ispell") "ispell")
                           nil)))
  (when executable-name
    (setq-default ispell-program-name executable-name)))

(progn
  ;; https://stackoverflow.com/a/27044941
  (setq ispell-dictionary "english"))

(with-eval-after-load 'flyspell
  (progn
    ;; remap `flyspell-auto-correct-word'
    (define-key flyspell-mode-map (kbd "C-M-i") nil)
    (define-key flyspell-mode-map (kbd "C-M-S-i") 'flyspell-auto-correct-word)))

(provide 'init-spell)
