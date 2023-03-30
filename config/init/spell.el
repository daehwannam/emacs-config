
(let ((executable-name (or (and (executable-find "aspell") "aspell")
                           (and (executable-find "hunspell") "hunspell")
                           (and (executable-find "ispell") "ispell")
                           nil)))
  (when executable-name
    (setq-default ispell-program-name executable-name)))

(progn
  ;; https://stackoverflow.com/a/27044941
  (setq ispell-dictionary "english"))

(provide 'init-spell)
