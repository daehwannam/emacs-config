
(let ((executable-name (or (and (executable-find "aspell") "aspell")
                           (and (executable-find "hunspell") "hunspell")
                           (and (executable-find "ispell") "ispell")
                           nil)))
  (when executable-name
    (setq-default ispell-program-name executable-name)))
