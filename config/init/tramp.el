
(progn
  ;; Enable tramp to use PATH of remote environments 
  ;; https://stackoverflow.com/a/61169654
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(progn
  ;; auth-source
  (progn
    ;; authentication priorities
    ;; "~/.authinfo" is not used
    (setq auth-sources '("~/.authinfo.gpg" "~/.netrc")))
  (progn
    ;; disable to ask whether to save authentication information
    ;; the value should be one of nil, t, or 'ask
    (customize-save-variable 'auth-source-save-behavior nil)))

(provide 'dhnam-tramp)
