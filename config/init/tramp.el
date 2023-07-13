
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

(progn
 ;; default shell setting
 ;; withotu setting `explicit-shell-file-name', the default shell is "/usr/bin/sh"
 (setq explicit-shell-file-name "/usr/bin/bash"))

(progn
  ;; Enable directory local variables in remote environments (via trmap ssh)
  (setq enable-remote-dir-locals t))

(provide 'init-tramp)
