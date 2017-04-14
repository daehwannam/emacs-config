
;; file path
;; http://stackoverflow.com/questions/28196228/emacs-how-to-get-directory-of-current-buffer

;; extract directory
;; http://stackoverflow.com/questions/27284851/emacs-lisp-get-directory-name-not-path-from-the-path

(condition-case nil
   (progn
      (require 'org)
      ;(load (concat (file-name-directory (message buffer-file-name)) "./dependent/org-mode.el")))
      (load-file "~/.emacs.d/config/init/dependent/org-mode.el"))
      ;(load "./dependent/org-mode.el"))
  ;(file-error (message "Org not available; not configuring") ))
)
