;;; customization file setting
;; http://stackoverflow.com/questions/5052088/what-is-custom-set-variables-and-faces-in-my-emacs

;; http://ergoemacs.org/emacs/elisp_determine_OS_version.html
(setq custom-file (concat dhnam/emacs-root-path "config/custom/custom.el"))
(unless (file-exists-p custom-file)
  ;; https://stackoverflow.com/questions/17376706/in-emacs-lisp-how-can-i-append-a-string-to-a-file-that-i-dont-like-to-open
  ;; https://stackoverflow.com/questions/14071991/how-to-create-an-empty-file-by-elisp/14072295
  (write-region "t" nil custom-file)) ; 't' is for Windows. Loading empyt files in Windows occurs error.

(load custom-file)

(provide 'init-custom-setting)
