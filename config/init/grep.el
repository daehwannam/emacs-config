
;;; wgrep
(require 'wgrep nil t)

(require 'dhnam-grep)

;; (global-set-key (kbd "C-x C-M-s") 'rgrep)
(key-chord-define-global "rj" 'rgrep)

(when (package-installed-p 'pdfgrep)
  (require 'pdfgrep)
  (pdfgrep-mode))

(provide 'init-grep)
