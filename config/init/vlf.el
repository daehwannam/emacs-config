;;; vlf setting
;; https://github.com/m00natic/vlfi
;; (if (boundp 'vlf-setup)
;;     (require 'vlf-setup))

;; (cond
;;  ((string-equal machine-domain "vbox")
;;   (progn
;;     (require 'vlf-setup)
;;     ))
;;  ((string-equal machine-domain "engels")
;;   (progn
;;     (require 'vlf-setup))
;;   ))

(require 'vlf-setup nil t)

(with-eval-after-load 'vlf
  (require 'dhnam-vlf)

  (fset 'vlf-mode-map vlf-mode-map)
  (key-chord-define vlf-prefix-map "l;" 'vlf-mode-map)

  (define-key vlf-prefix-map (kbd "C-c s") 'vlf-re-search-forward)
  (define-key vlf-prefix-map (kbd "C-c r") 'vlf-re-search-backward)

  (define-key vlf-prefix-map (kbd "M-n") 'dhnam/vlf-re-search-forward-last)
  (define-key vlf-prefix-map (kbd "M-p") 'dhnam/vlf-re-search-backward-last))

(provide 'init-vlf)
