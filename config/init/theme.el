
;;; https://stackoverflow.com/a/12843877
(add-to-list 'custom-theme-load-path "~/.emacs.d/config/init/theme")

;;; highlight current line with keeping syntax coloring
(global-hl-line-mode 1)
;; (global-hl-line-mode 0)  ;; it disables hl-line-mode

;; highlight the middle line number
(require 'hmlinum)
(hmlinum-activate)

;;; color theme change
(let ((theme-style (machine-config-get 'theme-style)))
  (cond
   ((eq theme-style 'white)
    (load-theme 'leuven)
    (custom-set-faces
     '(mlinum-highlight-face ((t (:inherit default :foreground "cyan" :background "white"))))
     ))
   (t ;; (eq theme-style 'dark)
    (load-theme 'manoj-dark)
    (custom-set-faces
     ;; custom-set-faces was added by Custom.
     ;; If you edit it by hand, you could mess it up, so be careful.
     ;; Your init file should contain only one such instance.
     ;; If there is more than one, they won't work right.
     '(mode-line-buffer-id ((t (:background "grey65" :foreground "color-160" :weight bold :height 0.9))))
     (set-face-foreground 'hl-line nil))

    (custom-set-faces
     '(hi-yellow ((t (:background "yellow1" :foreground "color-33"))))
     '(hi-pink ((t (:background "pink" :foreground "color-34"))))
     '(hi-green ((t (:background "green" :foreground "magenta"))))
     '(hi-blue ((t (:background "light blue" :foreground "red"))))
     '(mlinum-highlight-face ((t (:inherit default :foreground "cyan" :background "black"))))
     ))
   ))


;; (when (require 'material-theme nil t)
;;   (load-theme 'material t) ;; load material theme
;; )

;(load-theme 'tsdh-light)
