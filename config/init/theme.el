
;;; https://stackoverflow.com/a/12843877
(add-to-list 'custom-theme-load-path "~/.emacs.d/config/init/theme")

;;; color theme change
(load-theme 'manoj-dark)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line-buffer-id ((t (:background "grey65" :foreground "color-160" :weight bold :height 0.9)))))

;;; highlight current line with keeping syntax coloring
(global-hl-line-mode 1)
(set-face-foreground 'hl-line nil)


;; (when (require 'material-theme nil t)
;;   (load-theme 'material t) ;; load material theme
;; )

;(load-theme 'tsdh-light)
