
;;; https://stackoverflow.com/a/12843877
(add-to-list 'custom-theme-load-path "~/.emacs.d/config/init/theme")

;;; highlight current line with keeping syntax coloring
(global-hl-line-mode 1)
;; (global-hl-line-mode 0)  ;; it disables hl-line-mode

(comment
 ;; highlight the middle line number
 (require 'hmlinum)
 (hmlinum-activate)

 ;; This make emacs very slow!!!!!!
 )

;;; color theme change
(let ((theme-style (machine-config-get-first 'theme-style)))
  (cond
   ((eq theme-style 'white)
    (load-theme 'leuven)
    (custom-set-faces
     '(mlinum-highlight-face ((t (:inherit default :foreground "purple" :background "white"))))
     ))
   (t ;; (eq theme-style 'dark)
    (load-theme 'manoj-dark)
    (custom-set-faces
     ;; custom-set-faces was added by Custom.
     ;; If you edit it by hand, you could mess it up, so be careful.
     ;; Your init file should contain only one such instance.
     ;; If there is more than one, they won't work right.
     (comment '(mode-line-buffer-id ((t (:background "grey65" :foreground "color-160" :weight bold :height 0.9)))))  ; color-160 is not compatible with GUI
     (comment '(mode-line-buffer-id ((t (:background "grey65" :foreground "red" :weight bold :height 0.9)))))
     '(mode-line-buffer-id ((t (:background "grey65" :foreground "firebrick" :weight bold :height 0.9))))
     (set-face-foreground 'hl-line nil))

    (custom-set-faces
     '(hi-yellow ((t (:background "yellow1" :foreground "color-33"))))
     '(hi-pink ((t (:background "pink" :foreground "color-34"))))
     '(hi-green ((t (:background "green" :foreground "magenta"))))
     '(hi-blue ((t (:background "light blue" :foreground "red"))))
     '(mlinum-highlight-face ((t (:inherit default :foreground "brightwhite" :background "black")))))

    (progn
      ;; flymake color
      ;; https://groups.google.com/g/gnu.emacs.help/c/gJpdhosRByY
      (require 'flymake)
      (progn
	;; text color
	(custom-set-faces
	 '(flymake-errline ((((class color)) (:foreground "red"))))
	 '(flymake-warnline ((((class color)) (:foreground "yellow"))))))

      (progn
	;; mode-line color
	;; warning counter uses the color of 'compilation-warning
	;; ==> (put 'flymake-warning 'mode-line-face 'compilation-warning)
	(custom-set-faces
	 '(compilation-warning ((t (:foreground "brightmagenta" :weight bold)))))))

    (when (fboundp 'ein:run)
      (custom-set-faces
       '(ein:cell-input-area ((t (:background "black"))))))

    ;; (custom-set-faces
    ;;  '(org-scheduled ((t (:foreground "PaleGreen")))))

    (comment (set-face-foreground 'mode-line-buffer-id "firebrick")))

   (when (package-installed-p 'highlight-parentheses)
     (custom-set-faces
      '(show-paren-match ((t (:background "black" :foreground "magenta"))))))))


;; (when (require 'material-theme nil t)
;;   (load-theme 'material t) ;; load material theme
;; )

					;(load-theme 'tsdh-light)
