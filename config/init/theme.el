
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
     ;; mode-line config
     '(mode-line ((t (:stipple nil :background "midnight blue" :foreground "grey120" :box nil :height 0.9))))
     '(mode-line-buffer-id ((t (:background "gray80" :foreground "firebrick" :weight bold :height 0.9))))
     '(mode-line-inactive ((t (:background "grey30" :foreground "grey80" :box nil :weight light :height 0.9))))
     (set-face-foreground 'hl-line nil))

    (custom-set-faces
     ;; hl-line config (highlighting line without losing character color)
     (set-face-foreground 'hl-line nil))

    (progn
      ;; tab-bar color
      (custom-set-variables
       '(tab-bar-close-button-show nil))
      (custom-set-faces
       '(tab-bar-mode t)
       '(tab-bar ((t (:inherit variable-pitch :background "black" :foreground "black"))))
       '(tab-bar-tab ((t (:inherit default :background "navy" :foreground "white"))))
       '(tab-bar-tab-inactive ((t (:inherit tab-bar-tab :background "black" :foreground "grey100"))))))

    (progn
      ;; disable fringe color
      (fringe-mode 1)
      (set-face-attribute 'fringe nil
                          :foreground (face-foreground 'default)
                          :background (face-background 'default)))


    (custom-set-faces
     ;; highlighting color
     ;;
     ;; e.g. highlight-symbol-at-point highlight-regexp
     '(hi-yellow ((t (:background "yellow1" :foreground "dodger blue"))))
     '(hi-pink ((t (:background "pink" :foreground "forest green"))))
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
	 '(flymake-warnline ((((class color)) (:foreground "yellow")))))))

    (when (fboundp 'ein:run)
      (custom-set-faces
       '(ein:cell-input-area ((t (:background "black"))))))

    (comment(custom-set-faces
             '(org-scheduled ((t (:foreground "PaleGreen")))))))

   (when (package-installed-p 'highlight-parentheses)
     (custom-set-faces
      '(show-paren-match ((t (:background "black" :foreground "magenta"))))))))
