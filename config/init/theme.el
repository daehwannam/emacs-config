
;;; https://stackoverflow.com/a/12843877
(add-to-list 'custom-theme-load-path "~/.emacs.d/config/init/theme-collection")

;;; highlight current line with keeping syntax coloring
(comment (global-hl-line-mode 1))
(add-hook 'find-file-hook 'hl-line-mode)
;; (global-hl-line-mode 0)  ;; it disables hl-line-mode

(comment
  ;; highlight the middle line number
  (require 'hmlinum)
  (hmlinum-activate)

  ;; This make emacs very slow!!!!!!
  )

;;; color theme change
(let ((theme-style (machine-config-get-first 'theme-style)))
  (progn
    (defvar my-current-theme nil)
    (defun load-and-set-current-theme (theme &optional no-confirm no-enable)
      (interactive
       (list
        (intern (completing-read "Load custom theme: "
                                 (mapcar #'symbol-name
				         (custom-available-themes))))
        nil nil))
      (setq my-current-theme theme)
      (load-theme theme my-current-theme)))

  (cond
   ((eq theme-style 'white)
    (load-and-set-current-theme 'leuven t)
    (custom-set-faces
     '(mlinum-highlight-face ((t (:inherit default :foreground "purple" :background "white"))))
     ))
   (t ;; (eq theme-style 'dark)
    (comment (load-and-set-current-theme 'my-doom-material-dark t))
    (comment (load-and-set-current-theme 'tsdh-dark t))
    (load-and-set-current-theme 'my-manoj-dark t)
    (comment (load-and-set-current-theme 'my-doom-material-dark t))
    (comment (load-and-set-current-theme 'doom-material-dark t))
    (comment (load-and-set-current-theme 'manoj-dark))

    (cond
     ((eq my-current-theme 'my-manoj-dark)
      (custom-set-faces
       ;; mode-line config
       '(mode-line ((t (:background "gray20" :foreground "gray80" :box nil))))
       '(mode-line-inactive ((t (:background "gray10" :foreground "gray50" :box nil :weight light)))))

      (comment
        (when (and exwm-cmd-arg-passed (display-graphic-p))
          (custom-set-faces
           '(default ((t (:background "gray12"))))))))
     ((eq my-current-theme 'my-doom-material-dark)
      (custom-set-faces
       '(mode-line ((t (:background "gray25" :foreground "white" :box nil))))
       '(mode-line-inactive ((t (:background "#202020" :foreground "dark gray" :box nil)))))
      )
     ((eq my-current-theme 'manoj-dark)
      (custom-set-faces
       ;; mode-line config
       '(mode-line ((t (:background "gray30" :foreground "gray100" :box nil :height 0.9))))
       ;; '(mode-line ((t (:background "dark slate gray" :foreground "gray100" :box nil :height 0.9))))
       '(mode-line-buffer-id ((t (:background "gray65" :foreground "firebrick" :weight bold :height 0.9))))
       '(mode-line-inactive ((t (:background "gray15" :foreground "gray80" :box nil :weight light :height 0.9))))
       '(window-divider ((t (:foreground "gray50")))))))

    (progn
      (require 'hl-line)
      (custom-set-faces
       ;; hl-line config (highlighting line without losing character color)
       (set-face-foreground 'hl-line nil)))

    (progn
      ;; tab-bar color
      (custom-set-variables
       '(tab-bar-close-button-show nil))
      (custom-set-faces
       '(tab-bar-mode t)
       '(tab-bar ((t (:inherit variable-pitch :background "black" :foreground "black"))))
       '(tab-bar-tab ((t (:inherit default :background "gray30" :foreground "white" :box (:line-width 2 :color "gray40")))))
       '(tab-bar-tab-inactive ((t (:inherit tab-bar-tab :background "black" :foreground "gray90"))))))

    (progn
      ;; disable fringe color
      (comment (fringe-mode 1))
      (set-face-attribute 'fringe nil
                          :foreground (face-foreground 'default)
                          :background (face-background 'default)))


    (progn
      (custom-set-faces
       ;; highlighting color
       ;;
       ;; e.g. highlight-symbol-at-point highlight-regexp
       '(hi-yellow ((t (:background "yellow1" :foreground "dodger blue"))))
       '(hi-pink ((t (:background "pink" :foreground "forest green"))))
       '(hi-green ((t (:background "green" :foreground "magenta"))))
       '(hi-blue ((t (:background "light blue" :foreground "red"))))
       '(hi-black ((t (:background "light purple" :foreground "green"))))
       '(hi-salmon ((t (:background "salmon" :foreground "blue"))))
       '(hi-aquamarine ((t (:background "aquamarine" :foreground "red")))))

      (progn
        ;; restrict colors
        (setq hi-lock-face-defaults '("hi-yellow" "hi-pink" "hi-green" "hi-blue" "hi-salmon" "hi-aquamarine"))))

    (custom-set-faces
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

    (custom-set-faces
     ;; swiper use underline instead of highlight
     '(swiper-line-face ((t (:inherit nil :overline nil :underline t))))
     (comment
       ;; grayxx overlays highlighted colors
       '(swiper-line-face ((t (:inherit nil :background "gray25" :overline nil :underline nil)))))
     (comment
       ;; color-xxx is only working terminal
       '(swiper-line-face ((t (:inherit nil :background "color-236" :overline nil :underline nil))))))

    (progn
      (custom-set-faces
       '(highlight-indentation-face ((t (:background "gray30"))))))

    (when (fboundp 'ein:run)
      (custom-set-faces
       '(ein:cell-input-area ((t (:background "black"))))))

    (comment(custom-set-faces
             '(org-scheduled ((t (:foreground "PaleGreen"))))))

    (progn
      ;; vertico & consult config
      (custom-set-faces
       ;; '(vertico-current ((t (:inherit highlight :background "royal blue"))))
       ;; '(consult-preview-line ((t (:inherit highlight :background "royal blue"))))
       '(vertico-current ((t (:inherit nil :underline t))))
       '(consult-preview-line ((t (:inherit nil :underline t))))))
    )

   (when (package-installed-p 'highlight-parentheses)
     (custom-set-faces
      '(show-paren-match ((t (:background "black" :foreground "magenta"))))))))
