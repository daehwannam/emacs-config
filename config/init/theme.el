
;;; https://stackoverflow.com/a/12843877
(add-to-list 'custom-theme-load-path (concat dhnam/lib-root-dir "theme"))

;;; highlight current line with keeping syntax coloring
(comment (global-hl-line-mode 1))
(add-hook 'find-file-hook 'hl-line-mode)
;; (global-hl-line-mode 0)  ;; it disables hl-line-mode

;;; color theme change
(require 'dhnam-theme)
(let ((theme-style (dhnam/machine-config-get-first 'theme-style)))
  (progn
    (defvar dhnam/current-theme nil)
    (defun dhnam/load-and-set-current-theme (theme &optional no-confirm no-enable)
      (interactive
       (list
        (intern (completing-read "Load custom theme: "
                                 (mapcar #'symbol-name
				                         (custom-available-themes))))
        nil nil))
      (setq dhnam/current-theme theme)
      (load-theme theme dhnam/current-theme)))

  (cond
   ((eq theme-style 'white)
    (dhnam/load-and-set-current-theme 'leuven t))
   (t ;; (eq theme-style 'dark)
    (comment (dhnam/load-and-set-current-theme 'my-doom-material-dark t))
    (comment (dhnam/load-and-set-current-theme 'tsdh-dark t))
    (dhnam/load-and-set-current-theme 'dhnam-manoj-dark t)
    (comment (dhnam/load-and-set-current-theme 'my-doom-material-dark t))
    (comment (dhnam/load-and-set-current-theme 'doom-material-dark t))
    (comment (dhnam/load-and-set-current-theme 'manoj-dark))

    (cond
     ((eq dhnam/current-theme 'dhnam-manoj-dark)
      (dhnam/update-dhnam-manoj-dark))
     ((eq dhnam/current-theme 'my-doom-material-dark)
      (custom-set-faces
       '(mode-line ((t (:background "gray25" :foreground "white" :box nil))))
       '(mode-line-inactive ((t (:background "#202020" :foreground "dark gray" :box nil)))))
      )
     ((eq dhnam/current-theme 'manoj-dark)
      (custom-set-faces
       ;; mode-line config
       '(mode-line ((t (:background "gray30" :foreground "gray100" :box nil :height 0.9))))
       ;; '(mode-line ((t (:background "dark slate gray" :foreground "gray100" :box nil :height 0.9))))
       '(mode-line-buffer-id ((t (:background "gray65" :foreground "firebrick" :weight bold :height 0.9))))
       '(mode-line-inactive ((t (:background "gray15" :foreground "gray80" :box nil :weight light :height 0.9))))
       '(window-divider ((t (:foreground "gray50")))))))

    (dhnam/update-dark-theme))

   (when (package-installed-p 'highlight-parentheses)
     (custom-set-faces
      '(show-paren-match ((t (:background "black" :foreground "magenta"))))))))

(provide 'init-theme)
