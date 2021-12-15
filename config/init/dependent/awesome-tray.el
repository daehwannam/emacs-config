
(when (member 'exwm-setup machine-options)
  (add-to-list `load-path (expand-file-name "~/.emacs.d/config/package/awesome-tray/"))
  (require 'awesome-tray)

  (progn
    ;; customize
    (setq awesome-tray-active-modules '("battery" "date"))

    (defun awesome-tray-module-date-info ()
      (comment (format-time-string "%m-%d %H:%M %a"))
      (format-time-string "%H:%M %a %m-%d-%Y")))

  (defun awesome-tray-enable ()
    ;; Save mode-line colors when first time.
    ;; Don't change `awesome-tray-mode-line-colors' anymore.
    (unless awesome-tray-mode-line-colors
      (setq awesome-tray-mode-line-colors
            (list (face-attribute 'mode-line :foreground)
                  (face-attribute 'mode-line :background)
                  (face-attribute 'mode-line :family)
                  (face-attribute 'mode-line :box)
                  (face-attribute 'mode-line-inactive :foreground)
                  (face-attribute 'mode-line-inactive :background)
                  (face-attribute 'mode-line-inactive :family)
                  (face-attribute 'mode-line-inactive :box)
                  )))
    (setq awesome-tray-mode-line-default-height (face-attribute 'mode-line :height))

    (comment
     ;; Disable mode line.
     (set-face-attribute 'mode-line nil
                         :foreground awesome-tray-mode-line-active-color
                         :background awesome-tray-mode-line-active-color
                         :height 0.1
                         :box nil)
     (set-face-attribute 'mode-line-inactive nil
                         :foreground awesome-tray-mode-line-inactive-color
                         :background awesome-tray-mode-line-inactive-color
                         :height 0.1
                         :box nil
                         :inherit 'unspecified))
    ;; Add update timer.
    (setq awesome-tray-timer
          (run-with-timer 0 awesome-tray-refresh-idle-delay 'awesome-tray-show-info))
    (add-hook 'focus-in-hook 'awesome-tray-show-info)
    (setq awesome-tray-active-p t))

  (awesome-tray-mode 1))
