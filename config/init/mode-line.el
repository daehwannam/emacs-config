
(when (or t (comment (member 'exwm-setup dhnam/machine-options)))
  (comment (add-to-list `load-path (expand-file-name (concat dhnam/emacs-root-dir "config/package/awesome-tray/"))))
  (require 'awesome-tray)

  (progn
    ;; customize
    (setq awesome-tray-active-modules
          (if dhnam/exwm-cmd-line-arg-passed
              '("ewg" "tab-bar" "battery" "date")
            (progn
              ;; '("frame" "tab-bar" "battery" "date")
              '("tab-bar" "battery" "date"))))

    (defun-override awesome-tray-module-date-info ()
      (comment (format-time-string "%m-%d %H:%M %a"))
      (format-time-string "%H:%M %a %m-%d-%Y")))
  (progn
    (defun dhnam/awesome-tray-my-module-battery-info ()
      (let ((current-seconds (awesome-tray-current-seconds)))
        (if (> (- current-seconds awesome-tray-battery-status-last-time) awesome-tray-battery-update-duration)
            (let* ((battery-info (funcall battery-status-function))
                   (battery-type (battery-format "%L" battery-info))
                   (battery-load (concat (battery-format "%p" battery-info) "%"))
                   battery-status)
              (setq awesome-tray-battery-status-last-time current-seconds)

              ;; Short battery type.
              (cond ((string-equal battery-type "on-line")
                     (setq battery-type "ON")
                     (setq battery-status (battery-format "-%p%%" battery-info)))
                    ((string-equal battery-type "off-line")
                     (setq battery-type "OFF")
                     (setq battery-status (battery-format "-%p%% %t" battery-info))))

              ;; Update battery cache.
              (setq awesome-tray-battery-status-cache (concat battery-load battery-status)))
          awesome-tray-battery-status-cache)))

    (push '("battery" . (dhnam/awesome-tray-my-module-battery-info awesome-tray-module-battery-face))
          awesome-tray-module-alist)

    (defun dhnam/awesome-tray-my-tab-bar-info ()
      (let* ((tabs (funcall tab-bar-tabs-function))
             (current-idx (tab-bar--current-tab-index tabs))
             (current-tab-num (1+ current-idx))
             (num-tabs (length tabs)))
        (format "%d/%d" current-tab-num num-tabs)))

    (push '("tab-bar" . (dhnam/awesome-tray-my-tab-bar-info awesome-tray-module-parent-dir-face))
          awesome-tray-module-alist)

    (defun dhnam/awesome-tray-my-frame-info ()
      (let ((frames (frame-list))
            (curr-frame (selected-frame)))
        (format "%d/%d" (length (member curr-frame frames)) (length (frame-list)))))

    (push '("frame" . (dhnam/awesome-tray-my-frame-info awesome-tray-module-awesome-tab-face))
          awesome-tray-module-alist)

    (defun dhnam/awesome-tray-my-ewg-info ()
      (let ((group-number (1+ (ewg/get-group-index exwm-workspace-current-index)))
            (num-total-groups (/ (exwm-workspace--count) ewg/max-group-size)))
        (format "%d/%d" group-number num-total-groups)))

    (push '("ewg" . (dhnam/awesome-tray-my-ewg-info awesome-tray-module-awesome-tab-face))
          awesome-tray-module-alist))

  (unless (display-graphic-p)
    ;; prevent to make additional line in terminal emacs
    (advice-add 'awesome-tray-get-frame-width :filter-return #'1-))

  (defun-override awesome-tray-enable ()
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


(require 'hide-mode-line nil t)

(provide 'init-mode-line)
