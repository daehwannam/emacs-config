
(comment (load "~/.emacs.d/config/init/theme.el"))

(comment
 (when (file-exists-p "~/.emacs.d/config/init/eyebrowse.el")
   (load "~/.emacs.d/config/init/eyebrowse.el")))

(when (and exwm-cmd-arg-passed
           (machine-config-get-first 'exwm-multiple-monitor-layout-type))
    ;; this prevent wrong window deployment when
    ;; `exwm-base-input-simulation-keys' has many commands
    (exwm-init))
