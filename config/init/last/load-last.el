
(comment (load "~/.emacs.d/config/init/theme.el"))

(comment
  (when (file-exists-p "~/.emacs.d/config/init/eyebrowse.el")
    (load "~/.emacs.d/config/init/eyebrowse.el")))

(when (and dhnam/exwm-cmd-line-arg-passed
           (let ((physical-monitor-names (dhnam/machine-config-get-first 'exwm-physical-monitor-names)))
             (and physical-monitor-names (> (length physical-monitor-names) 1))))
  (progn
    ;; this prevent wrong frame deployment when
    ;; `exwm-base-input-simulation-keys' has many commands
    (comment (exwm-randr-refresh))
    (comment (exwm-init)))

  (progn
    ;; unblind some keys
    (mapcar #'global-unset-key (where-is-internal 'make-frame-command))
    (mapcar #'global-unset-key (where-is-internal 'delete-frame))
    (mapcar #'global-unset-key (where-is-internal 'delete-other-frames))
    (mapcar #'global-unset-key (where-is-internal 'other-frame))
    (mapcar #'global-unset-key (where-is-internal 'dhnam/other-frame-backwards))
    (mapcar #'global-unset-key (where-is-internal 'other-frame-repeat))
    (mapcar #'global-unset-key (where-is-internal 'dhnam/other-frame-backwards-repeat))))

(progn
  (defun dhnam/define-key-from-C-c (map key)
    (define-key map (kbd key) (key-binding (kbd (concat "C-c " key)))))

  (let ((map (make-sparse-keymap)))
    ;; hydra-buffer-move
    (dhnam/define-key-from-C-c map "m i")
    (dhnam/define-key-from-C-c map "m k")
    (dhnam/define-key-from-C-c map "m j")
    (dhnam/define-key-from-C-c map "m l")

    ;; hydra-buffer-shift
    (dhnam/define-key-from-C-c map "s i")
    (dhnam/define-key-from-C-c map "s k")
    (dhnam/define-key-from-C-c map "s j")
    (dhnam/define-key-from-C-c map "s l")

    ;; winner mode
    (dhnam/define-key-from-C-c map "<left>")
    (dhnam/define-key-from-C-c map "<right>")

    ;; er/expand-region
    (dhnam/define-key-from-C-c map ".")

    (defvar dhnam/ctl-c-map map
      "Keymap simulating C-c bindings")

    (fset 'dhnam/ctl-c-map dhnam/ctl-c-map)

    (comment
      (key-chord-define-global "qd" 'dhnam/ctl-c-map))))

(provide 'init-load-last)
