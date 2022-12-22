
(comment (load "~/.emacs.d/config/init/theme.el"))

(comment
  (when (file-exists-p "~/.emacs.d/config/init/eyebrowse.el")
    (load "~/.emacs.d/config/init/eyebrowse.el")))

(when (and dhnam/exwm-cmd-arg-passed
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
    (mapcar #'global-unset-key (where-is-internal 'other-frame-backwards))
    (mapcar #'global-unset-key (where-is-internal 'other-frame-repeat))
    (mapcar #'global-unset-key (where-is-internal 'other-frame-backwards-repeat))))

(progn
  (defun define-key-from-C-c (map key)
    (define-key map (kbd key) (key-binding (kbd (concat "C-c " key)))))

  (let ((map (make-sparse-keymap)))
    ;; hydra-buffer-move
    (define-key-from-C-c map "m i")
    (define-key-from-C-c map "m k")
    (define-key-from-C-c map "m j")
    (define-key-from-C-c map "m l")

    ;; hydra-buffer-shift
    (define-key-from-C-c map "s i")
    (define-key-from-C-c map "s k")
    (define-key-from-C-c map "s j")
    (define-key-from-C-c map "s l")

    ;; winner mode
    (define-key-from-C-c map "<left>")
    (define-key-from-C-c map "<right>")

    ;; er/expand-region
    (define-key-from-C-c map ".")

    (defvar my-ctl-c-map map
      "Keymap simulating C-c bindings")

    (fset 'my-ctl-c-map my-ctl-c-map)

    (comment
      (key-chord-define-global "qd" 'my-ctl-c-map))))
