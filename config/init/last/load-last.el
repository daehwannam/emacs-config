
(comment (load "~/.emacs.d/config/init/theme.el"))

(comment
  (when (file-exists-p "~/.emacs.d/config/init/eyebrowse.el")
    (load "~/.emacs.d/config/init/eyebrowse.el")))

(when (and exwm-cmd-arg-passed
           (machine-config-get-first 'exwm-multiple-monitor-layout-type))
  (progn
    ;; this prevent wrong frame deployment when
    ;; `exwm-base-input-simulation-keys' has many commands
    (comment (exwm-randr-refresh))
    (comment (exwm-init)))

  (progn
    ;; unblind some keys
    (mapcar #'global-unset-key (where-is-internal 'make-frame-command))
    (mapcar #'global-unset-key (where-is-internal 'delete-frame))
    (mapcar #'global-unset-key (where-is-internal 'delete-other-frames))))

(progn
  (defun define-key-from-C-c (map key)
    (define-key map (kbd key) (key-binding (kbd (concat "C-c " key)))))

  (let ((map (make-sparse-keymap)))
    ;; hydra-buffer-move
    (define-key-from-C-c map "m i")
    (define-key-from-C-c map "m k")
    (define-key-from-C-c map "m j")
    (define-key-from-C-c map "m l")

    ;; winner mode
    (define-key-from-C-c map "<left>")
    (define-key-from-C-c map "<right>")

    ;; er/expand-region
    (define-key-from-C-c map ".")

    (defvar my-ctl-c-map map
      "Keymap simulating C-c bindings")

    (fset 'my-ctl-c-map my-ctl-c-map)

    (key-chord-define-global "qd" 'my-ctl-c-map)))
