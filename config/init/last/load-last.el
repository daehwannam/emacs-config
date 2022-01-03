
(comment (load "~/.emacs.d/config/init/theme.el"))

(comment
 (when (file-exists-p "~/.emacs.d/config/init/eyebrowse.el")
   (load "~/.emacs.d/config/init/eyebrowse.el")))

(when (and exwm-cmd-arg-passed
           (machine-config-get-first 'exwm-multiple-monitor-layout-type))
    ;; this prevent wrong window deployment when
    ;; `exwm-base-input-simulation-keys' has many commands
    (exwm-init))

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

    (defvar my-ctl-c-map map
      "Keymap simulating C-c bindings")

    (fset 'my-ctl-c-map my-ctl-c-map)))
