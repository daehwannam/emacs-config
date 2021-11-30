(progn
  (defvar exwm-cmd-arg-passed nil)

  (let ((remaining-cmd-args (cdr command-line-args)))
    ;; originally '(cdr command-line-args) is passed into `command-line-1'
    (setq exwm-cmd-arg-passed (member "--exwm" remaining-cmd-args)))

  ;; exwm setup
  (defun exwm-cmd-arg-handler (switch)
    ;; do nothing
    )

  (add-to-list 'command-switch-alist '("--exwm" . exwm-cmd-arg-handler)))

(when exwm-cmd-arg-passed
  (let ((exwm-enabled (machine-config-get-first 'exwm-enabled)))
    ;; conditional eval
    (when exwm-enabled))

  (defun exwm-simple-init ()
    (progn
      ;; disable scroll-bar, menu-bar, tool-bar
      (toggle-scroll-bar -1)
      (menu-bar-mode -1)
      (tool-bar-mode -1))
    (progn
      (require 'exwm)
      (require 'exwm-config)))

  (defun exwm-config-mine ()
    (exwm-simple-init)
    (progn
      (exwm-config-example)  ; same with (exwm-config-default)
      (ido-mode nil)

      (comment
        ;; enable key-chord and hydra
        ;; https://www.reddit.com/r/emacs/comments/8yf6dx/key_chords_in_exwm/
        (setq exwm-input-line-mode-passthrough t))

      (progn
        ;; global commands
        (global-set-key (kbd "C-t") tab-prefix-map)
        )

      (comment
       ;; commands for exwm globally
       ;;
       ;; (exwm-input-set-key (kbd "C-q o") (make-repeatable-command 'other-window))
       ;; (exwm-input-set-key (kbd "C-q O") (make-repeatable-command 'other-window-backwards))
       ;; TODO: use 'exwm-input-global-keys for key-chords
       )

      (progn
        ;; commands for line-mode
        (comment (define-key exwm-mode-map (kbd "C-q C-w") 'exwm-input-send-next-key))
        (define-key exwm-mode-map (kbd "C-;") 'exwm-input-send-next-key)
        ;; (key-chord-define exwm-mode-map "qe" 'tab-prefix-map)
        )

      ))

  (exwm-config-mine))
