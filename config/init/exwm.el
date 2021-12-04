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
      "this code block has the same effect with `exwm-config-misc'"
      (toggle-scroll-bar -1)
      (menu-bar-mode -1)
      (tool-bar-mode -1)
      (fringe-mode 1))
    (progn
      (require 'exwm)
      (require 'exwm-config))

    (defun exwm-config-my-base ()
      "This is modifed from `exwm-config-example' or `exwm-config-default'"

      ;; Set the initial workspace number.
      (unless (get 'exwm-workspace-number 'saved-value)
        (setq exwm-workspace-number 1))
      ;; Make class name the buffer name
      (add-hook 'exwm-update-class-hook
                (lambda ()
                  (exwm-workspace-rename-buffer exwm-class-name)))
      ;; Global keybindings.
      (unless (get 'exwm-input-global-keys 'saved-value)
        (setq exwm-input-global-keys
              `(
                ;; 's-r': Reset (to line-mode).
                ([?\s-r] . exwm-reset)
                ;; 's-w': Switch workspace.
                ([?\s-w] . exwm-workspace-switch)
                ;; 's-&': Launch application.
                ([?\s-&] . (lambda (command)
                             (interactive (list (read-shell-command "$ ")))
                             (start-process-shell-command command nil command)))
                ;; 's-N': Switch to certain workspace.
                ,@(mapcar (lambda (i)
                            `(,(kbd (format "s-%d" i)) .
                              (lambda ()
                                (interactive)
                                (exwm-workspace-switch-create ,i))))
                          (number-sequence 0 9)))))
      ;; Line-editing shortcuts
      (unless (get 'exwm-input-simulation-keys 'saved-value)
        (setq exwm-input-simulation-keys
              '(([?\C-b] . [left])
                ([?\C-f] . [right])
                ([?\C-p] . [up])
                ([?\C-n] . [down])
                ([?\C-a] . [home])
                ([?\C-e] . [end])
                ([?\M-v] . [prior])
                ([?\C-v] . [next])
                ([?\C-d] . [delete])
                ([?\C-k] . [S-end delete]))))
      ;; Enable EXWM
      (comment (exwm-enable))
      ;; Configure Ido
      (comment (exwm-config-ido))
      ;; Other configurations
      (exwm-config-misc)))

  (defun exwm-config-mine ()
    (exwm-simple-init)
    (exwm-config-my-base)

    (progn
      ;; interactive functions

      (defun exwm-get-pid-of-buffer (buffer-or-name)
        (interactive "bBuffer name: ")
        (let* ((buf (or buffer-or-name (current-buffer)))
               (id (exwm--buffer->id (get-buffer buf)))) ; ID of X window being displayed
          (message
           (if id
               (slot-value (xcb:+request-unchecked+reply
                               exwm--connection
                               (make-instance 'xcb:ewmh:get-_NET_WM_PID :window id))
                           'value)
             (user-error "Target buffer %S is not an X window managed by EXWM!"
                         buf))))))

    (comment
     ;; enable key-chord and hydra
     ;; https://www.reddit.com/r/emacs/comments/8yf6dx/key_chords_in_exwm/
     (setq exwm-input-line-mode-passthrough t))

    (progn
      ;; normal emacs global commands
      ;; (global-set-key (kbd "s-e") 'some-command)
      )

    (comment
     ;; key chord doesn't work
     ;; commands for exwm globally

     (comment
      ;; these keys enable "C-q" prefix
      (exwm-input-set-key (kbd "C-q o") (make-repeatable-command 'other-window))
      (exwm-input-set-key (kbd "C-q O") (make-repeatable-command 'other-window-backwards)))
     (comment (exwm-input-set-key (kbd "C-t") tab-prefix-map))

     (comment
      ;; key chord doesn't work
      (progn
        (key-chord-define global-map "qd" 'ctl-x-map)
        (exwm-input-set-key [(aref "qd" 0)] 'ctl-x-map)
        (exwm-input-set-key [(aref "qd" 1)] 'ctl-x-map))
      (progn
        (key-chord-define global-map "q3" 'tab-prefix-map)
        (exwm-input-set-key [(aref "q3" 0)] 'tab-prefix-map)
        (exwm-input-set-key [(aref "q3" 1)] 'tab-prefix-map)))
     )

    (progn
      ;; commands for exwm globally
      ;; https://github.com/ch11ng/exwm/wiki#global-key-bindings
      ;;
      ;; global keys are defined in `exwm-input-global-keys'
      ;;
      ;; direct customization of `exwm-input-global-keys' should be done before calling `exwm-enable'

      (progn
        (defun exwm-other-workspace (count)
          (interactive "p")
          (exwm-workspace-switch (% (+ exwm-workspace-current-index 1)
                                    (exwm-workspace--count))))

        (defun exwm-other-workspace-backwards () (interactive) (exwm-other-workspace -1))

        (let ((map (make-sparse-keymap)))
	  (define-key map (kbd "o") (make-repeatable-command 'exwm-other-workspace))
	  (define-key map (kbd "O") (make-repeatable-command 'exwm-other-workspace-backwards))
          (define-key map (kbd "ㅐ") (make-repeatable-command 'exwm-other-workspace))
          (define-key map (kbd "ㅒ") (make-repeatable-command 'exwm-other-workspace-backwards))
          (define-key map (kbd "s") 'exwm-workspace-switch)
          (define-key map (kbd "0") 'exwm-workspace-delete)
          (define-key map (kbd "8") 'exwm-workspace-add)

	  (defvar exwm-my-workspace-prefix-map map
	    "Keymap for workspace related commands."))

        (fset 'exwm-my-workspace-prefix-map exwm-my-workspace-prefix-map)
        (exwm-input-set-key (kbd "s-w") 'exwm-my-workspace-prefix-map))

      (comment (exwm-input-set-key (kbd "s-q") 'ctl-x-map))
      (comment (exwm-input-set-key (kbd "s-e") 'tab-prefix-map))
      (exwm-input-set-key (kbd "C-3") 'tab-prefix-map))

    (progn
      ;; prefix keys for line-mode are defined in `exwm-input-prefix-keys'
      ;; https://github.com/ch11ng/exwm/wiki#global-key-bindings
      )

    (comment
     ;; key chord doesn't work
     ;; local key bindings
     (comment (define-key exwm-mode-map (kbd "C-q C-w") 'exwm-input-send-next-key))
     (comment
      ;; key chord doesn't work
      (progn
        (exwm-input-set-key [(aref "qw" 0)] 'ctl-x-map)
        (exwm-input-set-key [(aref "qw" 1)] 'ctl-x-map))
      (progn
        (exwm-input-set-key [(aref "qe" 0)] 'tab-prefix-map)
        (exwm-input-set-key [(aref "qe" 1)] 'tab-prefix-map))))

    (progn
      ;; local key bindings
      ;; https://github.com/ch11ng/exwm/wiki#local-key-bindings
      (define-key exwm-mode-map (kbd "C-;") 'exwm-input-send-next-key)
      (define-key exwm-mode-map (kbd "C-q") 'ctl-x-map)
      (define-key exwm-mode-map (kbd "s-!") 'shell-command))

    (progn
      ;; simulation keys
      ;; https://github.com/ch11ng/exwm/wiki#simulation-keys

      (setq exwm-input-simulation-keys
            '(([?\C-b] . [left])
              ([?\C-f] . [right])
              ([?\C-p] . [up])
              ([?\C-n] . [down])
              ([?\C-a] . [home])
              ([?\C-e] . [end])
              ([?\M-v] . [prior])
              ([?\C-v] . [next])
              ([?\C-d] . [delete])
              ([?\C-k] . [S-end C-c delete]) ; updated

              ;; the below is newly added
              ([?\C-w] . [?\C-x])
              ([?\C-y] . [?\C-v])
              ([?\M-w] . [?\C-c])
              ;; ([C-S-f] . [S-right])
              ;; ([C-S-b] . [S-left])
              ;; ([C-F] . [S-right])
              ;; ([C-B] . [S-left])
              ([?\M-f] . [C-right])
              ([?\M-b] . [C-left])
              ;; ([M-S-f] . [C-S-right])
              ;; ([M-S-b] . [C-S-left])
              ;; ([M-F] . [C-S-right])
              ;; ([M-B] . [C-S-left])

              ([?\C-/] . [?\C-y])
              )))

    (progn
      ;; volume
      (require 'volume nil t))

    (comment
     ;; system tray
     ;; https://github.com/ch11ng/exwm/wiki#system-tray
     (require 'exwm-systemtray)
     (exwm-systemtray-enable)

     ;; TODO: use polybar
     ;; https://www.youtube.com/watch?v=usCfMstCZ7E
     )

    (comment
      ;; polybar
      ;; https://github.com/daviwil/emacs-from-scratch/blob/master/show-notes/Emacs-Desktop-05.org
      ;; https://www.youtube.com/watch?v=bzRF8TlKQhY
      ;;
      ;; NOTE: Disable exwm-systemtray before restarting Emacs so that the tray works!

      (defvar efs/polybar-process nil
        "Holds the process of the running Polybar instance, if any")

      (defun efs/kill-panel ()
        (interactive)
        (when efs/polybar-process
          (ignore-errors
            (kill-process efs/polybar-process)))
        (setq efs/polybar-process nil))

      (defun efs/start-panel ()
        (interactive)
        (efs/kill-panel)
        (setq efs/polybar-process (start-process-shell-command "polybar" nil "polybar panel")))

      ;; Start the Polybar panel
      (efs/start-panel))

    (comment
     ;; enable input method
     ;; https://github.com/ch11ng/exwm/wiki#input-method

     ;; using xim input
     (require 'exwm-xim)
     (exwm-xim-enable)
     (push ?\C-\\ exwm-input-prefix-keys) ;; use Ctrl + \ to switch input method)
     )

    (progn
     ;; Enabling EXWM should be the last
     (exwm-enable)))

  (exwm-config-mine))
