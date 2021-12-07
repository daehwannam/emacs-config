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
      (progn
        ;; workspace start number
        ;; https://www.reddit.com/r/emacs/comments/arqg6z/comment/egp2e1u/?utm_source=share&utm_medium=web2x&context=3

        (setq exwm-my-workspace-start-number 1)
        (assert (member exwm-my-workspace-start-number '(0 1)))  ; should be 0 or 1
        (setq exwm-workspace-index-map
              (lambda (index) (number-to-string (+ exwm-my-workspace-start-number index))))
        (dotimes (i 10)
          (exwm-input-set-key (kbd (format "s-%d" i))
                              `(lambda ()
                                 (interactive)
                                 (exwm-workspace-switch-create
                                  (% (+ ,i 10 (- exwm-my-workspace-start-number)) 10))))))
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
                          (number-sequence (+ 0 exwm-my-workspace-start-number 
                                           (% (+ 9 exwm-my-workspace-start-number) 10)))))))
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

      (progn
        ;; cursor and mouse config
        ;; https://github.com/daviwil/emacs-from-scratch/blob/5ebd390119a48cac6258843c7d5e570f4591fdd4/show-notes/Emacs-Desktop-04.org
        (setq exwm-workspace-warp-cursor t)
        (setq mouse-autoselect-window t)
        (setq focus-follows-mouse t))
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

    (progn
      ;; local key bindings
      ;; https://github.com/ch11ng/exwm/wiki#local-key-bindings
      (define-key exwm-mode-map (kbd "C-;") 'exwm-input-send-next-key)
      (define-key exwm-mode-map (kbd "C-q") 'ctl-x-map)
      (define-key exwm-mode-map (kbd "M-!") 'shell-command)
      (define-key exwm-mode-map (kbd "M-#") 'lookup-word-from-web-other-window-for-exwm))

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

    (progn
      ;; enable switching betwen buffers in other workspaces
      ;;
      ;; https://github.com/ch11ng/exwm/wiki#x-window-handling-among-workspaces
      (setq exwm-workspace-show-all-buffers t)
      (setq exwm-layout-show-all-buffers t))

    (when (machine-config-get-first 'exwm-multiple-monitor-type)
      ;; multiple monitor test
      ;; https://github.com/daviwil/emacs-from-scratch/blob/5ebd390119a48cac6258843c7d5e570f4591fdd4/show-notes/Emacs-Desktop-04.org
      (require 'exwm-randr)

      (pcase (machine-config-get-first 'exwm-multiple-monitor-type)
        (descartes-triple
         (progn
           ;; mapping workspace indices with monitors
           (setq exwm-randr-workspace-monitor-plist
                 '(0 "HDMI-1-1" 1 "DVI-I-1" 2 "HDMI-4"
                     3 "HDMI-1-1" 4 "DVI-I-1" 5 "HDMI-4"))
           ;; run xrandr
           (add-hook 'exwm-randr-screen-change-hook
                     (lambda ()
                       (start-process-shell-command
                        "xrandr" nil
                        "xrandr --output DVI-I-1 --auto \
                                --output HDMI-1-1 --auto --left-of DVI-I-1 \
                                --output HDMI-4 --auto --right-of DVI-I-1")))))
        (t (error "Unknown monitor configuration")))

      (comment
       (defun exwm-change-screen-hook ()
         (let ((xrandr-output-regexp "\n\\([^ ]+\\) connected ")
               default-output)
           (with-temp-buffer
             (call-process "xrandr" nil t nil)
             (goto-char (point-min))
             (re-search-forward xrandr-output-regexp nil 'noerror)
             (setq default-output (match-string 1))
             (forward-line)
             (if (not (re-search-forward xrandr-output-regexp nil 'noerror))
                 (call-process "xrandr" nil nil nil "--output" default-output "--auto")
               (call-process
                "xrandr" nil nil nil
                "--output" (match-string 1) "--primary" "--auto"
                "--output" default-output "--off")
               (setq exwm-randr-workspace-monitor-plist (list 0 (match-string 1)))))))

       (add-hook 'exwm-randr-screen-change-hook 'exwm-change-screen-hook))

      (exwm-randr-enable))

    (progn
      ;; Enabling EXWM should be the last
      (exwm-enable))

    (comment
      ;; docking and undocking config
      ;;
      ;; https://github.com/daviwil/emacs-from-scratch/blob/5ebd390119a48cac6258843c7d5e570f4591fdd4/show-notes/Emacs-Desktop-04.org#docking-and-undocking

      (defun efs/update-displays ()
        (efs/run-in-background "autorandr --change --force")
        (message "Display config: %s"
                 (string-trim (shell-command-to-string "autorandr --current")))
        (efs/set-wallpaper))

      ;; React to display connectivity changes, do initial display update
      (add-hook 'exwm-randr-screen-change-hook #'efs/update-displays)
      (efs/update-displays)))

  (exwm-config-mine))
