
(defvar dhnam/exwm-cmd-line-arg-passed
  (cmd-line-arg/register-then-get "--exwm" nil))

(when dhnam/exwm-cmd-line-arg-passed
  (defun dhnam/exwm-simple-frame-init ()
    (progn
      "this code block has the same effect with `exwm-config-misc'"

      (comment
        ;; disable gui config
        ;;
        ;; similar effect with (exwm-config-misc)
        (menu-bar-mode -1)
        (tool-bar-mode -1)
        (scroll-bar-mode -1)
        (comment (fringe-mode 1)))

      (progn
        ;; window-divider config
        (setq window-divider-default-right-width 1)
        (window-divider-mode 1)))

    (progn
      ;; auto fullscreen for frames
      (add-to-list 'default-frame-alist '(fullscreen . maximized)))

    (progn
      ;; cursor and mouse config
      ;; https://github.com/daviwil/emacs-from-scratch/blob/5ebd390119a48cac6258843c7d5e570f4591fdd4/show-notes/Emacs-Desktop-04.org
      (let ((focus-and-select nil))
        (setq exwm-workspace-warp-cursor (not focus-and-select))
        (progn
          (setq mouse-autoselect-window focus-and-select)
          (setq focus-follows-mouse focus-and-select)))


      (start-process-shell-command "unclutter" nil "unclutter -idle 3 -root")
      (comment (start-process-shell-command "unclutter" nil "nohup unclutter -idle 2 &"))))

  (defun dhnam/exwm-config-base ()
    "This is modifed from `exwm-config-example' or `exwm-config-default'"

    (progn
      (require 'exwm)
      (require 'exwm-config))

    ;; set the initial workspace number.
    (unless (get 'exwm-workspace-number 'saved-value)
      (setq exwm-workspace-number 1))

    ;; make class name the buffer name
    (add-hook 'exwm-update-class-hook
              (lambda () (exwm-workspace-rename-buffer exwm-class-name)))

    (comment
      ;; Configure Ido
      (exwm-config-ido)))

  (defun dhnam/exwm-config-mine ()
    (dhnam/exwm-simple-frame-init)
    (dhnam/exwm-config-base)

    (comment
      (defun efs/exwm-update-title ()
        (pcase exwm-class-name
          ("Firefox" (exwm-workspace-rename-buffer (format "Firefox: %s" exwm-title)))
          ("Google-chrome" (exwm-workspace-rename-buffer (format "Google-chrome: %s" exwm-title)))))

      ;; When window title updates, use it to set the buffer name
      (add-hook 'exwm-update-title-hook #'efs/exwm-update-title))

    (progn
      ;; Application title update
      ;; https://github.com/ch11ng/exwm/issues/198#issuecomment-249723369
      ;;
      ;; similar methods:
      ;; https://github.com/daviwil/emacs-from-scratch/blob/39f63fe133cd4c41e13bbd1551c6517162851411/show-notes/Emacs-Desktop-03.org#customizing-buffer-name-based-on-window-title
      ;; https://www.youtube.com/watch?v=HGGU5Zvljj8

      (defvar dhnam/exwm-buffer-name-joint ": ")

      (defun dhnam/exwm-rename-buffer ()
        (interactive)
        (exwm-workspace-rename-buffer
         (concat exwm-class-name dhnam/exwm-buffer-name-joint
                 (if (<= (length exwm-title) 50) exwm-title
                   (concat (substring exwm-title 0 49) "...")))))

      ;; Add these hooks in a suitable place (e.g., as done in exwm-config-default)
      (add-hook 'exwm-update-class-hook 'dhnam/exwm-rename-buffer)
      (add-hook 'exwm-update-title-hook 'dhnam/exwm-rename-buffer))

    (require 'dhnam-exwm)

    (progn
      ;; workspace switching functions
      (progn
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "r") 'exwm-reset)
          (define-key map (kbd "t") 'exwm-floating-toggle-floating)
          (define-key map (kbd "f") 'dhnam/exwm-layout-set-fullscreen)

          (define-key map (kbd "j") 'exwm-input-grab-keyboard)
          (define-key map (kbd "k") 'exwm-input-release-keyboard)

	      (define-key map (kbd "o") (make-repeatable-command 'dhnam/exwm-other-workspace))
	      (define-key map (kbd "O") (make-repeatable-command 'dhnam/exwm-other-workspace-backwards))
          (define-key map (kbd "ㅐ") (make-repeatable-command 'dhnam/exwm-other-workspace))
          (define-key map (kbd "ㅒ") (make-repeatable-command 'dhnam/exwm-other-workspace-backwards))
          (define-key map (kbd "s") 'exwm-workspace-switch)
          (define-key map (kbd "0") 'exwm-workspace-delete)
          (define-key map (kbd "8") 'exwm-workspace-add)

          (define-key map (kbd "h") 'hide-mode-line-mode)

          (comment
            ;; not working
            (advice-add
             ;; to fix a bug which doesn't fill screen fully when new workspace is created
             'exwm-workspace-add
             :after
             #'(lambda (&rest args) (exwm-layout--refresh-workspace (selected-frame)))
             '((name . "exwm-workspace-add-fullscreen-advice"))))
	      (defvar dhnam/exwm-workspace-prefix-map map
	        "Keymap for workspace related commands."))
        (fset 'dhnam/exwm-workspace-prefix-map dhnam/exwm-workspace-prefix-map)
        (comment (exwm-input-set-key (kbd "s-w") 'dhnam/exwm-workspace-prefix-map)))

      (comment (exwm-input-set-key (kbd "s-q") 'ctl-x-map))
      (comment (exwm-input-set-key (kbd "s-e") 'tab-prefix-map))
      (comment (exwm-input-set-key (kbd "C-3") 'tab-prefix-map)))

    (comment
      ;; disable fullscreen
      (cl-defun dhnam/exwm-layout-set-fullscreen (&optional id)
        "Make window ID fullscreen."
        (interactive)
        ;; (exwm-layoaut-unset-fullscreen id)
        ;; (exwm-layout--show id)
        ))

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

    (when (dhnam/machine-config-get-first 'exwm-physical-monitor-names)
      ;; multiple monitor setting

      (progn
        ;; workspace-group functions
        (require 'exwm-workspace-group)

        (comment
          ;; xrandr-update functions
          (defun ewg/xrandr-dual-monitor-mirror-update ()
            (assert (= (length ewg/monitor-names) 2))
            (start-process-shell-command
             "xrandr" nil
             (format "$ xrandr --output %s --mode 2560x1440 --scale 1x1
                               --output %s --same-as %s --mode 1920x1080 --scale 1.333x1.333"
                     (nth 0 ewg/monitor-names)))
            (progn
              ;; this prevent wrong frame deployment when
              ;; `exwm-base-input-simulation-keys' has many commands
              (exwm-randr-refresh))))

        (ewg/init (dhnam/machine-config-get-first 'exwm-physical-monitor-names))

        (let ((map dhnam/exwm-workspace-prefix-map))
          ;; update `dhnam/exwm-workspace-prefix-map'
          (define-key map (kbd "o") (make-repeatable-command 'ewg/other-workspace-in-group))
          (define-key map (kbd "O") (make-repeatable-command 'ewg/other-workspace-in-group-backwards))

          (define-key map (kbd "w") 'ewg/swap-current-group-number)

          (define-key map (kbd "8") 'ewg/add-group)
          (define-key map (kbd "9") 'ewg/delete-other-groups)
          (define-key map (kbd "0") 'ewg/delete-current-group))))

    (when (package-installed-p 'volume)
      (comment
        (defhydra hydra-volume ()
          "volume"
          ("q" nil "quit")
          ("+" volume-raise-10)
          ("=" volume-raise-10)
          ("-" volume-lower-10)
          ("s" volume-set)
          ("0" volume-set-to-0%))
        (define-key dhnam/exwm-workspace-prefix-map (kbd "v") 'hydra-volume/body))
      (define-key dhnam/exwm-workspace-prefix-map (kbd "v") 'volume-set))

    (require 'dhnam-application)
    (require 'dhnam-screenshot)
    (require 'dhnam-web-browser)

    (progn
      ;; application commands
      (let ((map (make-sparse-keymap)))
        (comment (define-key map (kbd "w") 'dhnam/app-command-open-web-browser))
        (comment (define-key map (kbd "W") 'dhnam/app-command-open-web-browser-private))
        (comment (define-key map (kbd "q") 'dhnam/app-command-query-to-browser))
        ;; (define-key map (kbd "W") 'dhnam/app-command-open-web-browser-incognito)

        (define-key map (kbd "q") 'dhnam/app-command-query-to-firefox)
        (define-key map (kbd "Q") 'dhnam/app-command-query-to-firefox-private)
        (define-key map (kbd "w") 'dhnam/app-command-open-bookmark-in-firefox)
        (define-key map (kbd "W") 'dhnam/app-command-open-bookmark-in-firefox-private)

        (define-key map (kbd "c") 'dhnam/app-command-open-google-chrome)
        (define-key map (kbd "C") 'dhnam/app-command-open-google-chrome-incognito)
        (comment (define-key map (kbd "f") 'dhnam/app-command-open-firefox))
        (comment (define-key map (kbd "F") 'dhnam/app-command-open-firefox-private))
        (define-key map (kbd "e") 'dhnam/app-command-open-terminal-emulator)
        (comment (define-key map (kbd "e") 'dhnam/gui-terminal))
        (define-key map (kbd "<print>") 'dhnam/app-command-open-flameshot-gui)
        (define-key map (kbd "<S-print> b") 'dhnam/screenshot-video-selection-start)
        (define-key map (kbd "<S-print> e") 'dhnam/screenshot-video-stop)

	    (defvar dhnam/exwm-command-prefix-map map
	      "Keymap for application related commands."))
      (fset 'dhnam/exwm-command-prefix-map dhnam/exwm-command-prefix-map))

    (progn
      ;; Prevent to destroy EXWM window positions when `counsel-switch-buffer' preview buffers
      ;; https://www.reddit.com/r/emacs/comments/ixxga9/comment/grmw2mr/?utm_source=share&utm_medium=web2x&context=3

      (defvar fw/ivy-last-buffer nil)
      (defvar fw/ivy-last-window nil)

      (defun fw/unwind-exwm-buffer-p (current-buffer)
        "Need to unwind iff `fw/ivy-last-buffer' was displayed,\
    `CURRENT-BUFFER' is different, and `fw/ivy-last-buffer' is in `exwm-mode'."
        (and
         fw/ivy-last-buffer
         fw/ivy-last-window
         (not (equal fw/ivy-last-buffer current-buffer))
         (eq (with-current-buffer fw/ivy-last-buffer major-mode) 'exwm-mode)))

      (defun fw/unwind-exwm-buffer (&optional buffer window)
        "Switch back to EXWM buffer in the window it was taken from and update `fw/ivy-last-...' to BUFFER and WINDOW."
        (when (fw/unwind-exwm-buffer-p buffer)
          (set-window-buffer fw/ivy-last-window fw/ivy-last-buffer))
        (unless (equal fw/ivy-last-buffer buffer)
          (setq fw/ivy-last-window window))
        (setq fw/ivy-last-buffer buffer))

      (defun fw/counsel--switch-buffer-update-fn (original &rest args)
        "Wrap `(ORIGINAL &rest ARGS)' with `fw/ivy-unwind-last-buffer-if-exwm'."
        (let* ((buffer (get-buffer (ivy-state-current ivy-last)))
               (window (get-buffer-window buffer t)))
          (apply original args)
          (fw/unwind-exwm-buffer buffer window)))


      (advice-add 'counsel--switch-buffer-update-fn :around #'fw/counsel--switch-buffer-update-fn)
      (advice-add 'counsel--switch-buffer-unwind :after #'fw/unwind-exwm-buffer)

      (defadvice ivy-done (before fw/inhibit-unwind-exwm-buffer activate)
        "Inhibit unwinding of EXWM buffer iff it's selected."
        (when (fw/unwind-exwm-buffer-p nil)
          (setq fw/ivy-last-buffer nil
                fw/ivy-last-window nil))))

    (progn
      (require 'dhnam-counsel-for-exwm)

      (progn
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "d") 'dhnam/counsel-find-file-in-downloads)
          (define-key map (kbd "w") 'dhnam/kill-gc)
          (define-key map (kbd "k") 'dhnam/ivy-kill-marked)
          (define-key map (kbd "s") 'dhnam/switch-to-scratch-buffer)
          (define-key map (kbd "q") 'dhnam/eww-new)

          (define-key map (kbd "l") 'dhnam/open-primary-web-search-engine-list-file)
          (define-key map (kbd "m") 'dhnam/open-primary-web-bookmark-list-file)

          (defvar dhnam/exwm-extended-emacs-command-prefix-map map
	        "Keymap for emacs related commands."))

        (fset 'dhnam/exwm-extended-emacs-command-prefix-map dhnam/exwm-extended-emacs-command-prefix-map)))

    (progn
      (setq exwm-manage-configurations nil)
      (setq exwm-manage-configurations
            ;; former configurations have higher priorities
            (append
             (progn
               ;; disable line-mode for specific applications
               ;; https://www.reddit.com/r/emacs/comments/o6vzxz/comment/h2v5rn0/?utm_source=share&utm_medium=web2x&context=3
               '(((member exwm-class-name '("Emacs" "Gnome-terminal" "kitty" "qutebrowser" "Remote-viewer"))
	              char-mode t)))
             (progn
               ;; enable line-mode for specific applications
               ;; https://www.reddit.com/r/emacs/comments/o6vzxz/comment/h2v5rn0/?utm_source=share&utm_medium=web2x&context=3
               '(((member exwm-class-name
                          '(;; Browsers
                            "Firefox" "firefox" "Google-chrome" "google-chrome"
                            ;; Libreoffice
                            "Soffice" "libreoffice-calc"))
	              line-mode t)))
             (progn
               ;; start applications in char-mode by default
               ;; https://github.com/ch11ng/exwm/issues/411#issuecomment-379561414
               '((t char-mode t))))))

    (progn
      ;; miscellaneous commands

      (defun dhnam/exwm-reload-key-config ()
        (interactive)
        (call-process-shell-command (format "bash %s" (concat dhnam/emacs-root-dir "exwm/reload-key-config.sh")))))

    ;; (progn
    ;;   ;; run machine-specific config
    ;;   (start-process-shell-command
    ;;    "machine-specific-config" nil
    ;;    "bash"
    ;;    "/home/dhnam/.emacs.d/exwm/machine-config.sh"
    ;;    "/home/dhnam/.emacs.d/exwm/option.txt"))

    (progn
      ;; key bindings

      (progn
        ;; normal emacs global commands
        (comment (global-set-key (kbd "M-&") 'async-shell-command))
        (key-chord-define-global ";f" 'counsel-find-file)
        (global-set-key (kbd "C-x b") 'switch-to-buffer)
        (global-set-key (kbd "C-x M-b") 'ivy-switch-buffer)
        (global-set-key (kbd "C-x B") 'counsel-switch-buffer)
        (comment (key-chord-define-global "qj" 'ivy-switch-buffer))
        (key-chord-define-global "qj" 'dhnam/counsel-switch-buffer-excluding-exwm)
        (key-chord-define-global "qd" 'dhnam/exwm-workspace-prefix-map)

        (defhydra hydra-buffer-shift (global-map "C-c s")
          "buffer-shift"
          ("q" nil "quit")
          ("j" dhnam/buf-shift-left)
          ("l" dhnam/buf-shift-right)
          ("i" dhnam/buf-shift-up)
          ("k" dhnam/buf-shift-down))

        (comment
          (global-set-key (kbd "C-x 2") 'dhnam/exwm-split-window-below)
          (global-set-key (kbd "C-x 3") 'dhnam/exwm-split-window-right)

          (global-set-key (kbd "C-x 8") 'dhnam/exwm-split-window-below)
          (global-set-key (kbd "C-x 7") 'dhnam/exwm-split-window-right)))

      (comment
        ;; enable key-chord and hydra
        ;; https://www.reddit.com/r/emacs/comments/8yf6dx/key_chords_in_exwm/
        (comment (setq exwm-input-line-mode-passthrough t))
        (defun dhnam/toggle-exwm-input-line-mode-passthrough ()
          (setq exwm-input-line-mode-passthrough (not exwm-input-line-mode-passthrough))))


      (progn
        ;; commands for exwm globally
        ;;
        ;; https://github.com/ch11ng/exwm/wiki#global-key-bindings
        ;;
        ;; - global keys are defined in `exwm-input-global-keys'
        ;; - direct customization of `exwm-input-global-keys' should be done before calling `exwm-enable'

        (progn
          ;; workspace start number
          ;; https://www.reddit.com/r/emacs/comments/arqg6z/comment/egp2e1u/?utm_source=share&utm_medium=web2x&context=3

          (defvar dhnam/exwm-workspace-start-number 1)
          (setq ewg/workspace-start-number dhnam/exwm-workspace-start-number)
          (assert (member dhnam/exwm-workspace-start-number '(0 1))) ; should be 0 or 1
          (setq exwm-workspace-index-map
                (lambda (index) (number-to-string (+ dhnam/exwm-workspace-start-number index)))))

        (defvar exwm-environment-switch-create)

        (let ((physical-monitor-names
               (dhnam/machine-config-get-first 'exwm-physical-monitor-names)))
          (setq exwm-environment-switch-create
                (if (and physical-monitor-names (> (length physical-monitor-names) 1))
                    'ewg/switch-create-group
                  'exwm-workspace-switch-create)))

        (comment
          (dotimes (workspace-num 9)
            (lexical-let ((idx (% (+ workspace-num 10 (- dhnam/exwm-workspace-start-number)) 10)))
                         (define-key dhnam/exwm-workspapce-prefix-map (kbd (format "%d" workspace-num))
                           #'(lambda () (interactive)
                               (funcall exwm-environment-switch-create idx))))))

        (progn
          (progn
            (defun dhnam/exwm-make-environment-switch-create-specific (i)
              `(lambda () (interactive)
                 (funcall exwm-environment-switch-create
                          (% (+ ,i 10 (- dhnam/exwm-workspace-start-number)) 10)))))

          (comment
            ;; `lexical-let' is defined in `cl'
            (require 'cl)

            (defun dhnam/exwm-make-environment-switch-create-specific (i)
              (lexical-let ((i i))
                           (lambda ()
                             (interactive)
                             (funcall exwm-environment-switch-create
                                      (% (+ i 10 (- dhnam/exwm-workspace-start-number)) 10)))))))

        (progn
          (assert (not (get 'exwm-input-global-keys 'saved-value)))
          (fset 'help-map help-map)

          (setq exwm-input-global-keys
                (append
                 (list
                  ;; (cons (kbd "s-r") 'exwm-reset)
                  ;; (cons (kbd "s-t") 'exwm-floating-toggle-floating)

                  (cons (kbd "s-;") 'exwm-input-send-next-key)

                  (cons (kbd "s-&") 'dhnam/app-command-execute-shell)
                  (cons (kbd "s-m") 'dhnam/exwm-command-prefix-map)
                  (cons (kbd "s-d") 'dhnam/exwm-workspace-prefix-map)
                  (cons (kbd "s-'") 'dhnam/exwm-extended-emacs-command-prefix-map)

                  (cons (kbd "s-q") 'ctl-x-map)
                  (cons (kbd "s-x") '(lambda () (interactive) (funcall (key-binding (kbd "M-x")))))
                  (cons (kbd "s-X") '(lambda () (interactive) (funcall (key-binding (kbd "M-x")))))
                  (cons (kbd "s-w") 'tab-prefix-map)
                  (cons (kbd "s-e") 'dhnam/ctl-c-map) ; Use "s-e" as prefix key instead of "C-c" | https://emacs.stackexchange.com/a/64130
                  (cons (kbd "s-h") 'help-map)
                  (cons (kbd "s-u") 'universal-argument)

                  (cons (kbd "s-l") 'counsel-find-file)
                  (cons (kbd "s-k") 'kill-buffer)

                  (cons (kbd "s-j") 'dhnam/counsel-switch-buffer-excluding-exwm)
                  (cons (kbd "C-s-j") 'ivy-switch-buffer)
                  (cons (kbd "s-!") 'shell-command)

                  (cons (kbd "s-9") 'previous-buffer)
                  (cons (kbd "s-0") 'next-buffer)

                  (cons (kbd "s-i") 'dhnam/other-window-backwards)
                  (cons (kbd "s-o") 'other-window)
                  (cons (kbd "s-p") 'tab-previous)
                  (cons (kbd "s-n") 'tab-next))

                 (list
                  (cons (kbd "<S-s-up>") 'volume-raise-10)
                  (cons (kbd "<S-s-down>") 'volume-lower-10))

                 (let ((physical-monitor-names
                        (dhnam/machine-config-get-first 'exwm-physical-monitor-names)))
                   (if (and physical-monitor-names (> (length physical-monitor-names) 1))
                       (list
                        (cons (kbd "C-s-i") 'ewg/other-workspace-in-group-backwards)
                        (cons (kbd "C-s-o") 'ewg/other-workspace-in-group)
                        (cons (kbd "C-s-k") 'ewg/swap-with-other-workspace-in-group-backwards)
                        (cons (kbd "C-s-l") 'ewg/swap-with-other-workspace-in-group)
                        ;; (cons (kbd "s-I") 'ewg/swap-with-other-workspace-in-group-backwards)
                        ;; (cons (kbd "s-O") 'ewg/swap-with-other-workspace-in-group)
                        (cons (kbd "C-s-p") 'ewg/switch-previous-group)
                        (cons (kbd "C-s-n") 'ewg/switch-next-group))
                     (list
                      (cons (kbd "C-s-i") 'dhnam/exwm-other-workspace-backwards)
                      (cons (kbd "C-s-o") 'dhnam/exwm-other-workspace)
                      (cons (kbd "C-s-p") 'dhnam/exwm-other-workspace-backwards)
                      (cons (kbd "C-s-n") 'dhnam/exwm-other-workspace))))

                 (cl-letf (((symbol-function 'make-key-func-pair)
                            (lambda (i)
                              (cons (kbd (format "s-%d" i))
                                    (dhnam/exwm-make-environment-switch-create-specific i)))))
                   ;; 's-i': Switch to certain workspace group i.
                   (mapcar 'make-key-func-pair (number-sequence 1 7)))))

          (comment
            ;; Use "s-e" as prefix key instead of "C-c"
            ;; https://emacs.stackexchange.com/a/64130
            (define-key key-translation-map (kbd "s-e")  (kbd "C-c")))))

      ;; line-editing shortcuts
      (unless (get 'exwm-input-simulation-keys 'saved-value)
        ;; simulation keys
        ;; https://github.com/ch11ng/exwm/wiki#simulation-keys

        (progn
          ;; base bindings
          ;;
          ;; many bindings would cause wrong deployment for multiple monitor setting
          (setq exwm-base-input-simulation-keys
                (list
                 (cons (kbd "C-b") (kbd "<left>"))
                 (cons (kbd "C-f") (kbd "<right>"))
                 (cons (kbd "C-p") (kbd "<up>"))
                 (cons (kbd "C-n") (kbd "<down>"))
                 (cons (kbd "C-a") (kbd "<home>"))
                 (cons (kbd "C-e") (kbd "<end>"))
                 (cons (kbd "M-v") (kbd "<prior>"))
                 (cons (kbd "C-v") (kbd "<next>"))
                 (cons (kbd "M-i") (kbd "<prior>"))
                 (cons (kbd "M-o") (kbd "<next>"))
                 (cons (kbd "M->") (kbd "<C-end>"))
                 (cons (kbd "M-<") (kbd "<C-home>"))
                 (cons (kbd "M-v") (kbd "<prior>"))
                 (cons (kbd "C-d") (kbd "<delete>"))
                 (cons (kbd "C-k") (kbd "<S-end> C-c <delete>")) ; updated
                 (cons (kbd "M-k") (kbd "<S-end> C-c <right>")) ; updated

                 (cons (kbd "₫") (kbd "<escape>")) ; CruzeiroSign

                 (cons (kbd "C-w") (kbd "C-x"))
                 (cons (kbd "C-y") (kbd "C-v"))
                 (cons (kbd "M-w") (kbd "C-c"))
                 (cons (kbd "M-f") (kbd "<C-right>"))
                 (cons (kbd "M-b") (kbd "<C-left>"))

                 (cons (kbd "<M-backspace>") (kbd "<C-backspace>"))
                 (cons (kbd "M-d") (kbd "<C-delete>"))

                 (cons (kbd "C-/") (kbd "C-z"))
                 (cons (kbd "C-?") (kbd "C-y"))
                 (cons (kbd "M-/") (kbd "C-y"))))

          (setq exwm-advanced-input-simulation-keys
                (list
                 ;; expanding a region
                 (cons (kbd "C-M-p") (kbd "<S-up>"))
                 (cons (kbd "C-M-n") (kbd "<S-down>"))

                 (cons (kbd "C-M-b") (kbd "<C-S-left>"))
                 (cons (kbd "C-M-f") (kbd "<C-S-right>"))
                 (cons (kbd "C-M-a") (kbd "<S-home>"))
                 (cons (kbd "C-M-e") (kbd "<S-end>"))

                 ;; search
                 (cons (kbd "C-s") (kbd "C-f"))
                 (cons (kbd "C-g") (kbd "<escape>"))
                 (cons (kbd "M-p") (kbd "<S-f3>"))
                 (cons (kbd "M-n") (kbd "<f3>"))
                 (cons (kbd "<M-return>") (kbd "<escape>"))

                 ;; quit
                 (cons (kbd "C-x C-c") (kbd "C-q"))))

          (setq exwm-browser-input-simulation-keys
                (append
                 exwm-advanced-input-simulation-keys
                 (list
                  (cons (kbd "M-[") (kbd "<M-left>"))
                  (cons (kbd "M-]") (kbd "<M-right>"))

                  (cons (kbd "C-9") (kbd "<M-left>"))
                  (cons (kbd "C-0") (kbd "<M-right>"))
                  (cons (kbd "M-9") (kbd "<C-prior>"))
                  (cons (kbd "M-0") (kbd "<C-next>"))

                  ;; (cons (kbd "C-l") (kbd "<f6>"))
                  (cons (kbd "C-M-l") (kbd "<f6>"))
                  ;; (cons (kbd "M-l") (kbd "C-t"))

                  (cons (kbd "C-q C-k") (kbd "C-w"))

                  ;; caret browsing
                  (cons (kbd "<C-return>") (kbd "<f7>")))))

          (setq exwm-vimium-input-simulation-keys
                (list
                 ;; open links
                 (cons (kbd "C-j") (kbd "M-q M-j"))
                 (cons (kbd "M-j") (kbd "M-q M-l"))

                 ;; opne link
                 ;; (cons (kbd "C-o") (kbd "M-q M-o"))  ;; LinkHints.activateOpenInNewTab
                 ;; (cons (kbd "M-o") (kbd "M-q M-O"))  ;; LinkHints.activateWithQueue

                 ;; open back/forward history in a new tab
                 (cons (kbd "C-M-9") (kbd "M-q C-M-["))
                 (cons (kbd "C-M-0") (kbd "M-q C-M-]"))

                 ;; copy links
                 (cons (kbd "C-u") (kbd "T u"))
                 (cons (kbd "M-u") (kbd "T U"))

                 ;; visual mode
                 (cons (kbd "C-SPC") (kbd "M-q v"))

                 ;; select/copy text
                 (cons (kbd "M-s") (kbd "M-q s"))

                 ;; Tab deletion commands
                 (cons (kbd "C-q C-/") (kbd "M-q /"))
                 (cons (kbd "C-q C-o") (kbd "M-q *"))
                 (cons (kbd "C-q C-9") (kbd "M-q ("))
                 (cons (kbd "C-q C-0") (kbd "M-q )"))))

          (setq exwm-browser-app-input-simulation-keys
                (comment
                  (list
                   ;; for fuzzy search
                   ;; https://github.com/Fannon/search-bookmarks-history-and-tabs#readme
                   (cons (kbd "C-m") (kbd "C-S-.")))))

          (setq exwm-libreoffice-input-simulation-keys
                (append
                 exwm-base-input-simulation-keys
                 exwm-advanced-input-simulation-keys
                 (list
                  (cons (kbd "C-x C-s") (kbd "C-s"))
                  (cons (kbd "C-q C-s") (kbd "C-s"))
                  (cons (kbd "<C-return>") (kbd "<f2>"))))))

        (progn
          ;; global bindings
          (comment (setq exwm-input-simulation-keys exwm-base-input-simulation-keys))
          (progn
            ;; disable 'C-c' prefix
            (setq exwm-input-simulation-keys (list (cons (kbd "C-c") (kbd "C-c"))))))

        (progn
          ;; local bindings and customizations

          (defun dhnam/exwm-match-any-buffer-name (&rest names)
            (when exwm-class-name
              (member (downcase exwm-class-name) (mapcar 'downcase names))))

          (cl-macrolet ((register-simulation-keys
                         (buffer-name-or-names simulation-keys)
                         `(add-hook 'exwm-manage-finish-hook
                                    (lambda ()
                                      (when (and exwm-class-name
                                                 (let* ((buffer-name-or-names ,buffer-name-or-names)
                                                        (buffer-names (if (listp buffer-name-or-names)
                                                                          buffer-name-or-names
                                                                        (list buffer-name-or-names))))
                                                   (member (downcase exwm-class-name) (mapcar 'downcase buffer-names))))
                                        (exwm-input-set-local-simulation-keys
                                         ,simulation-keys))))))
            (comment
              (register-simulation-keys
               "Nyxt"
               (list
                (cons (kbd "C-p") (kbd "<up>"))
                (cons (kbd "C-n") (kbd "<down>"))
                (cons (kbd "C-g") (kbd "<escape>")))))

            (register-simulation-keys
             "Firefox"
             (append
              exwm-base-input-simulation-keys
              exwm-browser-input-simulation-keys
              exwm-vimium-input-simulation-keys
              exwm-browser-app-input-simulation-keys
              ))

            (register-simulation-keys
             "Google-chrome"
             (append
              exwm-base-input-simulation-keys
              exwm-browser-input-simulation-keys
              exwm-vimium-input-simulation-keys
              exwm-browser-app-input-simulation-keys))

            (register-simulation-keys
             '("kitty" "emacs")
             (list (cons (kbd "s-k") (kbd "C-x C-c"))))

            (comment
              (register-simulation-keys
               "kitty"
               nil))
            
            (register-simulation-keys
             '("Soffice" "libreoffice-calc")
             exwm-libreoffice-input-simulation-keys))))

      (progn
        ;; prefix keys for line-mode are defined in `exwm-input-prefix-keys'
        ;; https://github.com/ch11ng/exwm/wiki#global-key-bindings

        ;; disable some prefix keys
        (setq exwm-input-prefix-keys
              (cl-remove-if (lambda (x) (member x '(?\C-x ?\C-q ?\C-c ?\C-h)))
                            exwm-input-prefix-keys)))

      (progn
        ;; local key bindings
        ;; https://github.com/ch11ng/exwm/wiki#local-key-bindings
        (define-key exwm-mode-map (kbd "C-;") 'exwm-input-send-next-key)
        (comment (define-key exwm-mode-map (kbd "C-q") 'ctl-x-map))
        (define-key exwm-mode-map (kbd "M-!") 'shell-command)
        (comment (define-key exwm-mode-map (kbd "M-#") 'dhnam/lookup-word-from-web-other-window-for-exwm))
        (comment (define-key exwm-mode-map (kbd "C-x b") 'switch-to-buffer))
        (comment (define-key exwm-mode-map (kbd "M-&") 'dhnam/exwm-execute-shell-command))
        (lambda (command)
          (interactive (list (read-shell-command "$ ")))
          (start-process-shell-command command nil command))

        (comment
          (define-key exwm-mode-map (kbd "M-9") 'previous-buffer)
          (define-key exwm-mode-map (kbd "M-0") 'next-buffer))

        (comment (define-key exwm-mode-map (kbd "s-i") 'dhnam/counsel-switch-buffer-within-app)))

      (progn
        ;; Application-specific key bindings

        (progn
          ;; For `dhnam/exwm-firefox-mode'
          (cl-macrolet
              ((exwm-register-app-key
                (key)
                ;; Note
                ;; - the keys in `exwm-mode-map' triggers a Emacs command in an application such as Firefox
                ;; - if `dhnam/exwm-firefox-mode' is enabled,
                ;;   the keys in `dhnam/exwm-firefox-line-mode-map' have higher priorities than those of `exwm-mode-map'
                ;; - simulation keys have higher priorities than the keys in `exwm-mode-map'.
                ;; 
                `(define-key exwm-mode-map ,key '(lambda () (interactive) (dhnam/exwm-edit-send-key ,key)))))

            (exwm-register-app-key (kbd "C-l"))
            (exwm-register-app-key (kbd "M-l"))
            (exwm-register-app-key (kbd "M-L"))

            (exwm-register-app-key (kbd "C-m"))
            (exwm-register-app-key (kbd "M-m"))
            (exwm-register-app-key (kbd "M-M")))

          (add-hook 'exwm-manage-finish-hook 'dhnam/exwm-firefox-enable)
          ;; (define-key exwm-mode-map (kbd "C-l") #'dhnam/exwm-app-command-query-to-existing-firefox)
          )
        )))

  (progn
    ;; pre-config
    (dhnam/exwm-config-mine)

    ;; Enabling EXWM should be the last
    (exwm-enable))

  (progn
    ;; setting after exwm is started
    (start-process-shell-command
     "post-machine-config" nil
     "bash" "/home/dhnam/.emacs.d/exwm/post-machine-config.sh"))

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
    (efs/update-displays))

  (progn
    ;; posframe modes
    (when (fboundp 'ivy-posframe-mode)
      ;; https://github.com/tumashu/ivy-posframe
      (require 'ivy-posframe)

      ;; display at `ivy-posframe-style'
      (setq ivy-posframe-display-functions-alist
            '((ivy-switch-buffer                . ivy-posframe-display-at-frame-center)
              (counsel-find-file                . ivy-posframe-display-at-frame-center)
              (counsel-M-x                      . ivy-posframe-display-at-frame-bottom-center)
              (swiper                           . ivy-posframe-display-at-frame-bottom-center)
              (t                                . ivy-posframe-display-at-frame-bottom-center)
              ;; (t                                . ivy-display-function-fallback)
              ;; (t                                . ivy-posframe-display-at-frame-bottom-left)
              ))

      (comment
        ;; display emacs's minibuffer
        (setq ivy-posframe-hide-minibuffer nil))
      (ivy-posframe-mode 1))

    (when (fboundp 'which-key-posframe-mode)
      ;; https://github.com/yanghaoxie/which-key-posframe
      (require 'which-key-posframe)
      (which-key-posframe-mode)
      (setq which-key-posframe-poshandler 'posframe-poshandler-frame-center))))

(provide 'init-exwm)
