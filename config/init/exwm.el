(progn
  ;; processing a custom command line argument
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
  (defun exwm-simple-frame-init ()
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

  (defun exwm-config-mine ()
    (exwm-simple-frame-init)
    (dhnam/exwm-config-base)

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

    (progn
      ;; workspace switching functions
      (progn
        (defun exwm-other-workspace (count)
          (interactive "p")
          (exwm-workspace-switch (% (+ exwm-workspace-current-index 1)
                                    (exwm-workspace--count))))

        (defun exwm-other-workspace-backwards () (interactive) (exwm-other-workspace -1))

        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "r") 'exwm-reset)
          (define-key map (kbd "t") 'exwm-floating-toggle-floating)
          (define-key map (kbd "f") 'exwm-layout-set-fullscreen)

          (define-key map (kbd "j") 'exwm-input-grab-keyboard)
          (define-key map (kbd "k") 'exwm-input-release-keyboard)

	      (define-key map (kbd "o") (make-repeatable-command 'exwm-other-workspace))
	      (define-key map (kbd "O") (make-repeatable-command 'exwm-other-workspace-backwards))
          (define-key map (kbd "ㅐ") (make-repeatable-command 'exwm-other-workspace))
          (define-key map (kbd "ㅒ") (make-repeatable-command 'exwm-other-workspace-backwards))
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
      (cl-defun exwm-layout-set-fullscreen (&optional id)
        "Make window ID fullscreen."
        (interactive)
        ;; (exwm-layoaut-unset-fullscreen id)
        ;; (exwm-layout--show id)
        ))

    (progn
      ;; volume
      (require 'volume nil t))

    (progn
      ;; brightness
      (defun xrandr-set-brightness (brightness)
        (interactive "NEnter brightness percentage: ")
        (if (<= 0 brightness 100)
            (start-process-shell-command
             "xrandr-set-brightness" nil
             "~/.emacs.d/config/init/dependent/xrandr-set-brightness.sh"
             (number-to-string (/ brightness 100.0)))
          (user-error "Out of range"))))

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
      ;; custom-minibuffer
      (load "~/.emacs.d/config/init/dependent/awesome-tray.el"))

    (progn
      (comment
        (defun efs/exwm-update-title ()
          (pcase exwm-class-name
            ("Firefox" (exwm-workspace-rename-buffer (format "Firefox: %s" exwm-title)))
            ("Google-chrome" (exwm-workspace-rename-buffer (format "Google-chrome: %s" exwm-title)))))

        ;; When window title updates, use it to set the buffer name
        (add-hook 'exwm-update-title-hook #'efs/exwm-update-title))

      (progn
        ;; https://github.com/ch11ng/exwm/issues/198#issuecomment-249723369
        ;;
        ;; similar methods:
        ;; https://github.com/daviwil/emacs-from-scratch/blob/39f63fe133cd4c41e13bbd1551c6517162851411/show-notes/Emacs-Desktop-03.org#customizing-buffer-name-based-on-window-title
        ;; https://www.youtube.com/watch?v=HGGU5Zvljj8

        (defvar exwm-buffer-name-joint ": ")

        (defun exwm-rename-buffer ()
          (interactive)
          (exwm-workspace-rename-buffer
           (concat exwm-class-name exwm-buffer-name-joint
                   (if (<= (length exwm-title) 50) exwm-title
                     (concat (substring exwm-title 0 49) "...")))))

        ;; Add these hooks in a suitable place (e.g., as done in exwm-config-default)
        (add-hook 'exwm-update-class-hook 'exwm-rename-buffer)
        (add-hook 'exwm-update-title-hook 'exwm-rename-buffer)))

    (progn
      ;; enable switching betwen buffers in other workspaces
      ;;
      ;; https://github.com/ch11ng/exwm/wiki#x-window-handling-among-workspaces
      (setq exwm-workspace-show-all-buffers t)
      (setq exwm-layout-show-all-buffers t))

    (when (machine-config-get-first 'exwm-physical-monitor-names)
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

        (ewg/init (machine-config-get-first 'exwm-physical-monitor-names))

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

    (progn
      ;; application commands
      (progn
        ;; web browser commands
        (defun dhnam/exwm-command-execute-shell (command)
          (interactive (list (read-shell-command "$ ")))
          (start-process-shell-command command nil command))

        (defun dhnam/url-string-p (s)
          (string-match "[^[:blank:][:space:]]*://[^[:blank:][:space:]]*"
                        s))

        (defun get-web-search-query-url-string (query-string)
          (cond
           ((string-empty-p query-string)
            "https://www.google.com/")
           ((dhnam/url-string-p query-string) ; when query is an URL
            query-string)
           (t
            (format "\"https://www.google.com/search?q=%s\"" query-string))))

        (defun dhnan/open-web-browser (browser-command &optional url)
          (let ((web-search-query-url-string
                 (cond
                  (url url)
                  ((use-region-p)
                   (deactivate-mark)
                   (get-web-search-query-url-string
                    (buffer-substring-no-properties (region-beginning) (region-end))))
                  (t ""))))
            (start-process-shell-command "web-browser" nil
                                         (concat browser-command " " web-search-query-url-string))))

        (defun old-dhnam/exwm-command-open-web-browser ()
          (interactive)
          (start-process-shell-command "web-browser" nil "nyxt")
          (comment (start-process-shell-command "web-browser" nil "qutebrowser"))
          (comment (start-process-shell-command "web-browser" nil "firefox -new-window"))
          (comment (start-process-shell-command "web-browser" nil "google-chrome --app=https://www.google.com/"))
          (comment (start-process-shell-command "web-browser" nil "firefox -new-window https://www.google.com/"))
          (comment (start-process-shell-command "web-browser" nil "google-chrome --app=https://www.google.com/ --start-fullscreen"))
          (comment (start-process-shell-command "web-browser" nil "google-chrome --new-window"))
          (comment (start-process-shell-command "web-browser" nil "xdg-open https://")))

        (comment
          (defun old-dhnam/exwm-command-open-web-browser-incognito ()
            (interactive)
            (start-process-shell-command "web-browser" nil "qutebrowser ':open -p'")
            (comment (start-process-shell-command "web-browser" nil "firefox --private-window"))
            (comment (start-process-shell-command "web-browser" nil "firefox --private-window https://www.google.com/"))
            (comment (start-process-shell-command "web-browser" nil "google-chrome --new-window google.com --incognito"))
            (comment (start-process-shell-command "web-browser" nil "google-chrome --new-window google.com --incognito --start-fullscreen"))
            (comment (start-process-shell-command "web-browser" nil "google-chrome --new-window --incognito"))
            (comment (start-process-shell-command "web-browser" nil "xdg-open https://"))))

        (comment (fset 'dhnam/exwm-command-open-web-browser 'dhnam/exwm-command-open-nyxt))
        (comment (fset 'dhnam/exwm-command-open-web-browser 'dhnam/exwm-command-open-firefox-private))
        (fset 'dhnam/exwm-command-open-web-browser 'dhnam/exwm-command-open-firefox)
        (fset 'dhnam/exwm-command-open-web-browser-private 'dhnam/exwm-command-open-firefox-private)

        (defun dhnam/exwm-command-open-nyxt (&optional url)
          (interactive)
          (dhnan/open-web-browser "nyxt" url))

        (progn
          (defvar dhnam/web-search-engine-list-file-path
            "~/.emacs.d/config/init/dependent/search-engines.lisp")

          (defvar dhnam/web-search-engines
            (car (read-from-string (get-string-from-file dhnam/web-search-engine-list-file-path))))

          (defun dhnam/update-web-search-engines ()
            (interactive)
            (setq dhnam/web-search-engines
                  (car (read-from-string (get-string-from-file dhnam/web-search-engine-list-file-path)))))

          (defun dhnam/exwm-query-to-browser (&optional query open-web-browser)
            (interactive "sSearch query: ")
            (let* ((splits (s-split " " query))
                   (search-engine-entry
                    (assoc (car splits) dhnam/web-search-engines))
                   (query-string nil)
                   (url nil))
              (cond
               (search-engine-entry
                (setq query-string (s-join " " (cdr splits))))
               ((and (= (length splits) 1) (dhnam/url-string-p query))
                (setq url query))
               (t
                (progn
                  (setq search-engine-entry (assoc "gg" dhnam/web-search-engines))
                  (setq query-string query))))

              (unless url
                (setq url (if (string-empty-p query-string)
                              (caddr search-engine-entry)
                            (concat "\"" (s-replace "~a" query-string (cadr search-engine-entry)) "\""))))
              (funcall open-web-browser url)))

          (defun dhnam/exwm-command-query-to-browser (&optional query)
            (interactive "sSearch query: ")
            (dhnam/exwm-query-to-browser query #'dhnam/exwm-command-open-web-browser)))

        (defun dhnam/exwm-command-open-google-chrome (&optional url)
          (interactive)
          (dhnan/open-web-browser "google-chrome" url)
          (comment (dhnan/open-web-browser "google-chrome --app=https://www.google.com/")))

        (defun dhnam/exwm-command-open-google-chrome-incognito (&optional url)
          (interactive)
          (dhnan/open-web-browser "google-chrome --incognito" url)
          (comment (dhnan/open-web-browser "google-chrome --new-window google.com --incognito")))

        (defun dhnam/exwm-command-open-firefox (&optional url)
          (interactive)
          (dhnan/open-web-browser "firefox --new-window" url))

        (defun dhnam/exwm-command-open-firefox-private (&optional url)
          (interactive)
          (dhnan/open-web-browser "firefox --private-window" url)))

      (defun dhnam/exwm-command-open-terminal-emulator ()
        (interactive)
        (start-process-shell-command "terminal" nil "kitty"))

      (defun dhnam/exwm-command-open-flameshot-gui ()
        (interactive)
        (start-process-shell-command "screenshot" nil "flameshot gui"))

      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "w") 'dhnam/exwm-command-open-web-browser)
        (define-key map (kbd "W") 'dhnam/exwm-command-open-web-browser-private)
        (define-key map (kbd "q") 'dhnam/exwm-command-query-to-browser)
        ;; (define-key map (kbd "W") 'dhnam/exwm-command-open-web-browser-incognito)
        (define-key map (kbd "c") 'dhnam/exwm-command-open-google-chrome)
        (define-key map (kbd "C") 'dhnam/exwm-command-open-google-chrome-incognito)
        (comment (define-key map (kbd "f") 'dhnam/exwm-command-open-firefox))
        (comment (define-key map (kbd "F") 'dhnam/exwm-command-open-firefox-private))
        (define-key map (kbd "e") 'dhnam/exwm-command-open-terminal-emulator)
        (comment (define-key map (kbd "e") 'dhnam/gui-terminal))
        (define-key map (kbd "<print>") 'dhnam/exwm-command-open-flameshot-gui)

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
      ;; additional functions for convenience
      (defun counsel-switch-buffer-within-app ()
        "Switch to another buffer within application.
Display a preview of the selected ivy completion candidate buffer
in the current window."
        (interactive)
        (let ((ivy-update-fns-alist
               '((ivy-switch-buffer . counsel--switch-buffer-update-fn)))
              (ivy-unwind-fns-alist
               '((ivy-switch-buffer . counsel--switch-buffer-unwind))))
          (ivy-switch-buffer-within-app)))

      (defun ivy-switch-buffer-within-app ()
        "Switch to another buffer within application.."
        (interactive)
        (let ((ivy-ignore-buffers
               (if exwm-class-name  ; (string-match-p exwm-buffer-name-joint (buffer-name (current-buffer)))
                   ivy-ignore-buffers
                 (cons exwm-buffer-name-joint ivy-ignore-buffers))))
          (ivy-read "Switch to buffer: " #'internal-complete-buffer
                    :keymap ivy-switch-buffer-map
                    :preselect (unless exwm-class-name (buffer-name (other-buffer (current-buffer))))
                    :action #'ivy--switch-buffer-action
                    :matcher #'ivy--switch-buffer-matcher
                    :caller 'ivy-switch-buffer
                    :initial-input (when exwm-class-name
                                     (concat (downcase exwm-class-name) exwm-buffer-name-joint))))))

    (progn
      ;; additional functions for convenience
      (defun counsel-switch-buffer-from-current ()
        "Switch to another buffer within application.
Display a preview of the selected ivy completion candidate buffer
in the current window."
        (interactive)
        (let ((ivy-update-fns-alist
               '((ivy-switch-buffer . counsel--switch-buffer-update-fn)))
              (ivy-unwind-fns-alist
               '((ivy-switch-buffer . counsel--switch-buffer-unwind))))
          (ivy-switch-buffer-from-current)))

      (defun ivy-switch-buffer-from-current ()
        "Switch to another buffer within application.."
        (interactive)
        (let ((ivy-ignore-buffers
               (if exwm-class-name  ; (string-match-p exwm-buffer-name-joint (buffer-name (current-buffer)))
                   ivy-ignore-buffers
                 (cons exwm-buffer-name-joint ivy-ignore-buffers))))
          (ivy-read "Switch to buffer: " #'internal-complete-buffer
                    :keymap ivy-switch-buffer-map
                    ;; :preselect (buffer-name (current-buffer))
                    :action #'ivy--switch-buffer-action
                    :matcher #'ivy--switch-buffer-matcher
                    :caller 'ivy-switch-buffer))))

    (progn
      ;; additional functions for convenience
      (defun counsel-switch-buffer-excluding-exwm ()
        "Switch to another buffer within application.
Display a preview of the selected ivy completion candidate buffer
in the current window."
        (interactive)
        (let ((ivy-update-fns-alist
               '((ivy-switch-buffer . counsel--switch-buffer-update-fn)))
              (ivy-unwind-fns-alist
               '((ivy-switch-buffer . counsel--switch-buffer-unwind))))
          (ivy-switch-buffer-excluding-exwm)))

      (defun ivy-switch-buffer-excluding-exwm ()
        "Switch to another buffer within application.."
        (interactive)
        (let ((ivy-ignore-buffers
               (cons (buffer-name (current-buffer))
                     (cons exwm-buffer-name-joint ivy-ignore-buffers))))
          (ivy-read "Switch to buffer: " #'internal-complete-buffer
                    :keymap ivy-switch-buffer-map
                    ;; :preselect (buffer-name (current-buffer))
                    :action #'ivy--switch-buffer-action
                    :matcher #'ivy--switch-buffer-matcher
                    :caller 'ivy-switch-buffer))))

    (with-eval-after-load 'ivy
      (defun ivy-insert-current-exwm-class-name ()
        "Insert `exwm-class-name'"
        (interactive)
        (when dhnam/ivy-original-exwm-class-name
          (delete-minibuffer-contents)
          (insert (concat (downcase dhnam/ivy-original-exwm-class-name) exwm-buffer-name-joint))))

      (progn
        (defun dhnam/ivy-switch-buffer-advice-for-exwm (orig-fun &rest args)
          (let ((dhnam/ivy-original-exwm-class-name exwm-class-name))
            (apply orig-fun args)))

        (advice-add 'ivy-switch-buffer :around #'dhnam/ivy-switch-buffer-advice-for-exwm)
        (advice-add 'ivy-switch-buffer-within-app :around #'dhnam/ivy-switch-buffer-advice-for-exwm)
        (advice-add 'ivy-switch-buffer-from-current :around #'dhnam/ivy-switch-buffer-advice-for-exwm)
        (advice-add 'ivy-switch-buffer-excluding-exwm :around #'dhnam/ivy-switch-buffer-advice-for-exwm))

      (ivy-define-key ivy-minibuffer-map (kbd "s-j") 'ivy-insert-current-exwm-class-name)
      (ivy-define-key ivy-minibuffer-map (kbd "C-s-j") 'ivy-insert-current-exwm-class-name))

    (progn
      ;; extended emacs commands for exwm

      (defun counsel-find-file-in-downloads (&optional initial-input initial-directory)
        "Forward to `find-file'.
When INITIAL-INPUT is non-nil, use it in the minibuffer during completion."
        (interactive)
        (let ((default-directory "~/Downloads/"))
          (counsel-find-file initial-input initial-directory)))

      (progn
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "d") 'counsel-find-file-in-downloads)
          (define-key map (kbd "w") 'dhnam/kill-gc)

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
               '(((member exwm-class-name '("Firefox" "firefox" "Google-chrome" "google-chrome"))
	              line-mode t)))
             (progn
               ;; start applications in char-mode by default
               ;; https://github.com/ch11ng/exwm/issues/411#issuecomment-379561414
               '((t char-mode t))))))

    (progn
      (defvar exwm-split-defined t)

      (defun exwm-split-window-below (&optional size)
        ;; https://github.com/ch11ng/exwm/issues/685#issuecomment-879903947
        (interactive "P")
        (split-window-below size)
        (comment (buf-move-down))
        (windmove-down)
        (redisplay)
        (windmove-up))

      (defun exwm-split-window-right (&optional size)
        ;; https://github.com/ch11ng/exwm/issues/685#issuecomment-879903947
        (interactive "P")
        (split-window-right size)
        (comment (buf-move-right))
        (windmove-right)
        (redisplay)
        (windmove-left))

      (defun exwm-split-move-window-below (&optional size)
        ;; https://github.com/ch11ng/exwm/issues/685#issuecomment-879903947
        (interactive "P")
        (split-window-below size)
        (redisplay)
        (windmove-down))

      (defun exwm-split-move-window-right (&optional size)
        ;; https://github.com/ch11ng/exwm/issues/685#issuecomment-879903947
        (interactive "P")
        (split-window-right size)
        (redisplay)
        (windmove-right)))

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
        (key-chord-define-global "qj" 'counsel-switch-buffer-excluding-exwm)
        (key-chord-define-global "qd" 'dhnam/exwm-workspace-prefix-map)

        (defhydra hydra-buffer-shift (global-map "C-c s")
          "buffer-shift"
          ("q" nil "quit")
          ("j" buf-shift-left)
          ("l" buf-shift-right)
          ("i" buf-shift-up)
          ("k" buf-shift-down))

        (comment
          (global-set-key (kbd "C-x 2") 'exwm-split-window-below)
          (global-set-key (kbd "C-x 3") 'exwm-split-window-right)

          (global-set-key (kbd "C-x 8") 'exwm-split-window-below)
          (global-set-key (kbd "C-x 7") 'exwm-split-window-right)))

      (comment
        ;; enable key-chord and hydra
        ;; https://www.reddit.com/r/emacs/comments/8yf6dx/key_chords_in_exwm/
        (comment (setq exwm-input-line-mode-passthrough t))
        (defun toggle-exwm-input-line-mode-passthrough ()
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

        (let ((physical-monitor-names
               (machine-config-get-first 'exwm-physical-monitor-names)))
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
          (assert (not (get 'exwm-input-global-keys 'saved-value)))
          (fset 'help-map help-map)

          (setq exwm-input-global-keys
                (append
                 '(;; ([?\s-r] . exwm-reset)
                   ;; ([?\s-t] . exwm-floating-toggle-floating)
                   ([?\s-\;] . exwm-input-send-next-key)

                   ([?\s-&] . dhnam/exwm-command-execute-shell)
                   ([?\s-m] . dhnam/exwm-command-prefix-map)
                   ([?\s-d] . dhnam/exwm-workspace-prefix-map)
                   ([?\s-'] . dhnam/exwm-extended-emacs-command-prefix-map)

                   ([?\s-q] . ctl-x-map)
                   ([?\s-x] . (lambda () (interactive) (funcall (key-binding (kbd "M-x")))))
                   ([?\s-X] . (lambda () (interactive) (funcall (key-binding (kbd "M-x")))))
                   ([?\s-w] . tab-prefix-map)
                   ;; ([?\s-e] . null) ; Use "s-e" as prefix key instead of "C-c" | https://emacs.stackexchange.com/a/64130
                   ([?\s-e] . my-ctl-c-map) ; Use "s-e" as prefix key instead of "C-c" | https://emacs.stackexchange.com/a/64130
                   ([?\s-h] . help-map)
                   ([?\s-u] . universal-argument)

                   ([?\s-l] . counsel-find-file)
                   ;; ([?\s-l] . find-file)
                   ([?\s-k] . kill-buffer)

                   ([?\s-j] . counsel-switch-buffer-excluding-exwm)
                   ([?\C-\s-j] . ivy-switch-buffer)
                   ;; ([?\C-\s-j] . counsel-switch-buffer)
                   ;; ([?\C-\s-j] . ivy-switch-buffer-within-app)
                   ;; ([?\C-\s-b] . ivy-switch-buffer-within-app)
                   ;; ([?\s-B] . counsel-switch-buffer-within-app)
                   ;; ([?\C-\s-b] . counsel-switch-buffer-within-app)
                   ;; ([?\C-\s-j] . counsel-switch-buffer-within-app)

                   ([?\s-!] . shell-command)

                   ([?\s-9] . previous-buffer)
                   ([?\s-0] . next-buffer)

                   ([?\s-i] . other-window-backwards)
                   ([?\s-o] . other-window)
                   ([?\s-p] . tab-previous)
                   ([?\s-n] . tab-next))

                 '(([S-s-up] . volume-raise-10)
                   ([S-s-down] . volume-lower-10))

                 (let ((physical-monitor-names
                        (machine-config-get-first 'exwm-physical-monitor-names)))
                   (if (and physical-monitor-names (> (length physical-monitor-names) 1))
                       '(([?\C-\s-i] . ewg/other-workspace-in-group-backwards)
                         ([?\C-\s-o] . ewg/other-workspace-in-group)
                         ([?\C-\s-p] . ewg/switch-previous-group)
                         ([?\C-\s-n] . ewg/switch-next-group))
                     '(([?\C-\s-i] . exwm-other-workspace-backwards)
                       ([?\C-\s-o] . exwm-other-workspace)
                       ([?\C-\s-p] . exwm-other-workspace-backwards)
                       ([?\C-\s-n] . exwm-other-workspace))))

                 `(;; 's-N': Switch to certain workspace.
                   ,@(mapcar (lambda (i)
                               `(,(kbd (format "s-%d" i)) .
                                 (lambda ()
                                   (interactive)
                                   (,exwm-environment-switch-create
                                    ,(% (+ i 10 (- dhnam/exwm-workspace-start-number)) 10)))))
                             ; using number key 1 to 7
                             (number-sequence 1 7)))

                 (comment
                   `(;; 's-N': Switch to certain workspace.
                     ,@(mapcar (lambda (i)
                                 `(,(kbd (format "s-%d" i)) .
                                   (lambda ()
                                     (interactive)
                                     (,exwm-environment-switch-create
                                      ,(% (+ i 10 (- dhnam/exwm-workspace-start-number)) 10)))))
                               ; using number key 0 to 9
                               (number-sequence 0 9))))))

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
                '(([?\C-b] . [left])
                  ([?\C-f] . [right])
                  ([?\C-p] . [up])
                  ([?\C-n] . [down])
                  ([?\C-a] . [home])
                  ([?\C-e] . [end])
                  ([?\M-v] . [prior])
                  ([?\C-v] . [next])
                  ([?\M->] . [C-end])
                  ([?\M-<] . [C-home])
                  ([?\M-v] . [prior])
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
                  ([M-backspace] . [C-backspace])
                  ([?\M-d] . [C-delete])

                  ([?\C-/] . [?\C-z])
                  ([?\C-?] . [?\C-y])
                  ([?\M-/] . [?\C-y])))

          (setq exwm-browser-input-simulation-keys
                '(([?\C-\M-p] . [S-up])
                  ([?\C-\M-n] . [S-down])
                  ;; ([?\C-\M-b] . [S-left])
                  ;; ([?\C-\M-f] . [S-right])
                  ;; ([?\C-\M-i] . [C-S-left])
                  ;; ([?\C-\M-o] . [C-S-right])
                  ([?\C-\M-b] . [C-S-left])
                  ([?\C-\M-f] . [C-S-right])
                  ([?\C-\M-a] . [S-home])
                  ([?\C-\M-e] . [S-end])

                  ([?\C-s] . [?\C-f])
                  ([?\C-g] . [escape])
                  ([?\M-p] . [S-f3])
                  ([?\M-n] . [f3])
                  ([\M-return] . [escape]) ;; <M-return> or M-RET
                  ;; ([\C-return] . [escape]) ;; <C-return>
                  ;; ([\C-\M-return] . [escape]) ;; <C-M-return>

                  ;; ([?\M-p] . [C-prior])
                  ;; ([?\M-n] . [C-next])
                  ([?\M-\[] . [M-left])
                  ([?\M-\]] . [M-right])

                  ([?\C-9] . [M-left])
                  ([?\C-0] . [M-right])
                  ([?\M-9] . [C-prior])
                  ([?\M-0] . [C-next])

                  ([?\C-i] . [\q \u])
                  ([?\M-i] . [\q \U])

                  ([?\C-l] . [f6])
                  ([?\M-l] . [?\C-t])

                  ([?\C-q?\C-k] . [?\C-w])
                  ([?\C-x?\C-c] . [?\C-q])

                  ;; caret browsing
                  ;; ([?\C-\M-\ ] . [f7])
                  ;; ([?\C-\] . [f7]) ; <C-return> --> not working
                  ([\C-return] . [f7])
                  ;; ([\M-space] . [f7]) ; not working
                  ;; ([\M-space] . [f7]) ; not working
                  ))

          (setq exwm-vimium-input-simulation-keys
                '(([?\C-j] . [?\M-q?\M-j])
                  ([?\M-j] . [?\M-q?\M-l])
                  ([?\C-o] . [?\M-q?\M-o])
                  ([?\M-o] . [?\M-q?\M-O])

                  ;; open back/forward history in a new tab
                  ([?\C-\M-9] . [?\M-q?\C-\M-b])
                  ([?\C-\M-0] . [?\M-q?\C-\M-f])

                  ;; Tab deletion commands
                  ([?\C-q?\C-/] . [?\M-q?\/])
                  ([?\C-q?\C-o] . [?\M-q?*])
                  ([?\C-q?\C-9] . [?\M-q?\(])
                  ([?\C-q?\C-0] . [?\M-q?\)])))

          (setq exwm-browser-app-input-simulation-keys
                '(;; for fuzzy search
                  ;; https://github.com/Fannon/search-bookmarks-history-and-tabs#readme
                  ([?\C-m] . [\C-S-.])))
          )

        (progn
          ;; global bindings
          (comment (setq exwm-input-simulation-keys exwm-base-input-simulation-keys))
          (progn
            ;; disable 'C-c' prefix
            (setq exwm-input-simulation-keys '(([?\C-c] . [?\C-c])))))

        (progn
          ;; local bindings and customizations

          (comment
            (add-hook 'exwm-manage-finish-hook
                      (lambda ()
                        (when (and exwm-class-name
                                   (string= exwm-class-name "Nyxt"))
                          (exwm-input-set-local-simulation-keys
                           '(([?\C-p] . [up])
                             ([?\C-n] . [down])
                             ([?\C-g] . [escape])
                             ))))))

          (add-hook 'exwm-manage-finish-hook
                    (lambda ()
                      (when (and exwm-class-name
                                 (string= (downcase exwm-class-name) (downcase "Firefox")))
                        (exwm-input-set-local-simulation-keys
                         (append
                          exwm-base-input-simulation-keys
                          exwm-browser-input-simulation-keys
                          exwm-vimium-input-simulation-keys
                          exwm-browser-app-input-simulation-keys
                          )))))

          (add-hook 'exwm-manage-finish-hook
                    (lambda ()
                      (when (and exwm-class-name
                                 (string= (downcase exwm-class-name) (downcase "Google-chrome")))
                        (exwm-input-set-local-simulation-keys
                         (append
                          exwm-base-input-simulation-keys
                          exwm-browser-input-simulation-keys
                          exwm-vimium-input-simulation-keys
                          exwm-browser-app-input-simulation-keys)))))

          (add-hook 'exwm-manage-finish-hook
                    (lambda ()
                      (when (and exwm-class-name
                                 (or (string= (downcase exwm-class-name) (downcase "kitty"))
                                     (string= (downcase exwm-class-name) (downcase "emacs"))))
                        (exwm-input-set-local-simulation-keys
                         '(([?\s-k] . [?\C-x?\C-c]))))))

          (comment
            (add-hook 'exwm-manage-finish-hook
                      (lambda ()
                        (when (and exwm-class-name
                                   (string= exwm-class-name "kitty"))
                          (exwm-input-set-local-simulation-keys nil)))))))

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
        (comment (define-key exwm-mode-map (kbd "M-#") 'lookup-word-from-web-other-window-for-exwm))
        (comment (define-key exwm-mode-map (kbd "C-x b") 'switch-to-buffer))
        (comment (define-key exwm-mode-map (kbd "M-&") 'dhnam/exwm-execute-shell-command))
        (lambda (command)
          (interactive (list (read-shell-command "$ ")))
          (start-process-shell-command command nil command))

        (comment
          (define-key exwm-mode-map (kbd "M-9") 'previous-buffer)
          (define-key exwm-mode-map (kbd "M-0") 'next-buffer))

        (comment (define-key exwm-mode-map (kbd "s-i") 'counsel-switch-buffer-within-app)))
      ))

  (progn
    ;; pre-config
    (exwm-config-mine)

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
