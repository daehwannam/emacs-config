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
  (defun exwm-simple-frame-init ()
    (progn
      "this code block has the same effect with `exwm-config-misc'"
      (toggle-scroll-bar -1)
      (menu-bar-mode -1)
      (tool-bar-mode -1)
      (fringe-mode 1))

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

  (defun exwm-config-my-base ()
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
      (exwm-config-ido))
    ;; Other configurations
    (exwm-config-misc))

  (defun exwm-config-mine ()
    (exwm-simple-frame-init)
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
	      (defvar exwm-my-workspace-prefix-map map
	        "Keymap for workspace related commands."))
        (fset 'exwm-my-workspace-prefix-map exwm-my-workspace-prefix-map)
        (comment (exwm-input-set-key (kbd "s-w") 'exwm-my-workspace-prefix-map)))

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

        (defun exwm-rename-buffer ()
          (interactive)
          (exwm-workspace-rename-buffer
           (concat exwm-class-name ": "
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

    (when (machine-config-get-first 'exwm-multiple-monitor-layout-type)
      ;; multiple monitor setting

      (progn
        ;; workspace-group functions
        (setq exwm-workspace-group-max-size
              (pcase (machine-config-get-first 'exwm-multiple-monitor-layout-type)
                (triple 3)
                (t (error "Unknown monitor layout configuration"))))
        (setq exwm-workspace-number exwm-workspace-group-max-size) ; initial num of workspaces

        (defun exwm-get-workspace-group-index (workspace-idx)
          (/ workspace-idx exwm-workspace-group-max-size))

        (defun exwm-get-last-workspace-group-index ()
          (exwm-get-workspace-group-index (1- (exwm-workspace--count))))

        (defun exwm-get-workspace-group-member-idx (workspace-idx)
          (- workspace-idx (* (exwm-get-workspace-group-index workspace-idx)
                              exwm-workspace-group-max-size)))
        (progn
          (defun exwm-other-workspace-in-group (count)
            (interactive "p")
            (let* ((group-idx (exwm-get-workspace-group-index exwm-workspace-current-index))
                   (group-size (min (- (exwm-workspace--count)
                                       (* group-idx exwm-workspace-group-max-size))
                                    exwm-workspace-group-max-size))
                   (member-idx (- exwm-workspace-current-index
                                  (* group-idx exwm-workspace-group-max-size)))
                   (next-member-idx (% (+ count member-idx group-size) group-size))
                   (next-workspace-idx (+ (* group-idx exwm-workspace-group-max-size)
                                          next-member-idx)))
              (exwm-workspace-switch next-workspace-idx)))

          (defun exwm-other-workspace-in-group-backwards () (interactive) (exwm-other-workspace-in-group -1)))
        
        (comment
          (global-set-key (kbd "C-c o") (make-repeatable-command 'exwm-other-workspace-in-group))
	      (global-set-key (kbd "C-c O") (make-repeatable-command 'exwm-other-workspace-in-group-backwards)))

        (defun exwm-workspace-group-switch-create (group-idx)
          (let* ((current-member-idx (exwm-get-workspace-group-member-idx exwm-workspace-current-index))
                 (new-workspace-idx (+ (* group-idx exwm-workspace-group-max-size)
                                       current-member-idx)))
            (dotimes (i exwm-workspace-group-max-size)
              (exwm-workspace-switch-create (+ (* group-idx exwm-workspace-group-max-size) i)))
            (exwm-workspace-switch new-workspace-idx)))

        (defun exwm-workspace-group-delete (group-idx)
          (let ((prev-workspace-idx exwm-workspace-current-index))
            (dolist (i (reverse (number-sequence 0 (1- exwm-workspace-group-max-size))))
              (exwm-workspace-delete (+ (* group-idx exwm-workspace-group-max-size) i)))
            (let ((new-workspace-idx (% (+ prev-workspace-idx (exwm-workspace--count))
                                        (exwm-workspace--count))))
              (exwm-workspace-switch new-workspace-idx))))

        (defun exwm-workspace-group-switch-next-group (count)
          (interactive "p")
          (if (<= (exwm-workspace--count) exwm-workspace-group-max-size)
              (user-error "There's no other workspace group")
            (let* ((current-group-idx (exwm-get-workspace-group-index exwm-workspace-current-index))
                   (num-groups (1+ (exwm-get-last-workspace-group-index)))
                   (next-group-idx (% (+ current-group-idx count num-groups) num-groups)))
              (exwm-workspace-group-switch-create next-group-idx))))

        (defun exwm-workspace-group-switch-previous-group ()
          (interactive)
          (exwm-workspace-group-switch-next-group -1))

        (defun exwm-workspace-group-add-group ()
          (interactive)
          (let* ((current-group-idx (exwm-get-workspace-group-index exwm-workspace-current-index))
                 (next-group-idx (1+ current-group-idx))
                 (current-member-idx (exwm-get-workspace-group-member-idx exwm-workspace-current-index))
                 (new-workspace-idx (+ (* next-group-idx exwm-workspace-group-max-size)
                                       current-member-idx)))
            (dotimes (i exwm-workspace-group-max-size)
              (exwm-workspace-add (+ (* next-group-idx exwm-workspace-group-max-size) i)))
            (exwm-workspace-switch new-workspace-idx)))

        (defun exwm-workspace-group-delete-current-group ()
          (interactive)
          (if (<= (exwm-workspace--count) exwm-workspace-group-max-size)
              (user-error "Attempt to delete the sole workspace group")
            (if (y-or-n-p (format "Are you sure you want to close this workspace group? "))
	            (exwm-workspace-group-delete
                 (exwm-get-workspace-group-index exwm-workspace-current-index))
              (message "Canceled closing the current workspace group"))))

        (defun exwm-workspace-group-delete-other-groups ()
          (interactive)
          (if (<= (exwm-workspace--count) exwm-workspace-group-max-size)
              (user-error "There's no other workspace group")
            (if (y-or-n-p (format "Are you sure you want to close other workspace groups? "))
                (let ((prev-workspace-idx exwm-workspace-current-index))
                  (let* ((group-idx (exwm-get-workspace-group-index exwm-workspace-current-index))
                         (first-workspace-idx-in-group (* group-idx exwm-workspace-group-max-size))
                         (workspace-indices-in-group
                          (number-sequence first-workspace-idx-in-group
                                           (+ first-workspace-idx-in-group
                                              (1- exwm-workspace-group-max-size)))))
                    (dolist (i (reverse (number-sequence 0 (1- (exwm-workspace--count)))))
                      (unless (member i workspace-indices-in-group)
                        (exwm-workspace-delete i))))
                  (exwm-workspace-switch (% prev-workspace-idx (exwm-workspace--count))))
              (message "Canceled closing other workspace groups"))))

        (defun exwm-workspace-swap-by-workspace-indices (index1 index2)
          (exwm-workspace-swap (exwm-workspace--workspace-from-frame-or-index index1)
                               (exwm-workspace--workspace-from-frame-or-index index2)))

        (defun exwm-workspace-group-swap (group-idx1 group-idx2)
          (dotimes (i exwm-workspace-group-max-size)
            (exwm-workspace-swap-by-workspace-indices
             (+ (* group-idx1 exwm-workspace-group-max-size) i)
             (+ (* group-idx2 exwm-workspace-group-max-size) i))))

        (defun exwm-workspace-group-swap-current-group-number (group-number)
          (interactive "nEnter workspace group number: ")
          (if (> group-number (exwm-workspace--count))
              (user-error "Workspace group number is out of range")
            (let ((group-idx (- group-number exwm-my-workspace-start-number))
                  (current-group-idx (exwm-get-workspace-group-index exwm-workspace-current-index)))
              (if (= group-idx current-group-idx)
                  (user-error "Cannot swap with the same workspace group")
                (exwm-workspace-group-swap current-group-idx group-idx)))))

        (let ((map exwm-my-workspace-prefix-map))
          ;; update `exwm-my-workspace-prefix-map'
          (define-key map (kbd "o") (make-repeatable-command 'exwm-other-workspace-in-group))
          (define-key map (kbd "O") (make-repeatable-command 'exwm-other-workspace-in-group-backwards))

          (define-key map (kbd "w") 'exwm-workspace-group-swap-current-group-number)

          (define-key map (kbd "8") 'exwm-workspace-group-add-group)
          (define-key map (kbd "9") 'exwm-workspace-group-delete-other-groups)
          (define-key map (kbd "0") 'exwm-workspace-group-delete-current-group)))

      (progn
        ;; xrandr config
        ;; https://github.com/daviwil/emacs-from-scratch/blob/5ebd390119a48cac6258843c7d5e570f4591fdd4/show-notes/Emacs-Desktop-04.org

        (require 'exwm-randr)
        (defvar exwm-my-monitor-names (comment (list "HDMI-1-1" "DVI-I-1" "HDMI-0")))

        (progn
          (require 'cl-lib)
          (defun get-exwm-randr-workspace-monitor-plist (max-num-groups)
            "mapping workspace indices with monitors"
            (let ((max-num-workspaces (* max-num-groups exwm-workspace-group-max-size)))
              (cl-labels
                  ((get-plist (num)
                              (when (< num max-num-workspaces)
                                (cons num (cons (nth (% num exwm-workspace-group-max-size)
                                                     exwm-my-monitor-names)
                                                (get-plist (1+ num)))))))
                (get-plist 0)))))

        (pcase (machine-config-get-first 'exwm-multiple-physical-monitor-layout)
          (descartes-triple
           (setq exwm-my-monitor-names (list "HDMI-1-1" "DVI-I-1" "HDMI-0"))
           (progn
             (setq exwm-randr-workspace-monitor-plist (get-exwm-randr-workspace-monitor-plist 10))
             (comment
               (setq exwm-randr-workspace-monitor-plist
                     '(0 "HDMI-1-1" 1 "DVI-I-1" 2 "HDMI-0"
                         3 "HDMI-1-1" 4 "DVI-I-1" 5 "HDMI-0" ...)))

             ;; run xrandr
             (add-hook 'exwm-randr-screen-change-hook
                       (lambda ()
                         (start-process-shell-command
                          "xrandr" nil
                          (format "xrandr --output %s --auto \
                                --output %s --auto --left-of %s \
                                --output %s --auto --right-of %s"
                                  (nth 1 exwm-my-monitor-names)
                                  (nth 0 exwm-my-monitor-names) (nth 1 exwm-my-monitor-names)
                                  (nth 2 exwm-my-monitor-names) (nth 1 exwm-my-monitor-names)))))))
          (t (error "Unknown monitor physical layout configuration")))

        (exwm-randr-enable)))

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
        (define-key exwm-my-workspace-prefix-map (kbd "v") 'hydra-volume/body))
      (define-key exwm-my-workspace-prefix-map (kbd "v") 'volume-set))

    (progn
      ;; application commands
      (progn
        ;; web browser commands
        (defun exwm-my-command-execute-shell (command)
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

        (defun my-open-web-browser (browser-command &optional url)
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

        (defun old-exwm-my-command-open-web-browser ()
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
          (defun old-exwm-my-command-open-web-browser-incognito ()
            (interactive)
            (start-process-shell-command "web-browser" nil "qutebrowser ':open -p'")
            (comment (start-process-shell-command "web-browser" nil "firefox --private-window"))
            (comment (start-process-shell-command "web-browser" nil "firefox --private-window https://www.google.com/"))
            (comment (start-process-shell-command "web-browser" nil "google-chrome --new-window google.com --incognito"))
            (comment (start-process-shell-command "web-browser" nil "google-chrome --new-window google.com --incognito --start-fullscreen"))
            (comment (start-process-shell-command "web-browser" nil "google-chrome --new-window --incognito"))
            (comment (start-process-shell-command "web-browser" nil "xdg-open https://"))))

        (fset 'exwm-my-command-open-web-browser 'exwm-my-command-open-nyxt)

        (defun exwm-my-command-open-nyxt (&optional url)
          (interactive)
          (my-open-web-browser "nyxt" url))

        (progn
          (defvar nyxt-search-engines
            (car (read-from-string (get-string-from-file "~/.config/nyxt/search-engines.lisp"))))

          (defun exwm-my-command-query-to-browser (&optional query)
            (interactive "sSearch query: ")
            (let* ((splits (s-split " " query))
                   (search-engine-entry
                    (assoc (car splits) nyxt-search-engines))
                   (query-string nil)
                   (url nil))
              (cond
               (search-engine-entry
                (setq query-string (s-join " " (cdr splits))))
               ((and (= (length splits) 1) (dhnam/url-string-p query))
                (setq url query))
               (t
                (progn
                  (setq search-engine-entry (assoc "gg" nyxt-search-engines))
                  (setq query-string query))))

              (unless url
                (setq url (if (string-empty-p query-string)
                              (caddr search-engine-entry)
                            (concat "\"" (s-replace "~a" query-string (cadr search-engine-entry)) "\""))))
              (exwm-my-command-open-web-browser url))))

        (defun exwm-my-command-open-google-chrome (&optional url)
          (interactive)
          (my-open-web-browser "google-chrome" url)
          (comment (my-open-web-browser "google-chrome --app=https://www.google.com/")))

        (defun exwm-my-command-open-google-chrome-incognito (&optional url)
          (interactive)
          (my-open-web-browser "google-chrome --incognito" url)
          (comment (my-open-web-browser "google-chrome --new-window google.com --incognito")))

        (defun exwm-my-command-open-firefox (&optional url)
          (interactive)
          (my-open-web-browser "firefox --new-window" url))

        (defun exwm-my-command-open-firefox-private (&optional url)
          (interactive)
          (my-open-web-browser "firefox --private-window" url)))

      (defun exwm-my-command-open-terminal-emulator ()
        (interactive)
        (start-process-shell-command "terminal" nil "kitty"))

      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "w") 'exwm-my-command-open-web-browser)
        (define-key map (kbd "q") 'exwm-my-command-query-to-browser)
        ;; (define-key map (kbd "W") 'exwm-my-command-open-web-browser-incognito)
        (define-key map (kbd "c") 'exwm-my-command-open-google-chrome)
        (define-key map (kbd "C") 'exwm-my-command-open-google-chrome-incognito)
        (define-key map (kbd "f") 'exwm-my-command-open-firefox)
        (define-key map (kbd "F") 'exwm-my-command-open-firefox-private)
        (define-key map (kbd "e") 'exwm-my-command-open-terminal-emulator)

	    (defvar exwm-my-command-prefix-map map
	      "Keymap for workspace related commands."))
      (fset 'exwm-my-command-prefix-map exwm-my-command-prefix-map))

    (progn
      ;; functions for line-mode
      (defun counsel-switch-buffer-within-app ()
        "Switch to another buffer within application.
Display a preview of the selected ivy completion candidate buffer
in the current window."
        (interactive)
        (let ((ivy-update-fns-alist
               '((ivy-switch-buffer . counsel--switch-buffer-update-fn)))
              (ivy-unwind-fns-alist
               '((ivy-switch-buffer . counsel--switch-buffer-unwind))))
          (ivy-read "Switch to buffer: " #'internal-complete-buffer
                    :keymap ivy-switch-buffer-map
                    :preselect (buffer-name (other-buffer (current-buffer)))
                    :action #'ivy--switch-buffer-action
                    :matcher #'ivy--switch-buffer-matcher
                    :caller 'ivy-switch-buffer
                    :initial-input (and exwm-class-name
                                        (concat (downcase (or exwm-class-name "")) ": ")))))

      (defun ivy-switch-buffer-within-app ()
        "Switch to another buffer within application.."
        (interactive)
        (ivy-read "Switch to buffer: " #'internal-complete-buffer
                  :keymap ivy-switch-buffer-map
                  :preselect (buffer-name (other-buffer (current-buffer)))
                  :action #'ivy--switch-buffer-action
                  :matcher #'ivy--switch-buffer-matcher
                  :caller 'ivy-switch-buffer
                  :initial-input (and exwm-class-name
                                      (concat (downcase (or exwm-class-name "")) ": ")))))

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
               '(((member exwm-class-name '("Firefox"))
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
        (buf-move-down)
        (redisplay)
        (windmove-up))

      (defun exwm-split-window-right (&optional size)
        ;; https://github.com/ch11ng/exwm/issues/685#issuecomment-879903947
        (interactive "P")
        (split-window-right size)
        (buf-move-right)
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
        (key-chord-define-global "qj" 'ivy-switch-buffer)
        (key-chord-define-global "qd" 'exwm-my-workspace-prefix-map)

        (global-set-key (kbd "C-x 2") 'exwm-split-move-window-below)
        (global-set-key (kbd "C-x 3") 'exwm-split-move-window-right)

        (global-set-key (kbd "C-x 8") 'exwm-split-move-window-below)
        (global-set-key (kbd "C-x 7") 'exwm-split-move-window-right))

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

          (defvar exwm-my-workspace-start-number 1)
          (assert (member exwm-my-workspace-start-number '(0 1))) ; should be 0 or 1
          (setq exwm-workspace-index-map
                (lambda (index) (number-to-string (+ exwm-my-workspace-start-number index)))))

        (setq exwm-environment-switch-create
              (if (machine-config-get-first 'exwm-multiple-monitor-layout-type)
                  'exwm-workspace-group-switch-create
                'exwm-workspace-switch-create))

        (comment
          (dotimes (workspace-num 9)
            (lexical-let ((idx (% (+ workspace-num 10 (- exwm-my-workspace-start-number)) 10)))
              (define-key exwm-my-workspapce-prefix-map (kbd (format "%d" workspace-num))
                #'(lambda () (interactive)
                    (funcall exwm-environment-switch-create idx))))))

        (progn
          (assert (not (get 'exwm-input-global-keys 'saved-value)))
          (fset 'help-map help-map)

          (setq exwm-input-global-keys
                (append
                 '(;; ([?\s-r] . exwm-reset)
                   ;; ([?\s-t] . exwm-floating-toggle-floating)

                   ([?\s-&] . exwm-my-command-execute-shell)
                   ([?\s-m] . exwm-my-command-prefix-map)
                   ([?\s-d] . exwm-my-workspace-prefix-map)

                   ([?\s-q] . ctl-x-map)
                   ([?\s-x] . (lambda () (interactive) (funcall (key-binding (kbd "M-x")))))
                   ([?\s-w] . tab-prefix-map)
                   ([?\s-e] . my-ctl-c-map)
                   ([?\s-h] . help-map)
                   ([?\s-u] . universal-argument)

                   ([?\s-l] . counsel-find-file)
                   ;; ([?\s-l] . find-file)
                   ([?\s-j] . ivy-switch-buffer)
                   ([?\s-k] . kill-buffer)
                   ([?\C-\s-j] . ivy-switch-buffer-within-app)
                   ;; ([?\s-B] . counsel-switch-buffer-within-app)
                   ([?\C-\s-b] . counsel-switch-buffer-within-app)
                   ([?\s-!] . shell-command)

                   ([?\s-9] . previous-buffer)
                   ([?\s-0] . next-buffer)

                   ([?\s-i] . other-window-backwards)
                   ([?\s-o] . other-window)
                   ([?\s-p] . tab-previous)
                   ([?\s-n] . tab-next))

                 '(([S-s-up] . volume-raise-10)
                   ([S-s-down] . volume-lower-10))
                 
                 (if (machine-config-get-first 'exwm-multiple-monitor-layout-type)
                     '(([?\C-\s-i] . exwm-other-workspace-in-group-backwards)
                       ([?\C-\s-o] . exwm-other-workspace-in-group)
                       ([?\C-\s-p] . exwm-workspace-group-switch-previous-group)
                       ([?\C-\s-n] . exwm-workspace-group-switch-next-group))
                   '(([?\C-\s-i] . exwm-other-workspace-backwards)
                     ([?\C-\s-o] . exwm-other-workspace)
                     ([?\C-\s-p] . exwm-other-workspace-backwards)
                     ([?\C-\s-n] . exwm-other-workspace)))

                 `(;; 's-N': Switch to certain workspace.
                   ,@(mapcar (lambda (i)
                               `(,(kbd (format "s-%d" i)) .
                                 (lambda ()
                                   (interactive)
                                   (,exwm-environment-switch-create
                                    ,(% (+ i 10 (- exwm-my-workspace-start-number)) 10)))))
                             (number-sequence 1 7)))

                 (comment
                   `(;; 's-N': Switch to certain workspace.
                     ,@(mapcar (lambda (i)
                                 `(,(kbd (format "s-%d" i)) .
                                   (lambda ()
                                     (interactive)
                                     (,exwm-environment-switch-create
                                      ,(% (+ i 10 (- exwm-my-workspace-start-number)) 10)))))
                               (number-sequence 0 9))))))))

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

                  ([?\C-/] . [?\C-z])
                  ([?\C-?] . [?\C-y]))))

        (progn
          ;; global bindings
          (comment (setq exwm-input-simulation-keys exwm-base-input-simulation-keys))
          (progn
            ;; disable 'C-c' prefix
            (setq exwm-input-simulation-keys '(([?\C-c] . [?\C-c])))))

        (progn
          ;; local bindings

          (comment
            (add-hook 'exwm-manage-finish-hook
                      (lambda ()
                        (when (and exwm-class-name
                                   (string= exwm-class-name "Nyxt"))
                          (exwm-input-set-local-simulation-keys

                           '(([?\C-p] . [up])
                            ([?\C-n] . [down])))))))

          (add-hook 'exwm-manage-finish-hook
                    (lambda ()
                      (when (and exwm-class-name
                                 (string= exwm-class-name "Firefox"))
                        (exwm-input-set-local-simulation-keys
                         (append
                          exwm-base-input-simulation-keys
                          '(([?\C-s] . [?\C-f])
                            ([?\C-g] . [escape])
                            ([?\M-p] . [S-f3])
                            ([?\M-n] . [f3])
                            ;; ([?\M-p] . [C-prior])
                            ;; ([?\M-n] . [C-next])
                            ([?\M-\[] . [M-left])
                            ([?\M-\]] . [M-right])

                            ([?\C-9] . [M-left])
                            ([?\C-0] . [M-right])
                            ([?\M-9] . [C-prior])
                            ([?\M-0] . [C-next])
                            )
                          )))))
          (comment
            (add-hook 'exwm-manage-finish-hook
                      (lambda ()
                        (when (and exwm-class-name
                                   (string= exwm-class-name "Google-chrome"))
                          (exwm-input-set-local-simulation-keys
                           (append
                            exwm-base-input-simulation-keys
                            '(([?\M-p] . [C-prior])
                              ([?\M-n] . [C-next])
                              ([?\M-\[] . [M-left])
                              ([?\M-\]] . [M-right]))))))))
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
        (comment (define-key exwm-mode-map (kbd "M-&") 'exwm-my-execute-shell-command))
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
