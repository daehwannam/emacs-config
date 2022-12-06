
(require 'named-term nil t)

(when (require 'multi-term nil t)
  (defalias 'mterm 'multi-term)

  (defun tmux-term-1 ()
    "Create new term buffer, then do tmux new session.
It's modified code of 'multi-term.
Will prompt you shell name when you type `C-u' before this command."
    (interactive)
    (let (term-buffer)
      ;; Set buffer.
      (setq term-buffer (multi-term-get-buffer current-prefix-arg))
      (setq multi-term-buffer-list (nconc multi-term-buffer-list (list term-buffer)))
      (set-buffer term-buffer)
      ;; Internal handle for `multi-term' buffer.
      (multi-term-internal)
      ;; Switch buffer
      (switch-to-buffer term-buffer))
    (term-send-raw-string "tmux new \C-m"))

  (cl-defun multi-term-tmux-sessions (&optional (user+host nil))
    "multi-term-tmux code is used: https://github.com/beyondmetis/multi-term-tmux"
    (interactive)
    (if user+host
	(let* ((sessionlist (shell-command-to-string (concat "ssh " user+host " -q -t -t tmux list-sessions"))) (sessionlist (split-string sessionlist "\n" t)) (sesslist nil))
	  (dolist (elt sessionlist)
	    (setq sessname (split-string elt ":" t))
	    (setq sessname (nth 0 sessname))
	    (setq sesslist (append sesslist (list sessname))))
	  sesslist)

      (let* ((sessionlist (shell-command-to-string "tmux list-sessions")) (sessionlist (split-string sessionlist "\n" t)) (sesslist nil))
	(dolist (elt sessionlist)
	  (setq sessname (split-string elt ":" t))
	  (setq sessname (nth 0 sessname))
	  (setq sesslist (append sesslist (list sessname))))
	sesslist)))

  (defun multi-term-get-tmux-buffer (session-name &optional special-shell dedicated-window)
    "Get term buffer.
If option SPECIAL-SHELL is `non-nil', will use shell from user input.
If option DEDICATED-WINDOW is `non-nil' will create dedicated `multi-term' window ."
    (with-temp-buffer
      (let ((shell-name (or multi-term-program ;shell name
			    (getenv "SHELL")
			    (getenv "ESHELL")
			    "/bin/sh"))
	    (index 1)                     ;setup new term index
	    term-name)                    ;term name
	(if dedicated-window
	    (setq term-name multi-term-dedicated-buffer-name)
	  ;; Compute index.
	  (while (buffer-live-p (get-buffer (format "*%s<%s>*" multi-term-buffer-name index)))
	    (setq index (1+ index)))
	  ;; switch to current local directory,
	  ;; if in-existence, switch to `multi-term-default-dir'.
	  (cd (or default-directory (expand-file-name multi-term-default-dir)))
	  ;; adjust value N when max index of term buffer is less than length of term list
	  (setq term-name (format "%s<%s>" multi-term-buffer-name index)))
	;; Try get other shell name if `special-shell' is non-nil.
	(if special-shell
	    (setq shell-name (read-from-minibuffer "Run program: " shell-name)))
	;; Make term, details to see function `make-term' in `term.el'.
	(if session-name
	    (if (member session-name (multi-term-tmux-sessions))
		(make-term term-name "tmux" nil "-L" "emacs-term" "a" "-t" session-name)
	      (make-term term-name "tmux" nil "-L" "emacs-term" "new" "-s" session-name))
	  (make-term term-name "tmux" nil "-L" "emacs-term" "new"))
	;; (if multi-term-program-switches
	;;     (make-term term-name shell-name nil multi-term-program-switches)
	;;   (make-term term-name shell-name))
	)))

  (defun tmux-term (&optional session-name)
    "Create new term buffer, then do tmux new session.
It's modified code of 'multi-term.
Will prompt you shell name when you type `C-u' before this command."
    (interactive (list (read-from-minibuffer "Session name: ")))
    (when (string= session-name "")
      (setq session-name nil))
    (let (term-buffer)
      ;; Set buffer.
      (setq term-buffer (multi-term-get-tmux-buffer session-name current-prefix-arg))
      (setq multi-term-buffer-list (nconc multi-term-buffer-list (list term-buffer)))
      (set-buffer term-buffer)
      ;; Internal handle for `multi-term' buffer.
      (multi-term-internal)
      ;; Switch buffer
      (switch-to-buffer term-buffer)))

  (defalias 'tterm 'tmux-term)
  ;; (require 'multi-term-tmux)
  ;; (defalias 'tterm 'multi-term-tmux-open)

  (defun tmux-send-toggle-logging-cmd ()
    (interactive)
    (term-send-raw-string
     "tmux -L emacs-term run-shell ~/.tmux/plugins/tmux-logging/scripts/toggle_logging.sh")
    (term-send-return))

  (comment
   ;; 'tmux-term-with-logging is not working
   (defun tmux-term-with-logging (&optional session-name)
     "Create new term buffer, then do tmux new session.
It's modified code of 'multi-term.
Will prompt you shell name when you type `C-u' before this command."
     (interactive (list (read-from-minibuffer "Session name: ")))
     (when (string= session-name "")
       (setq session-name nil))
     (let (term-buffer)
       ;; Set buffer.
       (setq term-buffer (multi-term-get-tmux-buffer session-name current-prefix-arg))
       (setq multi-term-buffer-list (nconc multi-term-buffer-list (list term-buffer)))
       (set-buffer term-buffer)
       ;; Internal handle for `multi-term' buffer.
       (multi-term-internal)
       ;; Switch buffer
       (switch-to-buffer term-buffer))
     (tmux-send-toggle-logging-cmd))

   (defalias 'tterm-logging 'tmux-term-with-logging))

  ;; tmux command
  ;;
  ;; string join: https://stackoverflow.com/questions/12999530/is-there-a-function-that-joins-a-string-into-a-delimited-string
  (defun tmux-kill-numbered-sessions ()
    (interactive)
    (let ((session-names (split-string (trim-string (shell-command-to-string
						     "tmux ls | grep '^[0-9][0-9]*: ' | awk '{print substr($1, 0, length($1))}' | xargs echo")))))
      (dolist (session-name session-names)
	(shell-command (concat "tmux kill-session -t " session-name)))
      (message (concat "killed sessions: " (mapconcat 'identity session-names ", ")))))

  (defun tmux-kill-all-emacs-term-sessions ()
    (interactive)
    (shell-command "tmux -L emacs-term kill-server")))

(progn
  ;; Enable opening terminal with tramp via ssh
  ;; https://emacs.stackexchange.com/a/69784

  (defvar dhnam/ansi-term-char-mode-as-default t)

  (defun dhnam/ansi-term (&optional path name)
    "Opens a terminal at PATH. If no PATH is given, it uses
the value of `default-directory'. PATH may be a tramp remote path.
The term buffer is named based on `name' "
    (interactive)
    (require 'term)
    (unless path (setq path default-directory))
    (unless name (setq name (generate-new-buffer-name "*ansi-term*")))
    (let ((path (replace-regexp-in-string "^file:" "" path))
          (cd-str "fn=%s; if test ! -d $fn; then fn=$(dirname $fn); fi; cd $fn; exec bash")
          (start-term (lambda (termbuf)
                        (progn
                          (set-buffer termbuf)
                          (term-mode)
                          (if dhnam/ansi-term-char-mode-as-default
                              (term-char-mode)
                            (term-line-mode))
                          (switch-to-buffer termbuf)))))
      (if (tramp-tramp-file-p path)
          (let* ((tstruct (tramp-dissect-file-name path))
                 (cd-str-ssh (format cd-str (tramp-file-name-localname tstruct)))
                 (user (or (tramp-file-name-user tstruct) user-login-name))
                 (switches (list "-l" user
                                 "-t" (tramp-file-name-host tstruct)
                                 cd-str-ssh))
                 (termbuf (apply 'term-ansi-make-term name "ssh" nil switches)))
            (cond
             ((equal (tramp-file-name-method tstruct) "ssh")
              (funcall start-term termbuf))
             (t (error "not implemented for method %s"
                       (tramp-file-name-method tstruct)))))
        (let* ((cd-str-local (format cd-str path))
               (termbuf (apply 'term-ansi-make-term name "/bin/sh" nil (list "-c" cd-str-local))))
          (funcall start-term termbuf)))))

  (key-chord-define-global "o2" 'dhnam/ansi-term))

(progn
  ;; char mode binding
  (define-key term-raw-map (kbd "M-x") (key-binding (kbd "M-x")))
  (comment
    term-raw-map
    ;; this doesn't work
    (add-to-list 'term-bind-key-alist `("M-x" . ,(key-binding (kbd "M-x"))))))

(progn

  (defun dhnam/move-beginning-of-command-line ()
    (interactive)
    (let* ((line-begin-point (save-excursion (move-beginning-of-line 1) (point)))
           (input-begin-point (save-excursion
                                (comment (move-end-of-line 1))
                                (re-search-backward "[$\n]" nil t)
                                (forward-char 2)
                                (point)))
           (input-end-point (save-excursion (move-end-of-line 1) (point))))
      (goto-char
       (if (< (+ line-begin-point 1) input-begin-point)
           (if (<= input-begin-point input-end-point)
               input-begin-point
             input-end-point)
         line-begin-point))))

  (defun dhnam/term-previous-prompt ()
    (interactive)
    (let ((original-point (point)))
      (move-beginning-of-line 1)
      (if (re-search-backward "\\$" nil t)
          (forward-char 2)
        (goto-char original-point))))

  (defun dhnam/term-next-prompt ()
    (interactive)
    (let ((original-point (point)))
      (if (re-search-forward "\\$" nil t)
          (forward-char 1)
        (goto-char original-point))))

  ;; line-mode bindings
  (define-key term-mode-map (kbd "C-a") 'dhnam/move-beginning-of-command-line)
  (define-key term-mode-map (kbd "C-c C-p") 'dhnam/term-previous-prompt)
  (define-key term-mode-map (kbd "C-c C-n") 'dhnam/term-next-prompt)
  (define-key term-mode-map (kbd "M-P") 'term-previous-matching-input-from-input)
  (define-key term-mode-map (kbd "M-N") 'term-next-matching-input-from-input))

(progn
  ;; vterm
  ;; https://github.com/akermu/emacs-libvterm
  ;;
  ;; * Requirements
  ;;
  ;; - GNU Emacs (>= 25.1) with module support. You can check that, by verifying that module-file-suffix is not nil.
  ;; - cmake (>= 3.11)
  ;; - libtool-bin (related issues: #66 #85)
  ;;
  ;; Install with apt:
  ;; $ sudo apt install cmake libtool-bin
  ;;
  ;; Install with conda:
  ;; $ conda install -c anaconda cmake
  ;; $ conda install -c conda-forge libtool
  ;; $ conda install gxx_linux-64

  (use-existing-pkg vterm
    :bind
    (nil
     :map vterm-mode-map
     ("C-z" . vterm-send-next-key)
     ("C-;" . vterm-send-next-key)
     ("C-c C-j" . vterm-copy-mode)
     ("C-t" . dhnam/vterm-insert-tty-fix-template)
     ("C-c c" . dhnam/vterm-insert-conda-activate-env)
     ("M-9" . previous-buffer)
     ("M-0" . next-buffer)
     ("C-c C-d" . pdb-tracking-mode)
     ;; ("C-k" . vterm--self-insert)
     ("C-k" . dhnam/vterm-kill-line)
     ("C-_" . vterm-undo)

     :map vterm-copy-mode-map
     ;; ("C-c C-k" . vterm-copy-mode-done)
     ("C-c C-k" . dhnam/vterm-copy-mode-exit)
     ("RET" . dhnam/vterm-copy-mode-exit)
     ("C-a" . dhnam/move-beginning-of-command-line)
     ("C-c C-p" . dhnam/term-previous-prompt)
     ("C-c C-n" . dhnam/term-next-prompt)
     ("M->" . vterm-reset-cursor-point)
     ;; ([remap self-insert-command] . vterm--self-insert)
     )

    :init
    (key-chord-define-global "o3" 'dhnam/vterm-new-instance)

    ;; :hook
    ;; (vterm-copy-mode . disable-read-only-mode)

    (defun dhnam/vterm-new-instance ()
      (interactive)
      (vterm t))

    :config
    (key-chord-define vterm-mode-map "wj" 'vterm-copy-mode)

    (defun dhnam/vterm-insert-tty-fix-template ()
      ;; fix for vterm when opened via ssh-tramp
      ;; https://github.com/akermu/emacs-libvterm/issues/569
      ;; https://unix.stackexchange.com/questions/404173/shell-command-tmux-throws-cant-use-dev-tty-error/512979

      (interactive)
      (vterm-insert "( exec </dev/tty; exec <&1; )")
      (vterm-send-left))

    (defun dhnam/vterm-insert-conda-activate-env ()
      (interactive)
      (vterm-insert "conda activate "
                    (completing-read "Environment name: " (pyvenv-virtualenv-list)
                                     nil t nil 'pyvenv-workon-history nil nil)))

    (defun dhnam/vterm-copy-mode-exit ()
      (interactive)
      "Exit `vterm-copy-mode'"
      (vterm-copy-mode -1))

    (defun dhnam/vterm-kill-line ()
      (interactive)
      ;; https://www.emacswiki.org/emacs/CopyingWholeLines
      (let ((beg (point))
            (end (line-end-position)))
        (kill-ring-save beg end))
      (vterm--self-insert))

    (comment
      (defun disable-read-only-mode ()
        (read-only-mode 0)))))

(progn
  ;; https://gist.github.com/dfeich/50ee86c3d4338dbc878b

  (defvar dhnam/gui-terminal-command "kitty")
  (defvar dhnam/gui-terminal-command-arg "-e" )

  (defun dhnam/gui-terminal (&optional path)
    "Opens a gnome terminal at PATH. If no PATH is given, it uses
the value of `default-directory'. PATH may be a tramp remote path."
    (interactive)
    (unless path (setq path default-directory))
    (if (tramp-tramp-file-p path)
        (let ((tstruct (tramp-dissect-file-name path)))
	      (cond
	       ((equal (tramp-file-name-method tstruct) "ssh")
	        (start-process "terminal" nil
                           dhnam/gui-terminal-command
                           dhnam/gui-terminal-command-arg
			               "--" "bash" "-c"
			               (format "ssh -t %s@%s -p %s 'cd %s; exec bash'; exec bash"
                                   (tramp-file-name-user-domain tstruct)
				                   (tramp-file-name-host tstruct)
                                   (or (tramp-file-name-port-or-default tstruct) 22)
				                   (tramp-file-name-localname tstruct))))
	       (t (error "not implemented for method%s"
		             (tramp-file-name-method tstruct)))))
      (start-process "terminal" nil dhnam/gui-terminal-command)))

  (key-chord-define-global "o4" 'dhnam/gui-terminal))

(progn
  (defun insert-conda-activate-env ()
    (interactive)
    (insert "conda activate "
            (completing-read "Environment name: " (pyvenv-virtualenv-list)
                             nil t nil 'pyvenv-workon-history nil nil)))

  (define-key shell-mode-map (kbd "C-c c") 'insert-conda-activate-env)
  (define-key term-mode-map (kbd "C-c c") 'insert-conda-activate-env))
