
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

  (comment (key-chord-define-global "o2" 'dhnam/ansi-term)))

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
  ;; $ conda install -y -c anaconda cmake
  ;; $ conda install -y -c conda-forge libtool
  ;; $ conda install -y gxx_linux-64

  (progn
    ;; async-shell-command and signal example:
    ;; https://emacs.stackexchange.com/a/42174

    (defun dhnam/run-vterm-when-signal (process signal)
      (when (memq (process-status process) '(exit signal))
        (vterm)
        (shell-command-sentinel process signal)))


    (defun dhnam/vterm-setup ()
      (interactive)
      (let* ((output-buffer (generate-new-buffer "*Async shell command*"))
             (proc (progn
                     (dhnam/display-async-shell-command
                      (async-shell-command (concat "conda install -y -c anaconda cmake" " && "
                                                   "conda install -y -c conda-forge libtool" " && "
                                                   "conda install -y gxx_linux-64")
                                           output-buffer))
                     (get-buffer-process output-buffer))))
        (if (process-live-p proc)
            (set-process-sentinel proc #'dhnam/run-vterm-when-signal)
          (vterm)))))

  (use-existing-pkg vterm
    :bind
    (nil
     :map vterm-mode-map
     ("C-_" . vterm-undo)

     ;; :map vterm-copy-mode-map
     ;; ("C-c C-k" . vterm-copy-mode-done)
     ("C-c C-k" . vtsl/deactivate)
     ("C-c C-j" . vtsl/activate))

    :commands (dhnam/vterm-new-instance)
    :init
    (progn
      (key-chord-define-global "o2" 'dhnam/vterm-new-instance))

    :config
    (progn
      ;; https://github.com/akermu/emacs-libvterm/issues/352#issuecomment-647913789
      (setq vterm-max-scrollback 100000))


    (progn
      (defun dhnam/vterm-new-instance (arg)
        (interactive "P")
        (let ((default-directory (if arg
                                     (read-directory-name "Directory: ")
                                   default-directory)))
          (vterm t)))

      (progn
        (require 'vterm-seamless)
        (comment (add-hook 'vterm-mode-hook 'vtsl/activate))

        (progn
          (defun dhnam/vterm-new-instance-advice (orig-fun &rest args)
            (let ((buf (apply orig-fun args)))
              (with-current-buffer buf
                (vtsl/activate))
              buf))

          (advice-add 'dhnam/vterm-new-instance
                      :around 'dhnam/vterm-new-instance-advice)))

      ;; Interactive functions
      (defun dhnam/vterm-insert-tty-fix-template ()
        ;; fix for vterm when opened via ssh-tramp
        ;; https://github.com/akermu/emacs-libvterm/issues/569
        ;; https://unix.stackexchange.com/questions/404173/shell-command-tmux-throws-cant-use-dev-tty-error/512979

        (interactive)
        (vterm-insert "( exec </dev/tty; exec <&1; )")
        (vterm-send-left))

      (defun vtsl/vterm-send-ctrl-r ()
        "Send `C-r' to the libvterm."
        (interactive)
        (vterm-send-key "r" nil nil t))

      (defun vtsl/vterm-send-ctrl-s ()
        "Send `C-s' to the libvterm."
        (interactive)
        (vterm-send-key "s" nil nil t))

      (defun vtsl/vterm-send-ctrl-c ()
        "Send `C-c' to the libvterm."
        (interactive)
        (vterm-send-key "c" nil nil t))

      (let ((map vterm-seamless-mode-map))
        (define-key map (kbd "C-z")           #'vterm-send-next-key)
        (define-key map (kbd "C-;")           #'vterm-send-next-key)
        (comment (define-key map (kbd "C-c C-j")       #'vterm-copy-mode))
        (define-key map (kbd "C-t")           #'dhnam/vterm-insert-tty-fix-template)
        ;; (define-key map (kbd "C-c c")         #'dhnam/vterm-send-conda-activate-env)
        ;; (define-key map (kbd "C-c C")         #'dhnam/vterm-send-conda-deactivate)
        (define-key map (kbd "M-9")           #'previous-buffer)
        (define-key map (kbd "M-0")           #'next-buffer)
        (define-key map (kbd "M-L")           #'dhnam/reverse-recenter-top-bottom)
        (define-key map (kbd "C-v")           #'dhnam/scroll-up-small)
        (define-key map (kbd "M-v")           #'dhnam/scroll-down-small)
        (define-key map (kbd "<f7>")          #'pop-to-mark-command)
        (define-key map (kbd "<f8>")          #'dhnam/unpop-to-mark-command)
        ;; (define-key map (kbd "C-r")           (vtsl/copy-mode-then 'isearch-backward))
        ;; (define-key map (kbd "C-s")           (vtsl/copy-mode-then 'isearch-forward))
        (define-key map (kbd "₣")             (vtsl/copy-mode-then 'avy-goto-char-timer))
        ;; (define-key map (kbd "M-r")           #'vtsl/vterm-send-ctrl-r)
        ;; (define-key map (kbd "M-s")           #'vtsl/vterm-send-ctrl-s)
        (define-key map (kbd "M-P")           #'vtsl/vterm-send-ctrl-r)
        (define-key map (kbd "M-N")           #'vtsl/vterm-send-ctrl-s)
        (define-key map (kbd "C-g")           #'vtsl/vterm-send-ctrl-c)

        (define-key map (kbd "C-c C-d")       #'pdb-tracking-mode)

        ;; key-chords
        (key-chord-define map "wj" 'vterm-copy-mode)
        (key-chord-define map "w;" 'vterm-send-next-key)
        (key-chord-define map "sj" (vtsl/copy-mode-then 'dhnam/swiper-within-region))
        (key-chord-define map "fj" (vtsl/copy-mode-then 'ctrlf-backward-default)))


      (setq pdb-tracking/check-python-shell-prompt-pdb-regexp nil)

      (progn
        (require 'pdb-tracking)

        (key-chord-define pdb-tracking-mode-map "r;" 'pdb-tracking/search-backward-pdbpp-arrow)

        (defun pdb-tracking/search-backward-pdbpp-arrow-advice-for-vterm (orig-fun &rest args)
          (vterm-copy-mode)
          (apply orig-fun args))

        (advice-add 'pdb-tracking/search-backward-pdbpp-arrow
                    :around 'pdb-tracking/search-backward-pdbpp-arrow-advice-for-vterm)))))

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

  (when (display-graphic-p)
    (key-chord-define-global "o3" 'dhnam/gui-terminal)))


(provide 'init-term)
