
(progn
  ;; open a shell in the current window.
  (comment
    ;; https://stackoverflow.com/a/46122387
    ;; WARNING: this change the behavior of 'pdbtrace, so pdbtrace doesn't show the indicated source code
    (push (cons "\\*shell\\*" display-buffer--same-window-action) display-buffer-alist))

  (progn
    ;; [WARNING]
    ;; The first call of command 'other-window-repeat may not work after a shell is created
    ;; (Also, other commands made by 'make-repeatable-command dont' work)
    ;; It's because of the prifix argument (C-u), whose value is 4 as default.
    ;; So, other-window-repeat repeats 4 times.
    (add-to-list 'display-buffer-alist
		         '("\\*shell\\*" . (display-buffer-same-window)))
    (comment
      ;; [THIS CODE IS NOT WORKING]
      ;; trick to consume the prefix argument by advice
      (defun dhnam/shell-same-window-advice (orig-fun &rest args)
        (apply orig-fun args)
        (universal-argument-more 1))

      (advice-add 'shell :around #'dhnam/shell-same-window-advice)))

  (comment
    (defun dhnam/shell-same-window-advice (orig-fun &rest args)
      (let ((prev-window (selected-window))
	        (num-windows (count-windows)))
        (apply orig-fun args)
        (when (not (equal (count-windows) num-windows))
	      (delete-window))
        (let ((shell-buffer (current-buffer)))
	      (previous-buffer)
	      (select-window prev-window)
	      (switch-to-buffer shell-buffer))))

    (advice-add 'shell :around #'dhnam/shell-same-window-advice))

  (comment
    ;; below link contains a more general method which is applied without matching buffer names
    ;; https://stackoverflow.com/a/40351851/6710003
    )

  (comment
    (defun dhnam/shell-same-window-advice (orig-fun &rest args)
      (split-window-right)
      (apply orig-fun args)
      (other-window -1)
      (delete-window)
      (other-window 1))

    (advice-add 'shell :around #'dhnam/shell-same-window-advice))

  (comment
    ;; another option is replacing 'pop-to-buffer with 'pop-to-buffer-same-window in 'shell
    ;; however, it has the same problem with prefix argument
    (comment
      (defun shell (...)
        ...
        (progn
	      ;; [dhnam]
	      ;; shell is oppend in the current window 
	      (comment (pop-to-buffer buffer))
	      (pop-to-buffer-same-window buffer))
        ...))))

(comment
  (defun dhnam/sudo-shell-command (command)
    (interactive "MShell command (root): ")
    (with-temp-buffer
      (cd "/sudo::/")
      (async-shell-command command))))

(progn
  ;; shell for virtual env or conda
  ;;
  ;; original simple code
  ;;
  ;; (defun tf-shell (&optional buffer)
  ;;   (interactive)
  ;;   (let (buffer)
  ;;     (if current-prefix-arg
  ;;         (setq buffer (read-buffer "Shell buffer: ")))
  ;;     (shell buffer)
  ;;     (insert "source activate tensorflow")
  ;;     (comint-send-input)))

  ;; below is based on emacs "shell" function
  ;; https://github.com/emacs-mirror/emacs/blob/master/lisp/shell.el

  (defun dhnam/source-shell (&optional buffer env-name)
    (interactive
     (list
      (and current-prefix-arg
	       (prog1
	           (read-buffer "Shell buffer: "
			                ;; If the current buffer is an inactive
			                ;; shell buffer, use it as the default.
			                (if (and (eq major-mode 'shell-mode)
				                     (null (get-buffer-process (current-buffer))))
			                    (buffer-name)
			                  (generate-new-buffer-name "*shell*")))
	         (if (file-remote-p default-directory)
	             ;; It must be possible to declare a local default-directory.
	             ;; FIXME: This can't be right: it changes the default-directory
	             ;; of the current-buffer rather than of the *shell* buffer.
	             (setq default-directory
		               (expand-file-name
		                (read-directory-name
		                 "Default directory: " default-directory default-directory
		                 t nil))))))
      (read-string "Environment name: ")))
    (setq buffer (if (or buffer (not (derived-mode-p 'shell-mode))
		                 (comint-check-proc (current-buffer)))
		             (get-buffer-create (or buffer "*shell*"))
		           ;; If the current buffer is a dead shell buffer, use it.
		           (current-buffer)))
    (shell buffer)
    ;; (insert (concat "conda activate " (dhnam/machine-config-get-first 'pyvenv-name)))
    (insert (concat "source activate " env-name))
    (comint-send-input))

  (progn
    (defun dhnam/conda-shell (&optional buffer env-name)
      (interactive
       (list
        (and current-prefix-arg
	         (prog1
	             (read-buffer "Shell buffer: "
			                  ;; If the current buffer is an inactive
			                  ;; shell buffer, use it as the default.
			                  (if (and (eq major-mode 'shell-mode)
				                       (null (get-buffer-process (current-buffer))))
				                  (buffer-name)
			                    (generate-new-buffer-name "*shell*")))
	           (if (file-remote-p default-directory)
		           ;; It must be possible to declare a local default-directory.
		           ;; FIXME: This can't be right: it changes the default-directory
		           ;; of the current-buffer rather than of the *shell* buffer.
		           (setq default-directory
		                 (expand-file-name
			              (read-directory-name
			               "Default directory: " default-directory default-directory
			               t nil))))))
        (completing-read "Environment name: " (pyvenv-virtualenv-list)
                         nil t nil 'pyvenv-workon-history nil nil)))

      ;; completing-read is used instead of read-string
      ;; e.g. (read-string "Environment name: ")

      (setq buffer (if (or buffer (not (derived-mode-p 'shell-mode))
			               (comint-check-proc (current-buffer)))
		               (get-buffer-create (or buffer "*shell*"))
		             ;; If the current buffer is a dead shell buffer, use it.
		             (current-buffer)))
      (shell buffer)
      (compilation-shell-minor-mode)   ; for compliation mode's features
      ;; (insert (concat "conda activate " (dhnam/machine-config-get-first 'pyvenv-name)))
      (insert (concat "conda activate " env-name))
      (comint-send-input))

    (defun dhnam/conda-shell-with-default-name ()
      (interactive)
      (dhnam/conda-shell (generate-new-buffer-name "*shell*")
		                 (completing-read "Environment name: " (pyvenv-virtualenv-list)
				                          nil t nil 'pyvenv-workon-history nil nil)))

    (comment (key-chord-define-global "o2" 'dhnam/conda-shell-with-default-name))

    (defun dhnam/conda-shell-with-default-name-other-window (count)
      (interactive "p")
      split-window-sensibly
      (other-window count)
      (dhnam/conda-shell-with-default-name))

    (comment (key-chord-define-global "o4" 'dhnam/conda-shell-with-default-name-other-window)))

  (defun dhnam/py3-shell (&optional buffer)
    (interactive
     (list
      (and current-prefix-arg
	       (prog1
	           (read-buffer "Shell buffer: "
			                ;; If the current buffer is an inactive
			                ;; shell buffer, use it as the default.
			                (if (and (eq major-mode 'shell-mode)
				                     (null (get-buffer-process (current-buffer))))
			                    (buffer-name)
			                  (generate-new-buffer-name "*shell*")))
	         (if (file-remote-p default-directory)
	             ;; It must be possible to declare a local default-directory.
	             ;; FIXME: This can't be right: it changes the default-directory
	             ;; of the current-buffer rather than of the *shell* buffer.
	             (setq default-directory
		               (expand-file-name
		                (read-directory-name
		                 "Default directory: " default-directory default-directory
		                 t nil))))))))
    (setq buffer (if (or buffer (not (derived-mode-p 'shell-mode))
		                 (comint-check-proc (current-buffer)))
		             (get-buffer-create (or buffer "*shell*"))
		           ;; If the current buffer is a dead shell buffer, use it.
		           (current-buffer)))
    (shell buffer)
    ;; (insert "source activate py3")
    (insert "conda activate py3")
    (comint-send-input))

  (defun dhnam/py2-shell (&optional buffer)
    (interactive
     (list
      (and current-prefix-arg
	       (prog1
	           (read-buffer "Shell buffer: "
			                ;; If the current buffer is an inactive
			                ;; shell buffer, use it as the default.
			                (if (and (eq major-mode 'shell-mode)
				                     (null (get-buffer-process (current-buffer))))
			                    (buffer-name)
			                  (generate-new-buffer-name "*shell*")))
	         (if (file-remote-p default-directory)
	             ;; It must be possible to declare a local default-directory.
	             ;; FIXME: This can't be right: it changes the default-directory
	             ;; of the current-buffer rather than of the *shell* buffer.
	             (setq default-directory
		               (expand-file-name
		                (read-directory-name
		                 "Default directory: " default-directory default-directory
		                 t nil))))))))
    (setq buffer (if (or buffer (not (derived-mode-p 'shell-mode))
		                 (comint-check-proc (current-buffer)))
		             (get-buffer-create (or buffer "*shell*"))
		           ;; If the current buffer is a dead shell buffer, use it.
		           (current-buffer)))
    (shell buffer)
    ;; (insert "source activate py2")
    (insert "conda activate py2")
    (comint-send-input)))
