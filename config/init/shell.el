
;; http://stackoverflow.com/questions/2472273/how-do-i-run-a-sudo-command-in-emacs

(defun sudo-shell-command (command)
  (interactive "MShell command (root): ")
  (shell-command (concat "echo " (shell-quote-argument (read-passwd "Password? "))
                       " | sudo -S " command))
)

;(defun sudo-shell-command (command)
;  (interactive "MShell command (root): ")
;  (with-temp-buffer
;    (cd "/sudo::/")
;    (async-shell-command command)))

(global-set-key (kbd "C-c M-!") 'sudo-shell-command)

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
     (defun shell-same-window-advice (orig-fun &rest args)
       (apply orig-fun args)
       (universal-argument-more 1))

     (advice-add 'shell :around #'shell-same-window-advice)))

  (comment
   (defun shell-same-window-advice (orig-fun &rest args)
     (let ((prev-window (selected-window))
	   (num-windows (count-windows)))
       (apply orig-fun args)
       (when (not (equal (count-windows) num-windows))
	 (delete-window))
       (let ((shell-buffer (current-buffer)))
	 (previous-buffer)
	 (select-window prev-window)
	 (switch-to-buffer shell-buffer))))

   (advice-add 'shell :around #'shell-same-window-advice))

  (comment
   ;; below link contains a more general method which is applied without matching buffer names
   ;; https://stackoverflow.com/a/40351851/6710003
   )

  (comment
   (defun shell-same-window-advice (orig-fun &rest args)
     (split-window-right)
     (apply orig-fun args)
     (other-window -1)
     (delete-window)
     (other-window 1))

   (advice-add 'shell :around #'shell-same-window-advice))

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

;;; shell for virtual env or conda
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

(defun source-shell (&optional buffer env-name)
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
  ;; (insert (concat "conda activate " (machine-config-get 'pyvenv-name)))
  (insert (concat "source activate " env-name))
  (comint-send-input))

(defun conda-shell (&optional buffer env-name)
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
  (compilation-shell-minor-mode) ; for compliation mode's features
  ;; (insert (concat "conda activate " (machine-config-get 'pyvenv-name)))
  (insert (concat "conda activate " env-name))
  (comint-send-input))

(progn
  (key-chord-define shell-mode-map "fp" 'comint-previous-matching-input-from-input)
  (key-chord-define shell-mode-map "fn" 'comint-next-matching-input-from-input))

(progn
  (define-key compilation-shell-minor-mode-map (kbd "C-M-]") 'compilation-next-error)
  (define-key compilation-shell-minor-mode-map (kbd "C-M-[") 'compilation-previous-error)
  (define-key compilation-shell-minor-mode-map (kbd "C-M-n") 'forward-list)
  (define-key compilation-shell-minor-mode-map (kbd "C-M-p") 'backward-list))

(progn
  (defun comint-previous-input-or-compilation-previous-error (arg)
    "Cycle backwards through input history, saving input, or move
point to the previous error in the compilation buffer"
    (interactive "*p")
    (if (comint-after-pmark-p)
	(comint-previous-input arg)
      (compilation-previous-error arg)))

  (defun comint-next-input-or-compilation-next-error (arg)
    "Cycle forwards through input history, saving input, or move
point to the next error in the compilation buffer"
    (interactive "*p")
    (if (comint-after-pmark-p)
	(comint-next-input arg)
      (compilation-next-error arg)))

  (add-hook 'compilation-shell-minor-mode-hook
	    (lambda () (local-set-key (kbd "M-p") 'comint-previous-input-or-compilation-previous-error)))

  (add-hook 'compilation-shell-minor-mode-hook
	    (lambda () (local-set-key (kbd "M-n") 'comint-next-input-or-compilation-next-error))))

(defun py3-shell (&optional buffer)
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

(defun py2-shell (&optional buffer)
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
  (comint-send-input))
