
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
(defun tf-shell (&optional buffer)
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
  (insert "source activate tf3")
  (comint-send-input))