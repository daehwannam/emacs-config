
(require 'dhnam-make-repeatable-command)

(defun dhnam/shell-new-instance ()
  (interactive)
  (shell (get-buffer-create (generate-new-buffer-name "*shell*"))))

(key-chord-define-global "o1" 'dhnam/shell-new-instance)

(defun dhnam/shell-new-instance-other-window (count)
  (interactive "p")
  (split-window-sensibly)
  (other-window count)
  (dhnam/shell-new-instance))

(comment (key-chord-define-global "o3" 'dhnam/shell-new-instance-other-window))

;; http://stackoverflow.com/questions/2472273/how-do-i-run-a-sudo-command-in-emacs
(defun dhnam/sudo-shell-command (command)
  (interactive "MShell command (root): ")
  (shell-command (concat "echo " (shell-quote-argument (read-passwd "Password? "))
                         " | sudo -S " command))
  )

(progn
  ;; hide output of 'async-shell-command
  ;; https://emacs.stackexchange.com/a/58341
  (add-to-list 'display-buffer-alist '("*Async Shell Command*" display-buffer-no-window (nil)))

  (defmacro dhnam/display-async-shell-command (&rest args)
    `(let ((display-buffer-alist (cons '("*Async Shell Command*" display-buffer-same-window)
                                       display-buffer-alist)))
       (progn ,@args))))

(global-set-key (kbd "C-c M-!") 'dhnam/sudo-shell-command)

(progn
  ;; open a shell in the current window.
  (add-to-list 'display-buffer-alist
		       '("\\*shell\\*" . (display-buffer-same-window))))

(defun dhnam/shell-with-command (command)
  "Run shell with COMMAND. It makes a temporary script to run the command."
  (interactive
   (list
    (read-shell-command (if shell-command-prompt-show-cwd
                            (format-message "Shell command in `%s': "
                                            (abbreviate-file-name
                                             default-directory))
                          "Shell command: ")
                        nil nil
			            (let ((filename
			                   (cond
				                (buffer-file-name)
				                ((eq major-mode 'dired-mode)
				                 (dired-get-filename nil t)))))
			              (and filename (file-relative-name filename))))))

  (let ((script-path (dhnam/string-trim (shell-command-to-string "mktemp /tmp/cmd-XXXXX"))))
    (with-temp-file script-path
      (dhnam/insert-line "#!/usr/bin/sh")
      (dhnam/insert-line (format "trap 'rm -f %s' EXIT" script-path))
      (dhnam/insert-line command))
    (shell-command (format "chmod +x %s" script-path))
    (let ((explicit-shell-file-name script-path))
      (dhnam/shell-new-instance))))

(comment
  (progn
    ;; Apply sh-mode for a specific file name
    (add-to-list 'auto-mode-alist '("\\.sbatch\\'" . sh-mode)))

  (progn
    ;; Apply sh-mode for a specific shebang
    ;; https://unix.stackexchange.com/a/15633
    (add-to-list 'interpreter-mode-alist '("sbatch" . sh-mode))))

(provide 'dhnam-shell)
