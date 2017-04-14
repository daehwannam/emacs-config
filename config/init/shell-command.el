
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
