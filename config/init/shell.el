
(require 'dhnam-make-repeatable-command)

(require 'dhnam-shell)

(key-chord-define-global "o1" 'dhnam/shell-new-instance)
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


(comment
  (progn
    ;; Apply sh-mode for a specific file name
    (add-to-list 'auto-mode-alist '("\\.sbatch\\'" . sh-mode)))

  (progn
    ;; Apply sh-mode for a specific shebang
    ;; https://unix.stackexchange.com/a/15633
    (add-to-list 'interpreter-mode-alist '("sbatch" . sh-mode))))

(provide 'init-shell)
