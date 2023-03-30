(comment
  (autoload 'save-current-configuration "revive" "Save status" t)
  (autoload 'resume "revive" "Resume Emacs" t)
  (autoload 'wipe "revive" "Wipe Emacs" t)

  (unless (file-exists-p "~/.emacs.d/layout/")
    ;; https://stackoverflow.com/questions/17376706/in-emacs-lisp-how-can-i-append-a-string-to-a-file-that-i-dont-like-to-open
    ;; https://stackoverflow.com/questions/14071991/how-to-create-an-empty-file-by-elisp/14072295
    (make-directory "~/.emacs.d/layout/"))

  (defun dhnam/revive-layout-read-args (prompt mustmatch)
    (list (read-file-name prompt "~/.emacs.d/layout/" nil mustmatch)))

  (defun dhnam/emacs-save-layout (out-name)
    "save the frame and window layout to ~/.layout. Requires revive.el."
    (interactive
     (dhnam/revive-layout-read-args "Save the layout into: "
			                        (confirm-nonexistent-file-or-buffer)))
    (setq revive:configuration-file out-name)
    (save-current-configuration))

  (defun dhnam/emacs-load-layout (in-name)
    "Load the layout saved by emacs-save-layout. Requires revive.el."
    (interactive
     (dhnam/revive-layout-read-args "Load the layout from: "
			                        (confirm-nonexistent-file-or-buffer)))
    (setq revive:configuration-file in-name)
    (resume))

  (global-set-key (kbd "C-c S") 'dhnam/emacs-save-layout)
  (global-set-key (kbd "C-c F") 'dhnam/emacs-load-layout))

(provide 'init-revive-config)
