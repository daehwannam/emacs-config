;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; revive config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'restore-window-configuration "revive")
(autoload 'current-window-configuration-printable "revive")

(unless (file-exists-p "~/.emacs.d/layout/")
  ;; https://stackoverflow.com/questions/17376706/in-emacs-lisp-how-can-i-append-a-string-to-a-file-that-i-dont-like-to-open
  ;; https://stackoverflow.com/questions/14071991/how-to-create-an-empty-file-by-elisp/14072295
  (make-directory "~/.emacs.d/layout/"))

(defun layout-read-args (prompt mustmatch)
  (list (read-file-name prompt "~/.emacs.d/layout/" nil mustmatch)))

(require 'windows)
(require 'recentf)

;; -- Save place in file
(setq-default save-place t)

(defun windows-save-layout (out-name)
  "save layout by using windows.el"
  (interactive
   (layout-read-args "Save the layout into: "
			    (confirm-nonexistent-file-or-buffer)))
  (let ((default-file-name win:configuration-file))
    (setq win:configuration-file out-name)
    (win-save-all-configurations)
    (setq win:configuration-file default-file-name)))

(defun windows-load-layout (in-name)
  "load layout by using windows.el"
  (interactive
   (layout-read-args "Load the layout from: "
			    (confirm-nonexistent-file-or-buffer)))
  (let ((default-file-name win:configuration-file))
    (setq win:configuration-file in-name)
    (win-load-all-configurations)
    (setq win:configuration-file default-file-name)))

(global-set-key (kbd "C-c S") 'windows-save-layout)
(global-set-key (kbd "C-c F") 'windows-load-layout)
