
(with-eval-after-load 'dired
  (require 'dired-async)
  (define-key dired-mode-map (kbd "C-c R") 'dired-async-do-rename)
  (define-key dired-mode-map (kbd "C-c C") 'dired-async-do-copy)

  (comment
   ;; Both 'dired-do-create-files and 'dired-do-rename use 'dired-create-files
   ;; which is adviced by dired-async-create-files.
   ;; Then, it makes a problem that dired-do-rename doesn't change its buffer name.
   ;;
   ;; https://github.com/jwiegley/emacs-async
   (autoload 'dired-async-mode "dired-async.el" nil t)
   (dired-async-mode 1)))

(provide 'dhnam-async)
