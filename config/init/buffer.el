
(progn
  (comment
   ;; buffer-menu
   ;; http://stackoverflow.com/questions/1231188/emacs-list-buffers-behavior
   (global-set-key "\C-x\C-b" 'buffer-menu))
  (progn
    ;; ibuffer
    ;; http://emacs.stackexchange.com/questions/202/close-all-dired-buffers
    (global-set-key "\C-x\C-b" 'ibuffer)))

(require 'make-repeatable-command)

(comment
 ;; other window with repeatition
 ;; http://stackoverflow.com/questions/91071/emacs-switch-to-previous-window
 ;;(load "make-repeatable-command") ;(load "~/.emacs.d/package/make-repeatable-command.el")
  
 ;; switch buffers
 ;; http://emacs.stackexchange.com/questions/728/how-do-i-switch-buffers-quickly
 ;;(global-set-key (kbd "C-c p") 'previous-buffer)
 ;;(global-set-key (kbd "C-c n") 'next-buffer)

 ;;(global-set-key (kbd "C-c C-p") (make-repeatable-command 'previous-buffer))
 ;;(global-set-key (kbd "C-c C-n") (make-repeatable-command 'next-buffer))

 ;;(global-set-key (kbd "C-M-p") (make-repeatable-command 'previous-buffer))
 ;;(global-set-key (kbd "C-M-n") (make-repeatable-command 'next-buffer))

 ;;(global-set-key (kbd "C-c C-b") (make-repeatable-command 'previous-buffer))
 ;;(global-set-key (kbd "C-c C-f") (make-repeatable-command 'next-buffer))

 ;; (global-set-key (kbd "ESC <f7>") (make-repeatable-command 'previous-buffer)) ; M-<f7>
 ;; (global-set-key (kbd "ESC <f8>") (make-repeatable-command 'next-buffer)) ; M-<f8>

 ;; (global-set-key (kbd "<f7>") (make-repeatable-command 'previous-buffer)) ; M-<f7>
 ;; (global-set-key (kbd "<f8>") (make-repeatable-command 'next-buffer)) ; M-<f8>

 ;; (global-set-key (kbd "C-x <left>") (make-repeatable-command 'previous-buffer)) ; M-<f7>
 ;; (global-set-key (kbd "C-x <right>") (make-repeatable-command 'next-buffer)) ; M-<f8>
 )

(if (and nil (package-installed-p 'hydra))
    (progn
      (defhydra hydra-prev-next-buffer ()
	"prev-next-buffer"
	("<left>" previous-buffer)
	("<right>" next-buffer))
      (global-set-key (kbd "C-x <left>") #'hydra-prev-next-buffer/previous-buffer)
      (global-set-key (kbd "C-x <right>") #'hydra-prev-next-buffer/next-buffer))
  (progn
    (global-set-key (kbd "C-x <left>") (make-repeatable-command 'previous-buffer))
    (global-set-key (kbd "C-x <right>") (make-repeatable-command 'next-buffer))))

(progn
  (require 'buffer-move)
  (comment
   (defhydra hydra-buffer-move ()
     "buffer-move"
     ("h" buf-move-left)
     ("l" buf-move-right)
     ("k" buf-move-up)
     ("j" buf-move-down)
     ("q" nil "quit"))
   (global-set-key (kbd "C-c m") 'hydra-buffer-move/body))

  (defhydra hydra-buffer-move (global-map "C-c m")
    "buffer-move"
    ("q" nil "quit")
    ("j" buf-move-left)
    ("l" buf-move-right)
    ("k" buf-move-up)
    ("i" buf-move-down)))

(progn
  ;; https://stackoverflow.com/a/384346

  (defun rename-file-and-buffer (new-name)
    "Renames both current buffer and file it's visiting to NEW-NAME."
    (interactive "sNew name: ")
    (let ((name (buffer-name))
          (filename (buffer-file-name)))
      (if (not filename)
          (message "Buffer '%s' is not visiting a file!" name)
	(if (get-buffer new-name)
            (message "A buffer named '%s' already exists!" new-name)
          (progn
            (rename-file filename new-name 1)
            (rename-buffer new-name)
            (set-visited-file-name new-name)
            (set-buffer-modified-p nil)))))))

(progn
  (key-chord-define-global "kk" 'kill-buffer)
  (key-chord-define-global "ff" 'find-file))
