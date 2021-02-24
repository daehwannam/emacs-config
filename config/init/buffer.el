
(progn
  (comment
   ;; buffer-menu
   ;; http://stackoverflow.com/questions/1231188/emacs-list-buffers-behavior
   (global-set-key "\C-x\C-b" 'buffer-menu))
  (progn
    ;; ibuffer
    ;; http://emacs.stackexchange.com/questions/202/close-all-dired-buffers
    (global-set-key "\C-x\C-b" 'ibuffer)))

(comment
  ;; other window with repeatition
  ;; http://stackoverflow.com/questions/91071/emacs-switch-to-previous-window
  ;;(load "make-repeatable-command") ;(load "~/.emacs.d/package/make-repeatable-command.el")
  (require 'make-repeatable-command)

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

  (global-set-key (kbd "C-x <left>") (make-repeatable-command 'previous-buffer)) ; M-<f7>
  (global-set-key (kbd "C-x <right>") (make-repeatable-command 'next-buffer)) ; M-<f8>
  )

(progn
  (defhydra hydra-prev-next-buffer ()
    "prev-next-buffer"
    ("<left>" previous-buffer)
    ("<right>" next-buffer))
  (global-set-key (kbd "C-x <left>") #'hydra-prev-next-buffer/previous-buffer)
  (global-set-key (kbd "C-x <right>") #'hydra-prev-next-buffer/next-buffer))

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
