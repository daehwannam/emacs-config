
;;; other window with repeatition
;; http://stackoverflow.com/questions/91071/emacs-switch-to-previous-window
;(load "make-repeatable-command") ;(load "~/.emacs.d/package/make-repeatable-command.el")
(require 'make-repeatable-command)

;;; switch buffers
;; http://emacs.stackexchange.com/questions/728/how-do-i-switch-buffers-quickly
;(global-set-key (kbd "C-c p") 'previous-buffer)
;(global-set-key (kbd "C-c n") 'next-buffer)
(global-set-key (kbd "C-c p") (make-repeatable-command 'previous-buffer))
(global-set-key (kbd "C-c n") (make-repeatable-command 'next-buffer))
