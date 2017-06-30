;;; other-window
;; http://stackoverflow.com/questions/5046597/changing-window-faster-in-emacs-or-repeating-last-shortcut-with-a-single-strike
;(when (fboundp 'windmove-default-keybindings)
;  (windmove-default-keybindings))

;; https://www.emacswiki.org/emacs/WindMove
;(global-set-key (kbd "C-c <left>")  'windmove-left)
;(global-set-key (kbd "C-c <right>") 'windmove-right)
;(global-set-key (kbd "C-c <up>")    'windmove-up)
;(global-set-key (kbd "C-c <down>")  'windmove-down)

;;; other window with repeatition
;; http://stackoverflow.com/questions/91071/emacs-switch-to-previous-window

(defun other-window-backwards () (interactive) (other-window -1))
(defun other-frame-backwards () (interactive) (other-frame -1))

;(load "make-repeatable-command") ;(load "~/.emacs.d/package/make-repeatable-command.el")
(require 'make-repeatable-command)
(global-set-key (kbd "C-x o") (make-repeatable-command 'other-window))
(global-set-key (kbd "C-x O") (make-repeatable-command 'other-window-backwards))
(global-set-key (kbd "C-x 5 o") (make-repeatable-command 'other-frame))
(global-set-key (kbd "C-x 5 O") (make-repeatable-command 'other-frame-backwards))

;; other-frame key setting
(global-set-key (kbd "C-c o") (make-repeatable-command 'other-frame))
(global-set-key (kbd "C-c O") (make-repeatable-command 'other-frame-backwards))

;; make-frame-command & delete-frame key setting
(global-set-key (kbd "C-c 0") 'delete-frame)
(global-set-key (kbd "C-c 1") 'delete-other-frames)
(global-set-key (kbd "C-c 2") 'make-frame-command)

;; window size adjust
(global-set-key (kbd "C-x ^") (make-repeatable-command 'enlarge-window))
(global-set-key (kbd "C-x %") (make-repeatable-command 'shrink-window))
(global-set-key (kbd "C-x }") (make-repeatable-command 'enlarge-window-horizontally))
(global-set-key (kbd "C-x {") (make-repeatable-command 'shrink-window-horizontally))
