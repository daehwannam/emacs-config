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

(global-set-key (kbd "C-x ㅐ") (make-repeatable-command 'other-window))

;; other-frame key setting
(global-set-key (kbd "C-c o") (make-repeatable-command 'other-frame))
(global-set-key (kbd "C-c O") (make-repeatable-command 'other-frame-backwards))

(global-set-key (kbd "C-c ㅐ") (make-repeatable-command 'other-frame))

;; make-frame-command & delete-frame key setting
(global-set-key (kbd "C-c 0") 'delete-frame)
(global-set-key (kbd "C-c 1") 'delete-other-frames)
(global-set-key (kbd "C-c 2") 'make-frame-command)

;; window size adjust
(global-set-key (kbd "C-x ^") (make-repeatable-command 'enlarge-window))
(global-set-key (kbd "C-x %") (make-repeatable-command 'shrink-window))
(global-set-key (kbd "C-x }") (make-repeatable-command 'enlarge-window-horizontally))
(global-set-key (kbd "C-x {") (make-repeatable-command 'shrink-window-horizontally))


;; kill buffer and delete window
(defun kill-buffer-and-delete-window ()
  (interactive)
  (kill-buffer)
  (delete-window))

(global-set-key (kbd "C-x K") 'kill-buffer-and-delete-window)

;; kill buffer and delete frame
(defun kill-buffer-and-delete-frame ()
  (interactive)
  (kill-buffer)
  (delete-frame))

;(global-set-key (kbd "C-c K") 'kill-buffer-and-delete-frame)

;; split windows below or right
(defun split-window-below-or-right-and-find-file (filename &optional wildcards)
  (interactive
   (find-file-read-args "Find file in other window: "
                        (confirm-nonexistent-file-or-buffer)))
  (if (< (* (window-body-height) 2) (window-body-width))
      (split-window-right)
    (split-window-below))
  (other-window 1)
  (find-file filename))

(global-set-key (kbd "C-x F") 'split-window-below-or-right-and-find-file)
