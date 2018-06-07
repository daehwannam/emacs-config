;;;; single key to jump thru marks
;(defun xah-pop-local-mark-ring ()
;  "Move cursor to last mark position of current buffer.
;Call this repeatedly will cycle all positions in `mark-ring'.
;URL `http://ergoemacs.org/emacs/emacs_jump_to_previous_position.html'
;version 2016-04-04"
;  (interactive)
;  (set-mark-command t))
;(global-set-key (kbd "M-7") 'pop-global-mark) ; Meta+7
;(global-set-key (kbd "M-8") 'xah-pop-local-mark-ring) ; Meta+8


;;; other window with repeatition
;; http://stackoverflow.com/questions/91071/emacs-switch-to-previous-window
;(load "make-repeatable-command") ;(load "~/.emacs.d/package/make-repeatable-command.el")
(require 'make-repeatable-command)


;;; move forward and backward
;; http://stackoverflow.com/questions/3393834/how-to-move-forward-and-backward-in-emacs-mark-ring
;; https://github.com/deestan/emacs/blob/master/emacs-goodies-el/marker-visit.el
;(require 'marker-visit)
;(global-set-key (kbd "s-7") 'marker-visit-prev) ; Meta+7
;(global-set-key (kbd "s-8") 'marker-visit-next) ; Meta+8

;(global-set-key (kbd "C-c p") (make-repeatable-command 'marker-visit-prev))
;(global-set-key (kbd "C-c n") (make-repeatable-command 'marker-visit-next))



;;; move forward and backward
;; http://stackoverflow.com/questions/3393834/how-to-move-forward-and-backward-in-emacs-mark-ring
(defun unpop-to-mark-command ()
  "Unpop off mark ring. Does nothing if mark ring is empty."
  (interactive)
      (when mark-ring
        (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
        (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
        (when (null (mark t)) (ding))
        (setq mark-ring (nbutlast mark-ring))
        (goto-char (marker-position (car (last mark-ring))))))

;; (global-set-key (kbd "C-x <f7>") (make-repeatable-command 'pop-to-mark-command))
;; (global-set-key (kbd "C-x <f8>") (make-repeatable-command 'unpop-to-mark-command))

(global-set-key (kbd "<f7>") (make-repeatable-command 'pop-to-mark-command))
(global-set-key (kbd "<f8>") (make-repeatable-command 'unpop-to-mark-command))
