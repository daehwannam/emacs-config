(require 'dhnam-make-repeatable-command)

;;; scrolling setting
;; http://stackoverflow.com/questions/13274439/how-to-configure-emacs-to-enable-c-v-to-move-cursor-half-window-height

;;; scroll half
(defun dhnam/window-half-height ()
  (max 1 (/ (1- (window-height (selected-window))) 2)))

(defun dhnam/scroll-up-half ()
  (interactive)
  (scroll-up (dhnam/window-half-height)))

(defun dhnam/scroll-down-half ()         
  (interactive)                    
  (scroll-down (dhnam/window-half-height)))

(comment
  (global-set-key [next] 'dhnam/scroll-up-half)
  (global-set-key [prior] 'dhnam/scroll-down-half))

;;; scroll small
(defun dhnam/window-small-height ()
  ;;  (max 1 (/ (1- (window-height (selected-window))) 10)))
  3)

(defun dhnam/scroll-up-small ()
  (interactive)
  (scroll-up (dhnam/window-small-height)))

(defun dhnam/scroll-down-small ()
  (interactive)                    
  (scroll-down (dhnam/window-small-height)))

(defun dhnam/scroll-up-small-with-cursor ()
  (interactive)
  (scroll-up (dhnam/window-small-height))
  (next-line (dhnam/window-small-height)))

(defun dhnam/scroll-down-small-with-cursor ()         
  (interactive)                    
  (scroll-down (dhnam/window-small-height))
  (previous-line (dhnam/window-small-height)))

(global-set-key (kbd "C-v") 'dhnam/scroll-up-small)
(global-set-key (kbd "M-v") 'dhnam/scroll-down-small)

;; (global-set-key (kbd "C-V") 'dhnam/scroll-up-small-with-cursor)
;; (global-set-key (kbd "M-v") 'dhnam/scroll-down-small-with-cursor)

;; dhnam/reverse-move-to-window-line-top-bottom
(defun dhnam/reverse-move-to-window-line-top-bottom ()
  (interactive)
  (move-to-window-line-top-bottom -1))
(global-set-key (kbd "M-R") 'dhnam/reverse-move-to-window-line-top-bottom)


(defun dhnam/reverse-recenter-top-bottom ()
  (interactive)
  (let ((this-scroll-margin
	     (min (max 0 scroll-margin)
	          (truncate (/ (window-body-height) 4.0)))))
    (recenter (- -1 this-scroll-margin))))
(global-set-key (kbd "M-L") 'dhnam/reverse-recenter-top-bottom)


;;; scrolling other window
(defun dhnam/scroll-other-up-small ()
  (interactive)
  (scroll-other-window (dhnam/window-small-height)))

(defun dhnam/scroll-other-down-small ()         
  (interactive)                    
  (scroll-other-window-down (dhnam/window-small-height)))

(global-set-key (kbd "<ESC> <next>") 'dhnam/scroll-other-up-small) ; page down
(global-set-key (kbd "<ESC> <prior>") 'dhnam/scroll-other-down-small) ; page up



(progn
  ;; diable auto-window-vscroll
  ;; http://stackoverflow.com/questions/18386824/emacs-how-do-you-disable-auto-recentering

  ;; (setq scroll-step 3)
  (setq scroll-step 1)
  (setq scroll-conservatively 10000)
  (setq auto-window-vscroll nil))


(progn
;;; recenter-top-bottom
  (defun dhnam/recenter-top-bottom-other ()
    (interactive)
    (save-excursion
      (other-window 1)
      (recenter-top-bottom)
      (other-window -1)))

  (global-set-key (kbd "C-x M-l") (make-repeatable-command 'dhnam/recenter-top-bottom-other)))


(comment
  (progn
    ;; disables shortcuts for making long region (mark-page)
    (global-unset-key (kbd "C-x C-p"))
    )) 

(comment
  (progn
    ;; performance enhance in files with long lines
    ;; https://emacs.stackexchange.com/a/19030
    (when (boundp 'global-so-long-mode)
      (global-so-long-mode 1))))

(provide 'init-scroll)
