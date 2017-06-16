;;; scrolling setting
;; http://stackoverflow.com/questions/13274439/how-to-configure-emacs-to-enable-c-v-to-move-cursor-half-window-height
(defun window-small-height ()
;  (max 1 (/ (1- (window-height (selected-window))) 10)))
  3)

(defun scroll-up-small ()
  (interactive)
  (scroll-up (window-small-height)))

(defun scroll-down-small ()         
  (interactive)                    
  (scroll-down (window-small-height)))

(global-set-key (kbd "C-v") 'scroll-up-small)
(global-set-key (kbd "M-v") 'scroll-down-small)



;;; scrolling other window
(defun scroll-other-up-small ()
  (interactive)
  (scroll-other-window (window-small-height)))

(defun scroll-other-down-small ()         
  (interactive)                    
  (scroll-other-window-down (window-small-height)))

(global-set-key (kbd "<ESC> <next>") 'scroll-other-up-small) ; page down
(global-set-key (kbd "<ESC> <prior>") 'scroll-other-down-small) ; page up



;;; diable auto-window-vscroll
;; http://stackoverflow.com/questions/18386824/emacs-how-do-you-disable-auto-recentering
;(setq scroll-step 3)
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)
