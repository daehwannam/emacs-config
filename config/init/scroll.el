;;; scrolling setting
;; http://stackoverflow.com/questions/13274439/how-to-configure-emacs-to-enable-c-v-to-move-cursor-half-window-height

;;; scroll half
(defun window-half-height ()
  (max 1 (/ (1- (window-height (selected-window))) 2)))

(defun scroll-up-half ()
  (interactive)
  (scroll-up (window-half-height)))

(defun scroll-down-half ()         
  (interactive)                    
  (scroll-down (window-half-height)))

(global-set-key [next] 'scroll-up-half)
(global-set-key [prior] 'scroll-down-half)

;;; scroll small 
(defun window-small-height ()
;  (max 1 (/ (1- (window-height (selected-window))) 10)))
  3)

(defun scroll-up-small ()
  (interactive)
  (scroll-up (window-small-height)))

(defun scroll-down-small ()         
  (interactive)                    
  (scroll-down (window-small-height)))

(defun scroll-up-small-with-cursor ()
  (interactive)
  (scroll-up (window-small-height))
  (next-line (window-small-height)))

(defun scroll-down-small-with-cursor ()         
  (interactive)                    
  (scroll-down (window-small-height))
  (previous-line (window-small-height)))

(global-set-key (kbd "C-v") 'scroll-up-small)
(global-set-key (kbd "M-v") 'scroll-down-small)

;; (global-set-key (kbd "C-V") 'scroll-up-small-with-cursor)
;; (global-set-key (kbd "M-v") 'scroll-down-small-with-cursor)

;; reverse-move-to-window-line-top-bottom
(defun reverse-move-to-window-line-top-bottom ()
  (interactive)
  (move-to-window-line-top-bottom -1))
(global-set-key (kbd "M-R") 'reverse-move-to-window-line-top-bottom)


(defun reverse-recenter-top-bottom ()
  (interactive)
  (let ((this-scroll-margin
	 (min (max 0 scroll-margin)
	      (truncate (/ (window-body-height) 4.0)))))
    (recenter (- -1 this-scroll-margin))))
(global-set-key (kbd "M-L") 'reverse-recenter-top-bottom)


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



;;; recenter-top-bottom

(defun recenter-top-bottom-other ()
  (interactive)
  (save-excursion
    (other-window 1)
    (recenter-top-bottom)
    (other-window -1)))

(global-set-key (kbd "C-x M-l") (make-repeatable-command 'recenter-top-bottom-other))


;; Avy: https://github.com/abo-abo/avy
(when (fboundp 'avy-goto-char-2)
  (global-set-key (kbd "M-g ;") 'avy-goto-char-2))


(progn
  ;; disables shortcuts for making long region
  (global-unset-key (kbd "C-x C-p"))
  )

(progn
  ;; performance enhance in files with long lines
  ;; https://emacs.stackexchange.com/a/19030
  (global-so-long-mode 1))
