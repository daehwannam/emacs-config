;;; yank-pop-forwards
(defun yank-pop-forwards (arg)
  (interactive "p")
  (yank-pop (- arg)))
(global-set-key "\M-Y" 'yank-pop-forwards) ; M-Y (Meta-Shift-Y)

(when (fboundp 'counsel-yank-pop)
  (global-set-key (kbd "C-M-y") 'counsel-yank-pop))
