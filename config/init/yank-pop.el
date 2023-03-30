;;; dhnam/yank-pop-forwards
(defun dhnam/yank-pop-forwards (arg)
  (interactive "p")
  (yank-pop (- arg)))
(global-set-key "\M-Y" 'dhnam/yank-pop-forwards) ; M-Y (Meta-Shift-Y)

(when (fboundp 'counsel-yank-pop)
  (global-set-key (kbd "C-M-y") 'counsel-yank-pop))

(provide 'init-yank-pop)
