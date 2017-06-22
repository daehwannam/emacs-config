;;; linum-mode setting
;(setq linum-format "%d ")
(setq linum-format "%4d \u2502 ")


;;; copy line number
;; http://ergoemacs.org/emacs/elisp_cut_copy_yank_kill-ring.html
(defun copy-linum ()
  (interactive)
  (setq numstr (number-to-string (line-number-at-pos)))
  (kill-new numstr)
  (message (concat "line number: " numstr)))
