;;; linum-mode setting

;; https://emacs.stackexchange.com/questions/21504/enable-linum-mode-for-all-files-with-extension-but-not-other-buffers
(add-hook 'find-file-hook 'linum-mode)
;; (add-hook 'text-mode-hook 'linum-mode)
;; (add-hook 'prog-mode-hook 'linum-mode)

;(setq linum-format "%d ")
(setq linum-format "%4d \u2502 ")


;;; copy line number
;; http://ergoemacs.org/emacs/elisp_cut_copy_yank_kill-ring.html
(defun copy-linum ()
  (interactive)
  (setq numstr (number-to-string (line-number-at-pos)))
  (kill-new numstr)
  (message (concat "line number: " numstr)))

(require 'hmlinum)
(hmlinum-activate)
