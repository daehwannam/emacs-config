
(cond
 ((fboundp 'display-line-numbers-mode)
  ;; built in line mode
  ;; https://www.reddit.com/r/orgmode/comments/e7pq7k/linummode_very_slow_for_large_org_files/
  (add-hook 'find-file-hook 'display-line-numbers-mode))
 ((fboundp 'linum-mode)
  ;; linum-mode setting
  ;; https://emacs.stackexchange.com/questions/21504/enable-linum-mode-for-all-files-with-extension-but-not-other-buffers
  (add-hook 'find-file-hook 'linum-mode)
  ;; (add-hook 'text-mode-hook 'linum-mode)
  ;; (add-hook 'prog-mode-hook 'linum-mode)
  ;; (setq linum-format "%d ")

  (progn
    (setq linum-format "%4d \u2502 ")
    (comment
     ;; https://emacs.stackexchange.com/a/5343
     ;; disable fringe color
     (fringe-mode 1)
     (set-face-attribute 'fringe nil
                         :foreground (face-foreground 'default)
                         :background (face-background 'default))))))

(defun copy-linum ()
  ;; copy line number
  ;; http://ergoemacs.org/emacs/elisp_cut_copy_yank_kill-ring.html
  (interactive)
  (setq numstr (number-to-string (line-number-at-pos)))
  (kill-new numstr)
  (message (concat "line number: " numstr)))
  
