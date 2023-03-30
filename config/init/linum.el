
(cond
 ((fboundp 'display-line-numbers-mode)
  ;; built in line mode
  ;; https://www.reddit.com/r/orgmode/comments/e7pq7k/linummode_very_slow_for_large_org_files/

  (defvar dhnam/display-line-numbers-disabled-modes-list '(pdf-view-mode))
  (defun dhnam/turn-on-display-line-numbers-mode ()
    ;; https://www.emacswiki.org/emacs/LineNumbers#h5o-10
    (unless (or (minibufferp) (member major-mode dhnam/display-line-numbers-disabled-modes-list))
      (display-line-numbers-mode 1)))
  (comment (add-hook 'find-file-hook 'display-line-numbers-mode))
  (add-hook 'find-file-hook 'dhnam/turn-on-display-line-numbers-mode))
 ((fboundp 'linum-mode)
  ;; linum-mode setting
  ;; https://emacs.stackexchange.com/questions/21504/enable-linum-mode-for-all-files-with-extension-but-not-other-buffers
  (add-hook 'find-file-hook 'linum-mode)
  ;; (add-hook 'text-mode-hook 'linum-mode)
  ;; (add-hook 'prog-mode-hook 'linum-mode)
  ;; (setq linum-format "%d ")

  (progn
    (setq linum-format "%4d \u2502 ")
    (comment (fringe-mode 1))
    (progn
      ;; https://emacs.stackexchange.com/a/5343
      ;; disable fringe color
      (set-face-attribute 'fringe nil
                          :foreground (face-foreground 'default)
                          :background (face-background 'default))))))

(defun dhnam/copy-linum ()
  ;; copy line number
  ;; http://ergoemacs.org/emacs/elisp_cut_copy_yank_kill-ring.html
  (interactive)
  (setq numstr (number-to-string (line-number-at-pos)))
  (kill-new numstr)
  (message (concat "line number: " numstr)))
  

(provide 'init-linum)
