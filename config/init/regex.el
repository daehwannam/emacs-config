
;; https://emacs.stackexchange.com/questions/5568/why-do-regular-expressions-created-with-the-regex-builder-use-syntax-different-f
(setq reb-re-syntax 'string)

;; (add-hook 'reb-mode-hook
;; 	  (local-set-key (kbd "C-c C-s") (make-repeatable-command 'reb-next-match)))
;; it don't work
