
(require 'dhnam-text)

(global-set-key (kbd "M-_") 'dhnam/toggle-camelcase-underscores)
(comment (global-set-key (kbd "C-x M-w") 'dhnam/kill-ring-save-symbol-at-point))
(comment (global-set-key (kbd "M-W") 'dhnam/kill-ring-save-symbol-at-point))
(key-chord-define-global "w." 'dhnamm/kill-ring-save-thing-at-point)
(global-set-key (kbd "M-<SPC>") 'dhnam/just-one-space-conditionally)

(comment
  (cl-flet ((bound-key-p (k) (when (let ((func (lookup-key (current-global-map) k)))
				                     (or (eq func 'just-one-space)
				                         (eq func 'dhnam/just-one-space-conditionally)))
			                   k)))
    (let ((k (or (bound-key-p (kbd "M-<SPC>"))
	             (bound-key-p (kbd "M-'"))
	             (t (error "No key for 'just-one-space is found")))))
      (global-set-key k 'dhnam/just-one-space-conditionally))))

(comment
  (comment (key-chord-define (current-global-map) "kk" (make-repeatable-command #'dhnam/copy-and-next-line)))
  (global-set-key (kbd "M-k") 'dhnam/copy-and-next-line))

(global-set-key (kbd "M-k") 'dhnam/copy-line)

(progn
  ;; add new keybindings for kitty terminal
  (key-chord-define-global "q\\" 'indent-region))

(when (package-installed-p 'expand-region)
  (require 'expand-region)
  (comment (global-set-key (kbd "C-c .") (make-repeatable-command #'er/expand-region)))
  (global-set-key (kbd "C-c .") 'er/expand-region)

  (setq expand-region-contract-fast-key ","))

(progn
  ;; enable `narrow-to-region'
  (put 'narrow-to-region 'disabled nil))

(progn
  ;; Change the line length for `fill-paragraph'.
  ;; https://stackoverflow.com/a/3567111
  ;;
  ;; The default value of `fill-column' is 70.
  (setq fill-column 110))

(progn
  (require 'dhnam-org-todo-comment)

  (global-set-key (kbd "C-c M-m") 'dhnam/otc-toggle)
  (define-key prog-mode-map (kbd "C-c M-m") 'dhnam/otc-toggle-in-program)
  (comment (global-set-key (kbd "C-c M-m") 'dhnam/otc-summarize-todo))
  (global-set-key (kbd "C-c M-n") 'dhnam/otc-summarize-todo))

(provide 'init-text)
