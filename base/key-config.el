
(comment
  (progn
    ;; space key as ctrl
    ;; https://emacs.stackexchange.com/a/26068/26068
    (define-key key-translation-map (kbd "<SPC>") 'event-apply-control-modifier))

  (defun insert-space-command ()
    (interactive)
    (insert " "))

  (progn
    ;; set-mark-command --> "C-@"

    (global-set-key (kbd "M-<SPC>") 'insert-space-command)

    ;; orignial key of "M-'" --> abbrev-prefix-mark
    (global-set-key (kbd "M-'") 'insert-space-command)))


(comment
  (global-set-key (kbd "M-P") 'backward-sexp)
  (global-set-key (kbd "M-N") 'forward-sexp))

(progn
  (fset 'ctl-x-map ctl-x-map)
  (comment (key-chord-define-global "ff" 'ctl-x-map))
  (global-set-key (kbd "C-q") 'ctl-x-map)
  (define-key ctl-x-map (kbd "C-q") 'exchange-point-and-mark)
  (global-set-key (kbd "C-c C-q") 'read-only-mode))

(comment
  (defvar my-ctl-c-map nil
    "Default keymap for C-c commands.
The normal global definition of the character C-x indirects to this keymap.")

  (fset 'my-ctl-c-map my-ctl-c-map)
  (key-chord-define-global "rr" 'my-ctl-c-map))
