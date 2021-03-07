
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
