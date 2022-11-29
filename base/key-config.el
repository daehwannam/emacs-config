
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
  ;; use "qw" as prefix key instead of "C-x"
  (fset 'ctl-x-map ctl-x-map)
  (key-chord-define-global "qw" 'ctl-x-map)

  (progn
    (global-set-key (kbd "C-c q") (lookup-key (current-global-map) (kbd "C-q")))
    (global-set-key (kbd "C-q") 'ctl-x-map))

  (progn
    ;; C-x C-q is originally mapped to 'read-only-mode
    (global-set-key (kbd "C-c C-q") (lookup-key (current-global-map) (kbd "C-x C-q")))
    (comment (define-key ctl-x-map (kbd "C-q") 'exchange-point-and-mark))
    (global-set-key (kbd "C-x C-q") 'exchange-point-and-mark)))

(progn
  ;; use "qd" as prefix key instead of "C-c"
  ;; https://emacs.stackexchange.com/a/64130

  (progn
    (key-chord-define-global "qd" 'null)
    (define-key key-translation-map (kbd "<key-chord> qd")  (kbd "C-c"))
    (define-key key-translation-map (kbd "<key-chord> dq")  (kbd "C-c"))))
