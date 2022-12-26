(progn
  ;; common key bindings independent of `dhnam/key-binding-style'

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
      (define-key key-translation-map (kbd "<key-chord> dq")  (kbd "C-c")))))

(let ((key-binding-style-file-name "~/.emacs.d/key-binding-style.txt"))
  (unless (file-exists-p key-binding-style-file-name)
    ;; the default key binding style is `fbnp'
    (write-region "fbnp" nil key-binding-style-file-name))

  (let ((cmd-line-arg-key "--key-binding-style"))
    (defvar dhnam/key-binding-style nil)
    (let ((cmd-line-arg-value (cmd-line-arg/register-then-get cmd-line-arg-key t)))
      (when cmd-line-arg-value
        (setq dhnam/key-binding-style
              (or (car (read-from-string cmd-line-arg-value))
                  (car (read-from-string (dhnam/get-string-from-file key-binding-style-file-name)))))))
    (cmd-line-arg/register cmd-line-arg-key t))

  (let ((key-binding-style-file-path (format "~/.emacs.d/base/key-binding/%s.el"
                                             (symbol-name dhnam/key-binding-style))))
    (if (file-exists-p key-binding-style-file-path)
        (load key-binding-style-file-path)
      (error (format "Unknown key-binding style: %s" dhnam/key-binding-style)))))
