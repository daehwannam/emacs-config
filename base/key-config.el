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
      (define-key key-translation-map (kbd "<key-chord> dq")  (kbd "C-c"))))

  (progn
    ;; special keys defined by "xmodmap" and "xcape" from "~/.emacs.d/exwm/load-key-config.sh"
    (defconst dhnam/xcape-left-alt (or (dhnam/machine-config-get-first 'xcape-left-alt)
                                       "₢")))

  (progn
    ;; <CruzeiroSign> = ₫
    (comment
      (keyboard-translate ?\₫ ?\C-g)
      (comment (global-set-key (kbd "C-g") 'dhnam/keyboard-quit)))
    (progn
      (global-set-key (kbd "₫") 'dhnam/keyboard-quit)
      (define-key isearch-mode-map (kbd "₫") 'isearch-abort))))

(progn
  (defvar dhnam/default-key-binding-style 'dhnam-iokl)
  (defvar dhnam/key-binding-style
    (let ((key-binding-style-file-name (concat dhnam/emacs-root-dir "key-binding-style.el")))
      (unless (file-exists-p key-binding-style-file-name)
        (write-region (symbol-name dhnam/default-key-binding-style) nil key-binding-style-file-name))

      (let ((cmd-line-arg-key "--key-binding-style"))
        (let ((cmd-line-arg-value (cmd-line-arg/register-then-get cmd-line-arg-key t)))
          (or (and cmd-line-arg-value (car (read-from-string cmd-line-arg-value)))
              (car (read-from-string (dhnam/get-string-from-file key-binding-style-file-name))))))))

  (let ((key-binding-style-file-path (format (concat dhnam/emacs-root-dir "base/key-binding/%s.el")
                                               (symbol-name dhnam/key-binding-style))))
      (if (file-exists-p key-binding-style-file-path)
          (load key-binding-style-file-path)
        (error (format "Unknown key-binding style: %s" dhnam/key-binding-style)))))

(progn
  ;; Commands
  (defun dhnam/keyboard-quit ()
    ;; Same definition with `ergoemacs-keyboard-quit'
    "Quit the current command/process.
Similar to `keyboard-quit', with the following changes:

• In the minibuffer, use `minibuffer-keyboard-quit'

• When a region is active, (see `region-active-p') deactivate the
  region with the function `deactivate-mark'.

• When \"C-g\" is bound to something other than ergoemacs /
  standard quit commands, run that command.

• When \"q\" is bound to something other than self-insert
  commands, run that command.

• Otherwise run `keyboard-quit'"
    (interactive)
    (let (bind)
      (cond
       ((minibufferp)
        (minibuffer-keyboard-quit))
       ((region-active-p)
        (setq saved-region-selection nil)
        (let (select-active-regions)
          (deactivate-mark)))
       ((and (setq bind (key-binding [7])) ;; C-g
             (not (memq bind '(dhnam/key-binding-style minibuffer-keyboard-quit keyboard-quit))))
        (call-interactively bind))
       ((and (setq bind (key-binding [?q]))
             (not (string-match-p "self-insert" (symbol-name bind)))
             (not (eq bind 'dhnam/key-binding-style)))
        (call-interactively bind))
       (t
        (keyboard-quit))))))
