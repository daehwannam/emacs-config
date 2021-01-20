
(use-existing-pkg key-chord
  :init
  (progn
    (key-chord-mode 1)

    ;; default delay is 0.1
    (setq key-chord-two-keys-delay 0.1)

    (progn
      (progn
	(key-chord-define-global "xb" 'switch-to-buffer)
	(key-chord-define-global "je" 'eval-last-sexp)
	(key-chord-define-global "fn" 'forward-list)
	(key-chord-define-global "fp" 'backward-list))
      (comment
       (key-chord-define-global "xo" 'other-window-repeat)
       (key-chord-define-global "xs" 'save-buffer))
      ))

  (comment
   (when (require 'space-chord nil t)
     ;; keyboard-quit is not working
     (comment (space-chord-define-global "g" 'keyboard-quit)))))
