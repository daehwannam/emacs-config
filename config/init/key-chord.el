
(use-existing-pkg key-chord
  :init
  (progn
    (key-chord-mode 1)

    ;; default delay is 0.1
    (setq key-chord-two-keys-delay 0.05)

    (progn
      (comment (key-chord-define-global "xo" 'other-window-repeat)))

    (when (require 'space-chord nil t)
      ;; keyboard-quit is not working
      (comment (space-chord-define-global "g" 'keyboard-quit)))))