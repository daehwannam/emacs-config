
(install-package-unless-installed 'key-chord)

(use-existing-pkg key-chord
  :init
  (progn
    (key-chord-mode 1)

    (progn
      ;; default values are 0.1 and 0.2
      (setq key-chord-two-keys-delay 0.1)
      (setq key-chord-one-key-delay 0.2))

    (progn
      (key-chord-define-global ";f" (lookup-key (current-global-map) (kbd "C-x C-f"))) ; find-file
      (key-chord-define-global "xs" 'save-buffer)
      (key-chord-define-global "xc" 'save-buffers-kill-emacs)
      (comment
       (key-chord-define-global "bb" (lookup-key (current-global-map) (kbd "C-x b")))
       (key-chord-define-global "BB" (lookup-key (current-global-map) (kbd "C-x C-b")))))

    (comment
     (progn
       (key-chord-define-global "xb" 'switch-to-buffer)
       (key-chord-define-global "je" 'eval-last-sexp)
       (key-chord-define-global "fn" 'forward-list)
       (key-chord-define-global "fp" 'backward-list)
       (key-chord-define-global "kk" (kbd "C-g")))
     (comment
      (key-chord-define-global "xo" 'other-window-repeat)
      (key-chord-define-global "xs" 'save-buffer))
     )

    (comment
     (when (require 'space-chord nil t)
       ;; keyboard-quit is not working
       (comment (space-chord-define-global "g" 'keyboard-quit)))))
  :config
  (key-chord-mode t))
