(when (eq window-system nil)  ; when it's in terminal mode
  (comment
    (let ((cmd-line-arg-key "--tui"))
      (let ((cmd-line-arg-passed-p (cmd-line-arg/register-then-get cmd-line-arg-key nil)))
        (when cmd-line-arg-passed-p
          (comment)))))

  (use-existing-pkg clipetty
    ;; https://github.com/spudlyo/clipetty
    ;;
    ;; - clipetty is available in some terminal emulators such as xterm, kitty or alacritty
    ;; - clipetty doesn't requrie X11 forwarding when interacting with remote terminal emacs via ssh
    :hook (after-init . global-clipetty-mode))

  (comment
    ;; In some ssh connections, xclip-mode is not working a few mins after xclip-mode is enabled.
    (when (package-installed-p 'xclip)
      ;; Enable copy/paste between terminal emacs and gui applications
      ;; https://stackoverflow.com/a/46219455
      ;;
      ;; When interacting with remote terminal emacs, ssh with X11 forwarding is required
      (xclip-mode t)))

  (progn
    ;; Enable mouse in terminal mode
    ;; https://www.reddit.com/r/emacs/comments/5j89xn/comment/dbfvi21/?utm_source=share&utm_medium=web2x&context=3
    (xterm-mouse-mode t)))

(provide 'init-tui)
