(when (eq window-system nil)  ; when it's in terminal mode
  (when (package-installed-p 'xclip)
    (progn
      ;; Enable copy/paste between terminal emacs and gui applications
      ;; https://stackoverflow.com/a/46219455
      ;;
      ;; When interacting with remote terminal emacs, ssh with X11 forwarding is required
      (xclip-mode t))
    (progn
      ;; Enable mouse in terminal mode
      ;; https://www.reddit.com/r/emacs/comments/5j89xn/comment/dbfvi21/?utm_source=share&utm_medium=web2x&context=3
      (xterm-mouse-mode t))))


