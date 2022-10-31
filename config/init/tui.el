(when (eq window-system nil)
  ;; Enable mouse in terminal mode
  ;; https://www.reddit.com/r/emacs/comments/5j89xn/comment/dbfvi21/?utm_source=share&utm_medium=web2x&context=3
  (xterm-mouse-mode t))

(when (package-installed-p 'xclip)
  ;; Enable copy/paste between terminal emacs and gui applications
  ;; https://stackoverflow.com/a/46219455
  (xclip-mode t))
