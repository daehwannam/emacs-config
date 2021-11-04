;; use C-j as newline-and-indent rather than Enter.
(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))

(setq indent-rigidly-map
  (let ((map (make-sparse-keymap)))
    (define-key map [left]  'indent-rigidly-left)
    (define-key map [right] 'indent-rigidly-right)
    (define-key map [S-left]  'indent-rigidly-left-to-tab-stop)
    (define-key map [S-right] 'indent-rigidly-right-to-tab-stop)
    (define-key map (kbd "j")  'indent-rigidly-left)
    (define-key map (kbd "l") 'indent-rigidly-right)
    (define-key map (kbd "J")  'indent-rigidly-left-to-tab-stop)
    (define-key map (kbd "L") 'indent-rigidly-right-to-tab-stop)
    (define-key map (kbd "q") 'keyboard-quit)
    map))
