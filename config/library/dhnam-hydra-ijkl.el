
(unless (fboundp 'comment)
  (defmacro comment (&rest args)
    `nil))

(defhydra dhnam-hydra-ijkl
  (:hint nil
   :pre (set-cursor-color "#40e0d0")
   :post (progn
           (set-cursor-color "#ffffff")
           (comment (message "Thank you, come again."))))
  "ijkl"

  ("i" previous-line)
  ("k" next-line)
  ("j" backward-char)
  ("l" forward-char)
  ("u" backward-word)
  ("o" forward-word)

  ("M-i" backward-sexp)
  ("M-k" forward-sexp)
  ("M-j" backward-list)
  ("M-l" forward-list)
  ("M-u" backward-up-list)
  ("M-o" down-list)
  ("C-i"  dhnam/scroll-up-small)
  ("C-k"  dhnam/scroll-down-small)

  ("a" move-beginning-of-line)
  ("s" move-end-of-line)
  ("A" beginning-of-buffer)
  ("S" end-of-buffer)
  ("m" back-to-indentation)

  ("w" kill-ring-save)
  ("e" kill-region)
  ("d" delete-char)
  ("D" kill-word)
  ("f" kill-line)
  ("F" kill-sexp)

  ("y" yank)
  ("M-y" yank-pop)
  ("M-Y" dhnam/yank-pop-forwards)

  ("SPC" set-mark-command)
  ("r" exchange-point-and-mark)
  ("g" keyboard-quit)

  ("p" other-window-repeat)

  ("RET" nil "quit")
  ("q" nil "quit"))

(comment
  (global-set-key (kbd "<XF86WWAN>") 'dhnam-hydra-ijkl/body))

(comment
  (require 'dhnam-paredit))

(defvar dhnam-ijkl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-i") 'previous-line)
    (define-key map (kbd "M-k") 'next-line)
    (define-key map (kbd "M-j") 'backward-char)
    (define-key map (kbd "M-l") 'forward-char)
    (define-key map (kbd "M-u") 'backward-word)
    (define-key map (kbd "M-o") 'forward-word)

    (define-key map (kbd "C-M-i") 'backward-sexp)
    (define-key map (kbd "C-M-k") 'forward-sexp)
    (define-key map (kbd "C-M-j") 'backward-list)
    (define-key map (kbd "C-M-l") 'forward-list)
    (define-key map (kbd "C-M-u") 'backward-up-list)
    (define-key map (kbd "C-M-o") 'down-list)
    (comment (define-key map (kbd "C-M-u") 'dhnam/paredit-backward-up-or-down))
    (comment (define-key map (kbd "C-M-o") 'dhnam/paredit-forward-up-or-down))

    (define-key map (kbd "M-n") 'electric-newline-and-maybe-indent)
    (define-key map (kbd "C-M-n") 'default-indent-new-line)

    (define-key map (kbd "M-'") dhnam-ijkl/default-global-map)
    map)
  "Keymap for `dhnam-ijkl-mode'.")


(fset 'dhnam-ijkl/default-global-map global-map)


(define-minor-mode dhnam-ijkl-mode
  "IJKL key binding"
  nil                                ; Initial value, nil for disabled
  :global t
  :lighter " IJKL"
  :keymap dhnam-ijkl-mode-map

  (if dhnam-ijkl-mode
      (comment)
    (comment)))

(provide 'dhnam-hydra-ijkl)
