
(unless (fboundp 'comment)
  (defmacro comment (&rest args)
    `nil))

(defvar dhnam-hydra-ijkl/default-cursor-color "orchid")
(defvar dhnam-hydra-ijkl/activated-cursor-color "cyan")

(defun dhnam-hydra-ijkl/set-cursor-color (color)
  (if (display-graphic-p)
      (set-cursor-color color)
    (send-string-to-terminal (format "\033]12;%s\007" color))))

(defhydra dhnam-hydra-ijkl
  (:hint nil
         :pre (progn
                (dhnam-hydra-ijkl/set-cursor-color dhnam-hydra-ijkl/activated-cursor-color))
         :post (progn
                 (dhnam-hydra-ijkl/set-cursor-color dhnam-hydra-ijkl/default-cursor-color)))
  "ijkl"

  ("i" previous-line)
  ("k" next-line)
  ("j" backward-char)
  ("l" forward-char)
  ("u" backward-word)
  ("o" forward-word)

  ("M-i" backward-list)
  ("M-k" forward-list)
  ("M-j" backward-sexp)
  ("M-l" forward-sexp)
  ("M-u" backward-up-list)
  ("M-o" down-list)

  ("C-i"  dhnam/scroll-down-small)
  ("C-k"  dhnam/scroll-up-small)

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
  ("DEL" delete-backward-char)
  ("M-DEL" backward-kill-word)

  ("y" yank)
  ("M-y" yank-pop)
  ("M-Y" dhnam/yank-pop-forwards)
  ("C-M-y" counsel-yank-pop)

  ("r" recenter-top-bottom)
  ("R" move-to-window-line-top-bottom)

  ;; ("SPC" set-mark-command)
  ("v" set-mark-command)
  ("'" exchange-point-and-mark)
  ("g" keyboard-quit)

  ("/" undo)

  (";" other-window-repeat)
  ("p" tab-next-repeat)

  ("₫" nil "quit")
  ("RET" nil "quit")
  ("q" nil "quit"))

(hydra-set-property 'dhnam-hydra-ijkl :verbosity 0)

(comment
  (global-set-key (kbd "₢") 'dhnam-hydra-ijkl/body))

(provide 'dhnam-hydra-ijkl)
