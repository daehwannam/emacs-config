
(unless (fboundp 'comment)
  (defmacro comment (&rest args)
    `nil))

(defvar dhnam-hydra-ijkl/default-cursor-color "orchid")
(defvar dhnam-hydra-ijkl/activated-cursor-color "cyan")

(defconst dhnam-hydra-ijkl/activation-key "₢")
(defconst dhnam-hydra-ijkl/quit-key "₫")

(defun dhnam-hydra-ijkl/set-cursor-color (color)
  (if (display-graphic-p)
      (set-cursor-color color)
    (send-string-to-terminal (format "\033]12;%s\007" color))))

(defconst dhnam-hydra-ijkl/plist
  '(:pre (dhnam-hydra-ijkl/set-cursor-color dhnam-hydra-ijkl/activated-cursor-color)
    :post (dhnam-hydra-ijkl/set-cursor-color dhnam-hydra-ijkl/default-cursor-color)))

(eval
 `(progn
    (progn
      (defhydra dhnam-hydra-ijkl
        ,dhnam-hydra-ijkl/plist

        "ijkl"

        ("i" previous-line)
        ("k" next-line)
        ("j" backward-char)
        ("l" forward-char)
        ("u" backward-word)
        ("o" forward-word)

        ("M-i" backward-up-list)
        ("M-k" down-list)
        ("M-I" paredit-forward-up)
        ("M-K" paredit-backward-down)

        ("M-j" backward-sexp)
        ("M-l" forward-sexp)
        ("M-u" backward-list)
        ("M-o" forward-list)

        ("C-i"  dhnam/scroll-down-small)
        ("C-k"  dhnam/scroll-up-small)
        ("I"  scroll-down)
        ("K"  scroll-up)

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

        ("\\" indent-region)

        ("y" yank)
        ("M-y" yank-pop)
        ("M-Y" dhnam/yank-pop-forwards)
        ("C-M-y" counsel-yank-pop)

        ("r" recenter-top-bottom)
        ("R" dhnam/reverse-recenter-top-bottom)
        ("t" move-to-window-line-top-bottom)
        ("T" dhnam/reverse-move-to-window-line-top-bottom)

        ;; ("SPC" set-mark-command)
        ("v" set-mark-command)
        ("'" exchange-point-and-mark)
        ("<f7>" pop-to-mark-command)
        ("<f8>" dhnam/unpop-to-mark-command)

        ("." xref-find-definitions)
        ("," xref-pop-marker-stack)

        ("g" keyboard-quit)
        ("/" undo)

        ("M-9" previous-buffer)
        ("M-0" next-buffer)
        (";" other-window)
        (":" dhnam/other-window-backwards)
        ("p" tab-next-repeat)
        ("P" tab-previous-repeat)

        ("#" eval-last-sexp)
        ("M-#" eval-print-last-sexp)

        (,dhnam-hydra-ijkl/quit-key nil "quit")
        ("RET" nil "quit")
        ("q" nil "quit"))

      (progn
        ;; Disable any hint message
        (hydra-set-property 'dhnam-hydra-ijkl :verbosity 0))

      (define-key global-map (kbd ,dhnam-hydra-ijkl/activation-key) 'dhnam-hydra-ijkl/body))

    (when (package-installed-p 'paredit)
      (require 'dhnam-paredit)

      (defhydra dhnam-hydra-paredit-struct
        ,dhnam-hydra-ijkl/plist

        "paredit structure edit"

        ("i" paredit-split-sexp)
        ("k" paredit-join-sexps)
        ("M-i" paredit-raise-sexp)
        ("M-k" paredit-convolute-sexp)

        ("j" paredit-backward-slurp-sexp)
        ("M-j" paredit-backward-barf-sexp)
        ("l" paredit-forward-slurp-sexp)
        ("M-l" paredit-forward-barf-sexp)

        ("j" paredit-splice-sexp-killing-backward)
        ("o" paredit-splice-sexp-killing-forward)
        
        )

      (clone-hydra dhnam-hydra-paredit-ijkl dhnam-hydra-ijkl
        ,dhnam-hydra-ijkl/plist

        "ijkl"

        ("w" dhnam/paredit-kill-ring-save)
        ("e" paredit-kill-region)
        ("d" paredit-forward-delete)
        ("D" paredit-forward-kill-word)
        ("f" paredit-kill)
        ("F" kill-sexp)
        ("DEL" paredit-backward-delete)
        ("M-DEL" paredit-backward-kill-word)

        ("(" paredit-open-round)
        (")" paredit-close-round)
        ("[" paredit-open-square)
        ("[" paredit-close-square)
        ("{" paredit-open-curly)
        ("}" paredit-close-curly)

        ("M-(" paredit-wrap-round)
        ("M-)" paredit-splice-sexp)
        ("M-[" paredit-wrap-square)
        ("M-]" paredit-splice-sexp)
        ("M-{" paredit-wrap-curly)
        ("M-}" paredit-splice-sexp))

      (hydra-set-property 'dhnam-hydra-paredit-ijkl :verbosity 0) ; disable any hint message
      (define-key paredit-mode-map (kbd ,dhnam-hydra-ijkl/activation-key) 'dhnam-hydra-paredit-ijkl/body)))) 
      

(provide 'dhnam-hydra-ijkl)
