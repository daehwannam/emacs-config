
(require 'dhnam-macro)
(require 'dhnam-hydra)

(unless (fboundp 'comment)
  (defmacro comment (&rest args)
    `nil))

(defvar dhnam-ijkl/default-cursor-color "orchid")
(defvar dhnam-ijkl/activated-cursor-color "cyan")
(defvar dhnam-ijkl-paredit-struct-cursor-color "orange")
(defvar dhnam-ijkl-paredit-struct-cursor-color "orange")

(defconst dhnam-ijkl/activation-key "₢")
(defconst dhnam-ijkl/quit-key "₫")

(defun dhnam-ijkl/set-cursor-color (color)
  (if (display-graphic-p)
      (set-cursor-color color)
    (send-string-to-terminal (format "\033]12;%s\007" color))))

(dhnam-ijkl/set-cursor-color dhnam-ijkl/default-cursor-color)

(defconst dhnam-ijkl/plist
  '(:pre (dhnam-ijkl/set-cursor-color dhnam-ijkl/activated-cursor-color)
    :post (dhnam-ijkl/set-cursor-color dhnam-ijkl/default-cursor-color)))

(defconst dhnam-ijkl/paredit-struct-plist
  '(:pre (dhnam-ijkl/set-cursor-color dhnam-ijkl-paredit-struct-cursor-color)
    :post (dhnam-ijkl/set-cursor-color dhnam-ijkl/default-cursor-color)))

(eval
 `(progn
    (progn
      ;; dhnam-ijkl
      (defhydra dhnam-ijkl
        ,dhnam-ijkl/plist

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
        ("M-DEL" baqckward-kill-word)

        ;; ("W ." dhnam/kill-ring-save-at-point)
        ;; ("W p" dhnam/kill-path-to-clipboard)
        ;; ("W l" dhnam/org-kill-link-to-clipboard)

        ("n" electric-newline-and-maybe-indent)
        ("M-j" default-indent-new-line)

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

        ("SPC ;" comment-dwim)
        ("SPC :" eval-expression)

        (,dhnam-ijkl/quit-key nil "quit")
        ;; ("RET" nil "quit")
        ("q" nil "quit"))

      (progn
        ;; Disable any hint message
        (hydra-set-property 'dhnam-ijkl :verbosity 0))

      (define-key global-map (kbd ,dhnam-ijkl/activation-key) 'dhnam-ijkl/body))

    (when (package-installed-p 'paredit)
      (require 'dhnam-paredit)

      (defhydra dhnam-ijkl-paredit-struct
        ,dhnam-ijkl/paredit-struct-plist

        "paredit structure editing"

        ("i" paredit-split-sexp)
        ("k" paredit-join-sexps)
        ("M-i" paredit-raise-sexp)
        ("M-k" paredit-convolute-sexp)

        ("j" paredit-backward-slurp-sexp)
        ("M-j" paredit-backward-barf-sexp)
        ("l" paredit-forward-slurp-sexp)
        ("M-l" paredit-forward-barf-sexp)

        ("u" paredit-splice-sexp-killing-backward)
        ("o" paredit-splice-sexp-killing-forward)

        ("/" undo)

        ;; (,dhnam-ijkl/quit-key dhnam-ijkl/body :exit t)
        ("q" nil "quit"))

      (hydra-set-property 'dhnam-ijkl-paredit-struct :verbosity 0) ; disable any hint message

      (clone-hydra dhnam-ijkl-paredit-move dhnam-ijkl
        ,dhnam-ijkl/plist

        "ijkl"

        ("w" dhnam/paredit-kill-ring-save)
        ("e" paredit-kill-region)
        ("d" paredit-forward-delete)
        ("D" paredit-forward-kill-word)
        ("f" paredit-kill)
        ("F" kill-sexp)
        ("DEL" paredit-backward-delete)
        ("M-DEL" paredit-backward-kill-word)

        ("n" paredit-newline)
        ("M-j" default-indent-new-line)

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
        ("M-}" paredit-splice-sexp)

        ("SPC ;" dhnam/paredit-comment-dwim)

        (comment ("SPC e" dhnam-ijkl-paredit-struct/body  :exit t))
        ("c" dhnam-ijkl-paredit-struct/body  :exit t))

      (hydra-set-property 'dhnam-ijkl-paredit-move :verbosity 0) ; disable any hint message
      (define-key paredit-mode-map (kbd ,dhnam-ijkl/activation-key) 'dhnam-ijkl-paredit-move/body)))) 


(provide 'dhnam-ijkl)
