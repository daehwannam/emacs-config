
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

(defconst dhnam-ijkl/plist-1
  '(:pre (dhnam-ijkl/set-cursor-color dhnam-ijkl/activated-cursor-color)
    :post (dhnam-ijkl/set-cursor-color dhnam-ijkl/default-cursor-color)))

(defconst dhnam-ijkl/plist-2
  '(:pre (dhnam-ijkl/set-cursor-color dhnam-ijkl-paredit-struct-cursor-color)
    :post (dhnam-ijkl/set-cursor-color dhnam-ijkl/default-cursor-color)))

(eval
 `(progn
    (progn
      ;; dhnam-ijkl
      (defhydra dhnam-ijkl
        ,dhnam-ijkl/plist-1

        "ijkl"

        ("i" previous-line)
        ("k" next-line)
        ("j" backward-char)
        ("l" forward-char)
        ("u" backward-word)
        ("o" forward-word)

        ("M-i" backward-up-list)
        ("M-I" down-list)
        ("M-k" paredit-forward-up)
        ("M-K" paredit-backward-down)

        ("M-j" backward-sexp)
        ("M-l" forward-sexp)
        ("M-u" backward-list)
        ("M-o" forward-list)

        ("M-h"  dhnam/scroll-down-small)
        ("h"  dhnam/scroll-up-small)
        ("M-H"  scroll-down)
        ("H"  scroll-up)

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

        ("SPC w ." dhnam/kill-ring-save-at-point)
        ("SPC w p" dhnam/kill-path-to-clipboard)
        ("SPC w l" dhnam/org-kill-link-to-clipboard)

        ("RET" newline)
        ("n" electric-newline-and-maybe-indent)
        ("M-n" default-indent-new-line)
        ("M-SPC" dhnam/just-one-space-conditionally)
        ("M-\\" delete-horizontal-space)
        ("b" indent-for-tab-command)

        ("\\" indent-region)

        ("y" yank)
        ("M-y" yank-pop)
        ("M-Y" dhnam/yank-pop-forwards)
        ("C-M-y" counsel-yank-pop)

        ("r" recenter-top-bottom)
        ("R" dhnam/reverse-recenter-top-bottom)
        ("t" move-to-window-line-top-bottom)
        ("T" dhnam/reverse-move-to-window-line-top-bottom)

        ("v" set-mark-command)
        ("'" exchange-point-and-mark)
        ("<f7>" pop-to-mark-command)
        ("<f8>" dhnam/unpop-to-mark-command)

        ;; ("SPC s s" dhnam/swiper-within-region)
        ;; ("SPC s f" ctrlf-forward-default)
        ;; ("SPC s r" rgrep)
        ("." xref-find-definitions)
        ("," xref-pop-marker-stack)

        ("g" keyboard-quit)
        ("/" undo)

        ("#" eval-last-sexp)
        ("M-#" eval-print-last-sexp)

        (";" comment-dwim)
        (":" eval-expression)
        ;; ("SPC ;" comment-dwim)
        ;; ("SPC :" eval-expression)

        ("M-9" previous-buffer)
        ("M-0" next-buffer)

        ;; ("SPC 2" split-window-below)
        ;; ("SPC 3" split-window-right)
        ;; (";" other-window)
        ;; (":" dhnam/other-window-backwards)
        ;; ("p" tab-next)
        ;; ("P" tab-previous)
        ("SPC d" dhnam-ijkl-window/body :exit t)
        ("SPC f" dhnam-ijkl-tab/body :exit t)

        ("C-h k" describe-key)
        (,dhnam-ijkl/quit-key nil "quit")
        ;; ("RET" nil "quit")
        ("q" nil "quit"))

      (progn
        ;; Disable any hint message
        (hydra-set-property 'dhnam-ijkl :verbosity 0))

      (define-key global-map (kbd ,dhnam-ijkl/activation-key) 'dhnam-ijkl/body))

    (progn
      (defhydra dhnam-ijkl-window
        ,dhnam-ijkl/plist-2

        "ijkl"

        ("k" dhnam/other-window-backwards)
        ("l" other-window)
        ("i" dhnam/other-window-backwards)
        ("o" other-window)
        ("0" delete-window)
        ("9" delete-other-windows)
        ("8" split-window-below)
        ("7" split-window-right)

        (,dhnam-ijkl/quit-key dhnam-ijkl/body :exit t))

      (hydra-set-property 'dhnam-ijkl-window :verbosity 0))

    (progn
      (defhydra dhnam-ijkl-tab
        ,dhnam-ijkl/plist-2

        "ijkl"

        ("k" tab-previous)
        ("l" tab-next)
        ("i" tab-previous)
        ("o" tab-next)
        ("0" tab-close)
        ("9" tab-close-other)
        ("8" tab-new)

        (,dhnam-ijkl/quit-key dhnam-ijkl/body :exit t))

      (hydra-set-property 'dhnam-ijkl-tab :verbosity 0))

    (when (package-installed-p 'paredit)
      (require 'dhnam-paredit)

      (defhydra dhnam-ijkl-paredit-struct
        ,dhnam-ijkl/plist-2

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

        (,dhnam-ijkl/quit-key dhnam-ijkl/body :exit t)
        ("q" nil "quit"))

      (hydra-set-property 'dhnam-ijkl-paredit-struct :verbosity 0) ; disable any hint message

      (clone-hydra dhnam-ijkl-paredit-move dhnam-ijkl
        ,dhnam-ijkl/plist-1

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
        ("M-n" default-indent-new-line)

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

        ;; ("SPC e" dhnam-ijkl-paredit-struct/body  :exit t)
        ("c" dhnam-ijkl-paredit-struct/body  :exit t))

      (hydra-set-property 'dhnam-ijkl-paredit-move :verbosity 0) ; disable any hint message
      (define-key paredit-mode-map (kbd ,dhnam-ijkl/activation-key) 'dhnam-ijkl-paredit-move/body))

    (with-eval-after-load 'vterm-seamless
      (clone-hydra dhnam-ijkl-vterm dhnam-ijkl
        ,dhnam-ijkl/plist-1

        "ijkl"

        (,dhnam-ijkl/quit-key vtsl/copy-mode-exit :exit t))

      (let ((map vterm-seamless-mode-map))
        (define-key map (kbd ,dhnam-ijkl/activation-key) (vtsl/copy-mode-then 'dhnam-ijkl-vterm/body)))

      (let ((map vterm-seamless-copy-mode-map))
        (comment))

      ;; disable any hint message
      (hydra-set-property 'dhnam-ijkl-vterm :verbosity 0))))
 

(provide 'dhnam-ijkl)
