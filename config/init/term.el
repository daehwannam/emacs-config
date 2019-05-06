
(require 'multi-term)
(require 'named-term)

(defalias 'mterm 'multi-term)

(defun tmux-term ()
  "Create new term buffer, then do tmux new session.
It's modified code of 'multi-term.
Will prompt you shell name when you type `C-u' before this command."
  (interactive)
  (let (term-buffer)
    ;; Set buffer.
    (setq term-buffer (multi-term-get-buffer current-prefix-arg))
    (setq multi-term-buffer-list (nconc multi-term-buffer-list (list term-buffer)))
    (set-buffer term-buffer)
    ;; Internal handle for `multi-term' buffer.
    (multi-term-internal)
    ;; Switch buffer
    (switch-to-buffer term-buffer))
  (term-send-raw-string "tmux new \C-m"))

(defalias 'tterm 'tmux-term)
