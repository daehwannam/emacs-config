
(comment
  (define-key isearch-mode-map (kbd "M-j") 'isearch-yank-word-or-char)
  (progn
   (define-key isearch-mode-map (kbd "C-n") 'isearch-repeat-forward)
   (define-key isearch-mode-map (kbd "C-p") 'isearch-repeat-backward))
  (comment
    (define-key isearch-mode-map (kbd "M-f") 'isearch-repeat-forward)
    (define-key isearch-mode-map (kbd "M-b") 'isearch-repeat-backward))
  (define-key isearch-mode-map (kbd "M-g") 'isearch-abort))

(when (fboundp 'ctrlf-mode)
  (comment
    ;; disable explit `ctrlf-mode'
    (ctrlf-mode 1))

  (let ((keymap (current-global-map)))
    (define-key keymap [remap isearch-forward] #'ctrlf-forward-default)
    (define-key keymap [remap isearch-backward] #'ctrlf-backward-default)
    (define-key keymap [remap isearch-forward-regexp] #'ctrlf-forward-alternate)
    (define-key keymap [remap isearch-backward-regexp] #'ctrlf-backward-alternate)
    (define-key keymap [remap isearch-forward-symbol] #'ctrlf-forward-symbol)
    (define-key keymap [remap isearch-forward-symbol-at-point] #'ctrlf-forward-symbol-at-point))

  (progn
    ;; (key-chord-define-global "" #'ctrlf-forward-symbol)
    (key-chord-define-global "f." #'ctrlf-forward-symbol-at-point))

  (progn
    (define-key ctrlf-mode-map (kbd "C-n") 'ctrlf-forward-default)
    (define-key ctrlf-mode-map (kbd "C-p") 'ctrlf-backward-default)
    (define-key ctrlf-mode-map (kbd "C-v") #'ctrlf-next-page)
    (define-key ctrlf-mode-map (kbd "M-v")  #'ctrlf-previous-page)))
