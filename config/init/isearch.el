
(comment
  (define-key isearch-mode-map (kbd "M-j") 'isearch-yank-word-or-char)
  (progn
   (define-key isearch-mode-map (kbd "C-n") 'isearch-repeat-forward)
   (define-key isearch-mode-map (kbd "C-p") 'isearch-repeat-backward))
  (comment
    (define-key isearch-mode-map (kbd "M-f") 'isearch-repeat-forward)
    (define-key isearch-mode-map (kbd "M-b") 'isearch-repeat-backward))
  (define-key isearch-mode-map (kbd "M-g") 'isearch-abort))
