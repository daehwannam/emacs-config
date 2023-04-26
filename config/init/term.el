
(require 'dhnam-term)

(progn
  ;; char mode binding
  (define-key term-raw-map (kbd "M-x") (key-binding (kbd "M-x")))
  (comment
    term-raw-map
    ;; this doesn't work
    (add-to-list 'term-bind-key-alist `("M-x" . ,(key-binding (kbd "M-x"))))))

(progn
  ;; line-mode bindings
  (define-key term-mode-map (kbd "C-a") 'dhnam/move-beginning-of-command-line)
  (define-key term-mode-map (kbd "C-c C-p") 'dhnam/term-previous-prompt)
  (define-key term-mode-map (kbd "C-c C-n") 'dhnam/term-next-prompt)
  (define-key term-mode-map (kbd "M-P") 'term-previous-matching-input-from-input)
  (define-key term-mode-map (kbd "M-N") 'term-next-matching-input-from-input))


(provide 'init-term)
