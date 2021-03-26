
(unless (fboundp 'jsonnet-mode)
  (add-to-list 'auto-mode-alist '("\\.jsonnet\\'" . js-mode)))

(add-to-list 'auto-mode-alist '("\\.jsonl\\'" . js-mode))
