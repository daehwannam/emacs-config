
(require 'yasnippet)
(require 'dhnam-yasnippet)

(progn
  ;; https://github.com/joaotavora/yasnippet
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'LaTeX-mode-hook #'yas-minor-mode)
  (add-hook 'org-mode-hook #'yas-minor-mode))

(progn
  ;; delete default yassnippet snippets
  (delete 'yasnippet-classic-snippets-dir yas-snippet-dirs)
  (delete 'yasnippet-snippets-dir yas-snippet-dirs))

(provide 'init-yasnippet)
