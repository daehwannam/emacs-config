
(when (package-installed-p 'eglot)
  (require 'eglot)
  (progn
    (add-hook 'python-mode-hook 'eglot-ensure)
    (add-hook 'python-mode-hook 'company-mode))

  (progn
    (add-hook 'LaTeX-mode-hook 'eglot-ensure)
    (add-hook 'LaTeX-mode-hook 'company-mode)
    ;; (add-hook 'tex-mode-hook 'eglot-ensure)
    ;; (add-hook 'tex-mode-hook 'company-mode)
    )

  (progn
    ;; https://github.com/joaotavora/eglot/issues/607
    (custom-set-variables
     '(eglot-ignored-server-capabilites '(list
					                      ;; :documentHighlightProvider
					                      :hoverProvider
					                      ;; :signatureHelpProvider
					                      )))))

(comment
 ;; eldoc-doc-buffer: C-h .
 )

(provide 'dhnam-eglot)
