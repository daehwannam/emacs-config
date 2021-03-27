
(when (package-installed-p 'eglot)
  (require 'eglot)
  (add-hook 'python-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook 'company-mode)

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
