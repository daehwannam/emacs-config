
(when (package-installed-p 'eglot)
  ;; (require 'eglot)

  (progn
    (add-hook 'python-mode-hook 'eglot-ensure)
    (add-hook 'python-mode-hook 'company-mode))

  (progn
    (add-hook 'js-mode-hook 'eglot-ensure)
    (add-hook 'js-mode-hook 'company-mode))

  (comment
    (with-eval-after-load 'eglot
      ;; python language-server configuration
      ;; https://joaotavora.github.io/eglot/#Setting-Up-LSP-Servers
      (add-to-list 'eglot-server-programs
                   `(python-mode . ,(eglot-alternatives '(("pylsp") ("pyls") ("pyright" "--stdio")))))))

  (unless (string= (shell-command-to-string "command -v digestif") "")
    ;; Digestif setup (https://github.com/astoff/digestif)
    ;; https://github.com/astoff/digestif#luatex-with-the-self-installing-script-
    ;;
    ;; #+begin_src sh
    ;; LOCAL_BIN=~/.local/bin/  # Assume that $LOCAL_BIN is in $PATH
    ;; mkdir -p $LOCAL_BIN
    ;; cd $LOCAL_BIN
    ;; wget https://raw.githubusercontent.com/astoff/digestif/master/scripts/digestif
    ;; chmod +x digestif
    ;; #+end_src
    ;;
    ;; The first run of digestif makes "~/.digestif".

    (add-hook 'LaTeX-mode-hook 'eglot-ensure)
    (add-hook 'LaTeX-mode-hook 'company-mode)
    (comment (add-hook 'tex-mode-hook 'eglot-ensure))
    (comment (add-hook 'tex-mode-hook 'company-mode)))

  (progn
    ;; https://github.com/joaotavora/eglot/issues/607
    (custom-set-variables
     '(eglot-ignored-server-capabilites '(list
					                      ;; :documentHighlightProvider
					                      :hoverProvider
					                      ;; :signatureHelpProvider
					                      ))))

  (with-eval-after-load 'eglot
    (define-key eglot-mode-map (kbd "C-c R") 'eglot-reconnect)

    (progn
      ;; Adding filters
      ;; e.g. "*EGLOT (some-project/(python-mode python-ts-mode)) events*"
      (add-to-list 'consult-buffer-filter "\\`\\*EGLOT .* events\\*\\'")
      (add-to-list 'consult-buffer-filter "\\`\\*Python\\*\\'"))))

(comment
  ;; eldoc-doc-buffer: C-h .
  )

(provide 'init-eglot)
