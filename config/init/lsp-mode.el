
(use-existing-pkg lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "c l")
  :hook (;; list 'XXX-mode paired with 'lsp or 'lsp-deferred
	 ;; 'lsp-deferred is for deferring LSP server startup until the buffer is visible
         (python-mode . lsp-deferred)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-existing-pkg lsp-ui :commands lsp-ui-mode)

;; if you are ivy user
(use-existing-pkg lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-existing-pkg lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-existing-pkg dap-mode)
(use-existing-pkg dap-python) ; to load the dap adapter for your language

;; optional if you want which-key integration
(use-existing-pkg which-key
    :config
    (which-key-mode))

(use-existing-pkg lsp-mode
    :hook (XXX-mode . lsp-deferred)
    :commands (lsp lsp-deferred))
