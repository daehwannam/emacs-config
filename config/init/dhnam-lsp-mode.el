
(comment
 ;; setup information
 ;; 
 ;; https://www.mattduck.com/lsp-python-getting-started.html
 ;; https://vxlabs.com/2018/06/08/python-language-server-with-emacs-and-lsp-mode/
 )

(comment
 (use-existing-pkg lsp-mode
   :init
   (progn
     (setq lsp-keymap-prefix "C-c C-o")
     (comment
      ;; key-chord doesn't work
      (fset 'lsp-command-map lsp-command-map)
      (key-chord-define-global "cl" 'lsp-command-map))
     (comment (setq lsp-auto-guess-root t))

     ;; when debugging, change lsp-log-io as true
     (setq lsp-log-io nil))
   :config
   (progn
     (comment
      ;; https://github.com/emacs-lsp/lsp-ui/issues/276#issuecomment-487319894
      (defalias 'lsp--cur-line-diagnotics 'lsp--cur-line-diagnostics))

     (comment
      ;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
      ;; disable flycheck
      (comment (setq lsp-diagnostics-provider :auto))
      (comment (setq lsp-diagnostics-provider :flycheck))
      (setq lsp-diagnostics-provider :none)))

   :hook
   (;; list 'XXX-mode paired with 'lsp or 'lsp-deferred
    ;; 'lsp-deferred is for deferring LSP server startup until the buffer is visible
    (python-mode . lsp-deferred)
    ;; if you want which-key integration
    (lsp-mode . lsp-enable-which-key-integration))
   :commands lsp))


(use-existing-pkg lsp-mode
  :init
  (progn
    (setq lsp-keymap-prefix "C-c C-o")
    (comment
     ;; key-chord doesn't work
     (fset 'lsp-command-map lsp-command-map)
     (key-chord-define-global "cl" 'lsp-command-map))
    (comment (setq lsp-auto-guess-root t))

    ;; when debugging, change lsp-log-io as true
    (setq lsp-log-io nil))
  :config
  (progn
    (comment
     ;; https://github.com/emacs-lsp/lsp-ui/issues/276#issuecomment-487319894
     (defalias 'lsp--cur-line-diagnotics 'lsp--cur-line-diagnostics))
    (setq lsp-idle-delay 0.5
          lsp-enable-symbol-highlighting t
          lsp-enable-snippet nil ;; Not supported by company capf, which is the recommended company backend
	  lsp-pyls-plugins-flake8-enabled t
          ;; lsp-pyls-plugins-flake8-enabled nil
	  )

    (comment
     ;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
     ;; disable flycheck
     (comment (setq lsp-diagnostics-provider :auto))
     (comment (setq lsp-diagnostics-provider :flycheck))
     (setq lsp-diagnostics-provider :none))

    (lsp-register-custom-settings
     '(
       ("pyls.plugins.pyls_mypy.enabled" t t)
       ("pyls.plugins.pyls_mypy.live_mode" nil t)
       ("pyls.plugins.pyls_black.enabled" t t)
       ("pyls.plugins.pyls_isort.enabled" t t)
       ("pyls.plugins.flake8.enabled" t t)
       ;; ("pyls.plugins.flake8.enabled" nil t)

       ;; Disable these as they're duplicated by flake8
       ("pyls.plugins.pycodestyle.enabled" nil t)
       ("pyls.plugins.mccabe.enabled" nil t)
       ("pyls.plugins.pyflakes.enabled" nil t)
       )))

  :hook
  (;; list 'XXX-mode paired with 'lsp or 'lsp-deferred
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
