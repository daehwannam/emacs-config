(comment
  (use-existing-pkg vertico
    ;; Enable vertico
    :init
    (vertico-mode)

    ;; Different scroll margin
    ;; (setq vertico-scroll-margin 0)

    ;; Show more candidates
    ;; (setq vertico-count 20)

    ;; Grow and shrink the Vertico minibuffer
    ;; (setq vertico-resize t)

    ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
    (setq vertico-cycle t)

    (progn
      ;; A few more useful configurations...
      ;; 
      ;; Add prompt indicator to `completing-read-multiple'.
      ;; Alternatively try `consult-completing-read-multiple'.
      (defun crm-indicator (args)
        (cons (concat "[CRM] " (car args)) (cdr args)))
      (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

      ;; Do not allow the cursor in the minibuffer prompt
      (setq minibuffer-prompt-properties
            '(read-only t cursor-intangible t face minibuffer-prompt))
      (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

      ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
      ;; Vertico commands are hidden in normal buffers.
      ;; (setq read-extended-command-predicate
      ;;       #'command-completion-default-include-p)

      ;; Enable recursive minibuffers
      (setq enable-recursive-minibuffers t))

    (define-key vertico-map (kbd "C-j") #'vertico-exit-input))

  (comment
    ;; `orderless' is necessary for `consult-lin'

    (use-existing-pkg orderless
      ;; Optionally use the `orderless' completion style. See
      ;; `+orderless-dispatch' in the Consult wiki for an advanced Orderless style
      ;; dispatcher. Additionally enable `partial-completion' for file path
      ;; expansion. `partial-completion' is important for wildcard support.
      ;; Multiple files can be opened at once with `find-file' if you enter a
      ;; wildcard. You may also give the `initials' completion style a try.
      :init
      ;; Configure a custom style dispatcher (see the Consult wiki)
      ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
      ;;       orderless-component-separator #'orderless-escapable-split-on-space)

      (setq completion-styles '(orderless))
      (comment
        ;; the beolow code doesn't work with `shell-command'
        (setq completion-styles '(orderless)
              completion-category-defaults nil
              completion-category-overrides '((file (styles partial-completion)))))))

  (use-existing-pkg savehist
    ;; Persist history over Emacs restarts. Vertico sorts by history position.
    :init
    (savehist-mode))

  (use-existing-pkg vertico-posframe
    :init
    (setq vertico-posframe-poshandler #'posframe-poshandler-frame-bottom-left-corner)
    (vertico-posframe-mode 1))

  (comment
   (use-existing-pkg mini-frame
     :init
     (mini-frame-mode 1)

     (progn
       ;; set mini-buffer to the bottom
       ;; https://github.com/muffinmad/emacs-mini-frame/issues/1
       ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Position-Parameters.html

       (custom-set-variables
        '(mini-frame-show-parameters
          '((left . 0.5)
            (top . -100)
            (width . 1.0)
            (height . 1)))))))

  ;; Enable richer annotations using the Marginalia package
  (use-existing-pkg marginalia
    ;; Either bind `marginalia-cycle` globally or only in the minibuffer
    :bind (("M-A" . marginalia-cycle)
           :map minibuffer-local-map
           ("M-A" . marginalia-cycle))

    ;; The :init configuration is always executed (Not lazy!)
    :init

    ;; Must be in the :init section of use-package such that the mode gets
    ;; enabled right away. Note that this forces loading the package.
    (marginalia-mode))

  (use-existing-pkg consult
    :init
    (progn
      (key-chord-define-global "js" 'consult-line)))
  )
