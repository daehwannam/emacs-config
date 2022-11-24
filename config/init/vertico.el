
(when (package-installed-p 'vertico)
  (vertico-mode -1))

(use-existing-pkg vertico
  ;; Enable vertico
  :init
  (progn
    (progn
      ;; disable applying vertico for all completion
      (vertico-mode -1))

    ;; Different scroll margin
    ;; (setq vertico-scroll-margin 0)

    ;; Show more candidates
    ;; (setq vertico-count 20)

    ;; Grow and shrink the Vertico minibuffer
    ;; (setq vertico-resize t)

    ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
    (comment (setq vertico-cycle t))

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

    (define-key vertico-map (kbd "C-j") #'vertico-exit-input)
    (define-key vertico-map (kbd "M-v") #'vertico-scroll-down)
    (define-key vertico-map (kbd "C-v") #'vertico-scroll-up)))

(when (display-graphic-p)
  (use-existing-pkg vertico-posframe
    ;; Note: `awesome-tray-mode' is not compatible with the default
    ;; mini-buffer by `vertico' in the newly created frame for GUI
    ;; emacs.  Therfore, `vertico-posframe' should be enabled.
    :init
    (progn
      (progn
        ;; set the default poshandler
        (setq vertico-posframe-poshandler #'posframe-poshandler-frame-bottom-center))
      (defun my-vertico-posframe-poshandler-advice-to-display-at-frame-center (orig-func &rest args)
        (let ((vertico-posframe-poshandler #'posframe-poshandler-frame-center))
          (apply orig-func args)))
      (progn
        ;; for find-file
        (advice-add 'read-file-name :around #'my-vertico-posframe-poshandler-advice-to-display-at-frame-center))
      (vertico-posframe-mode 1))))

(use-existing-pkg savehist
  ;; Persist history over Emacs restarts. Vertico sorts by history position.
  :init
  (savehist-mode))

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

(progn
  ;; `orderless' is necessary for `consult-line'
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

    (progn
      (comment
        (setq completion-styles '(orderless)))

      (comment
        ;; the beolow code doesn't work with `shell-command'
        (setq completion-styles '(orderless)
              completion-category-defaults nil
              completion-category-overrides '((file (styles partial-completion)))))

      (defun my-completion-style-advice (orig-func &rest args)
        (let ((completion-styles '(orderless))
              (completion-category-defaults nil)
              (completion-category-overrides '((file (styles partial-completion)))))
          (apply orig-func args))))))



(when (and (fboundp 'vertico--advice) (fboundp 'my-completion-style-advice))
  (defun add-advice-for-vertico (&rest functions)
    (mapcar (lambda (x) (progn (advice-add x :around #'vertico--advice)
                               (advice-add x :around #'my-completion-style-advice)))
            functions))

  (comment
    ;; for find-file
    (add-advice-for-vertico 'read-file-name)
    (comment (key-chord-define-global "f;" 'firnd-file)))

  (use-existing-pkg consult
    :init
    (progn
      (progn
        ;; for consult-line, consult-grep, ...
        (apply #'add-advice-for-vertico (append '(consult--line consult--line-multi-candidates)
                                                '(consult--grep consult--read))))

      (defun consult-grep-on-default-directory (&optional dir initial)
        (interactive "P")
        (setq dir (or dir default-directory))
        (consult--grep "Grep" #'consult--grep-builder dir initial))

      (progn
        (comment (key-chord-define-global "js" 'consult-line))
        (key-chord-define-global "jt" 'consult-line-multi)
        ;; (key-chord-define-global "g;" 'consult-grep) ; use C-u prefix to set directory
        (key-chord-define-global "g;" 'consult-grep-on-default-directory) ; use C-u prefix to set directory
        (key-chord-define-global "gj" 'consult-git-grep)))
    :after (vertico orderless)))

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
