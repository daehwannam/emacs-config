
(when (package-installed-p 'vertico)
  (vertico-mode -1))

(require 'dhnam-vertico)

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
    (define-key vertico-map (kbd "C-v") #'vertico-scroll-up)
    (define-key vertico-map (kbd "M-i") #'vertico-scroll-down)
    (define-key vertico-map (kbd "M-o") #'vertico-scroll-up)
    (comment (define-key vertico-map (kbd "M-9") #'vertico-previous-group))
    (comment (define-key vertico-map (kbd "M-0") #'vertico-next-group))
    (define-key vertico-map (kbd "M-9") #'dhnam/vertico-goto-previous-group)
    (define-key vertico-map (kbd "M-0") #'dhnam/vertico-goto-next-group)))

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
      (defun dhnam/vertico-posframe-poshandler-advice-to-display-at-frame-center (orig-func &rest args)
        (let ((vertico-posframe-poshandler #'posframe-poshandler-frame-center))
          (apply orig-func args)))
      (progn
        ;; for find-file
        (advice-add 'read-file-name :around #'dhnam/vertico-posframe-poshandler-advice-to-display-at-frame-center))
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

      (defun dhnam/completion-style-advice (orig-func &rest args)
        (let ((completion-styles '(orderless))
              (completion-category-defaults nil)
              (completion-category-overrides '((file (styles partial-completion)))))
          (apply orig-func args))))))



(when (and (fboundp 'vertico--advice) (fboundp 'dhnam/completion-style-advice))
  (defun dhnam/add-advice-for-vertico (&rest functions)
    (mapcar (lambda (x) (progn (advice-add x :around #'vertico--advice)
                               (advice-add x :around #'dhnam/completion-style-advice)))
            functions))

  (comment
    ;; for find-file
    (dhnam/add-advice-for-vertico 'read-file-name)
    (comment (key-chord-define-global "f;" 'firnd-file)))

  (use-existing-pkg consult
    :init
    (progn
      (progn
        ;; for consult-line, consult-grep, ...
        (apply #'dhnam/add-advice-for-vertico (append '(consult--line consult--line-multi-candidates)
                                                      '(consult--grep consult--read))))

      (progn
        (comment (key-chord-define-global "js" 'consult-line))
        (key-chord-define-global "jt" 'consult-line-multi)

        ;; use C-u prefix to set directory for git-related commands
        (comment
          ;; search over the current git project
          (key-chord-define-global "g;" 'consult-grep))
        (progn
          ;; search over the current directory
          (key-chord-define-global "g;" 'dhnam/consult-grep-on-default-directory))
        (key-chord-define-global "gj" 'consult-git-grep)

        (progn
          ;; Find a file recursively
          ;; https://www.reddit.com/r/emacs/comments/skd03i/comment/hvksm0r/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
          (comment (key-chord-define-global "F:" 'dhnam/consult-find-from-current-dir))
          (key-chord-define-global "r;" 'dhnam/consult-find-from-current-dir))))
    :config
    (progn
      (define-key minibuffer-local-map (kbd "M-m") 'dhnam/minibuffer-toggle-symbol-boundaries))
    :after (vertico orderless)))

(comment
  ;; `marginalia' raises a face problem of `counsel-switch-buffer'

  (use-existing-pkg marginalia
    ;; Enable richer annotations using the Marginalia package

    ;; Either bind `marginalia-cycle` globally or only in the minibuffer
    :bind (("M-A" . marginalia-cycle)
           :map minibuffer-local-map
           ("M-A" . marginalia-cycle))

    ;; The :init configuration is always executed (Not lazy!)
    :init

    ;; Must be in the :init section of use-package such that the mode gets
    ;; enabled right away. Note that this forces loading the package.
    (marginalia-mode)))

(comment
  ;; Fix for the small font problem
  ;; https://github.com/minad/vertico/issues/190
  (set-face-attribute 'completions-common-part nil :height 'unspecified))

(provide 'init-vertico)
