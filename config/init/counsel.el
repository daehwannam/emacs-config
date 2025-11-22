(require 'dhnam-counsel)

(use-existing-pkg ivy
  :config
  ;; (ivy-mode 1)
  (ivy-define-key ivy-switch-buffer-map (kbd "C-SPC") 'dhnam/ivy-toggle-mark)
  (ivy-define-key ivy-minibuffer-map (kbd "M-n") 'dhnam/ivy-next-history-element)
  (ivy-define-key ivy-minibuffer-map (kbd "M-m") 'dhnam/ivy-toggle-symbol-boundaries)
  (ivy-define-key ivy-minibuffer-map (kbd "C-y") 'dhnam/swiper-yank)

  ;; :bind
  ;; (nil
  ;;  ;; ("C-c C-M-k" . dhnam/ivy-kill-marked)
  ;;  ;; :map ivy-minibuffer-map
  ;;  ;; ("C-k" . dhnam/ivy-kill-marked)
  ;;  )
  )

(comment
  ;; `ivy--call-marked' take an action, such as `ivy--kill-buffer-action' over marked items.
  )

(use-existing-pkg counsel
  :init
  (progn
    (defun swiper--recenter-p ()
      ;; fix for not applying scroll on exit in terminal emacs
      ;; https://github.com/abo-abo/swiper/issues/2159#issuecomment-514967202
      t)

    (key-chord-define-global "qm" 'dhnam/counsel-switch-buffer-within-same-major-mode)

    (progn
      ;; ivy mode

      ;; http://oremacs.com/swiper/
      ;; Initialization
      ;; (ivy-mode 1)
      (comment (setq ivy-use-virtual-buffers t))
      (setq ivy-use-virtual-buffers nil)
      (setq ivy-count-format "(%d/%d) ")

      (progn
	    ;; swiper
	    ;; Ivy-based interface to standard commands
	    ;; (global-set-key (kbd "C-s") 'isearch-forward)
	    ;; (global-set-key (kbd "C-c s") 'swiper)
	    (comment (key-chord-define-global "js" 'swiper))
	    (comment (key-chord-define-global "js" 'dhnam/swiper-within-region))
        (key-chord-define-global "jf" 'dhnam/swiper-within-region)
	    (key-chord-define-global "jw" 'dhnam/swiper-symbol-at-point)
	    (comment (space-chord-define-global "s" 'swiper))
	    ;; (global-set-key (kbd "C-c C-s") 'swiper) ; conflict with 'elpy-rgrep-symbol
	    ;; (global-set-key (kbd "C-r") 'swiper)
	    )

      ;; Ivy-resume and other commands
      ;; ivy-resume resumes the last Ivy-based completion.
      (global-set-key (kbd "C-c C-r") 'ivy-resume))

    (key-chord-define-global "qj" 'counsel-switch-buffer)
    (comment
     (unless (and (boundp 'dhnam/exwm-cmd-line-arg-passed) dhnam/exwm-cmd-line-arg-passed)
       (global-set-key (kbd "C-x b") 'counsel-switch-buffer)
       (global-set-key (kbd "C-x B") 'switch-to-buffer)))

    (progn
      ;; counsel-mode
      (global-set-key (kbd "M-x") 'counsel-M-x)
      (key-chord-define-global ";f" 'counsel-find-file)
      ;; (global-set-key (kbd "C-x C-f") 'counsel-find-file)
      (global-set-key (kbd "<f1> f") 'counsel-describe-function)
      (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
      (global-set-key (kbd "<f1> l") 'counsel-find-library)
      (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
      (global-set-key (kbd "<f2> u") 'counsel-unicode-char)

      ;; Ivy-based interface to shell and system tools
      (global-set-key (kbd "C-c g") 'counsel-git)
      (global-set-key (kbd "C-c j") 'counsel-git-grep)
      (global-set-key (kbd "C-c k") 'counsel-ag)
      (global-set-key (kbd "C-x l") 'counsel-locate)
      (global-set-key (kbd "C-S-o") 'counsel-rhythmbox))

    ;; (global-set-key (kbd "C-c C-s") 'dhnam/swiper-with-text-in-region)
    )

  :after (key-chord)

  :config
  (progn
    (progn
      ;; replace `ivy-partial-or-done'
      (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-partial)

      ;; disable magic-tilde
      (setq ivy-magic-tilde nil))))

(use-existing-pkg counsel-projectile-mode
  :init
  (counsel-projectile-mode))

(provide 'init-counsel)
