
;; Emacs lisp 
(electric-indent-mode -1)
(add-hook 'emacs-lisp-mode-hook
	  (lambda () (local-set-key (kbd "C-c C-j") #'eval-print-last-sexp)))
(add-to-list 'auto-mode-alist '("\\.el\\.gz\\'" . emacs-lisp-mode))
;; (add-to-list 'auto-mode-alist '(".el\\.gz\\'" . emacs-lisp-mode))


;; Common lisp
(let ((path-to-slime-helper (machine-config-get 'path-to-slime-helper))
      (path-to-inferior-lisp-program (machine-config-get 'path-to-inferior-lisp-program)))
  (when path-to-slime-helper
    (load (expand-file-name path-to-slime-helper)))
  ;; Replace "sbcl" with the path to your implementation
  (when path-to-inferior-lisp-program
    (setq inferior-lisp-program path-to-inferior-lisp-program))
  (when (fboundp 'slime)
    ;; (add-hook 'slime-mode-hook
    ;; 	      (lambda () (local-set-key (kbd "C-j") #'slime-eval-print-last-expression)))
    (add-hook 'slime-mode-hook
	      (lambda () (local-set-key (kbd "C-c C-d H") #'slime-documentation))))
  )

;; ;; ;; ParEdit
;; ;; ;; http://wikemacs.org/wiki/Paredit-mode
;; (when (functionp 'paredit-mode)
;;   (autoload 'enable-paredit-mode "paredit"
;;     "Turn on pseudo-structural editing of Lisp code."
;;     t)
;;   (add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)
;;   (add-hook 'lisp-mode-hook             'enable-paredit-mode)
;;   (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
;;   (add-hook 'scheme-mode-hook           'enable-paredit-mode))
