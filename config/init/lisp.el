
;; Emacs lisp 
(electric-indent-mode -1)
(add-hook 'emacs-lisp-mode-hook
	  (lambda () (local-set-key (kbd "C-j") #'eval-print-last-sexp)))
(add-to-list 'auto-mode-alist '("\\.el\\.gz\\'" . emacs-lisp-mode))
;; (add-to-list 'auto-mode-alist '(".el\\.gz\\'" . emacs-lisp-mode))


;; Common lisp
(let ((path-to-slime-helper "~/quicklisp/slime-helper.el")
      (path-to-inferior-lisp-program "/usr/bin/sbcl"))
  (cond
   ((string-equal machine-domain "vbox")
    (setq path-to-slime-helper "~/quicklisp/slime-helper.el")
    (setq path-to-inferior-lisp-program "/usr/bin/sbcl"))
   (t
    (setq path-to-slime-helper "~/NO-PATH-POSSIBLE")
    (setq path-to-inferior-lisp-program "/NO-PATH-POSSIBLE")))    
  (when (file-exists-p  path-to-slime-helper)
    (load (expand-file-name path-to-slime-helper)))
    ;; Replace "sbcl" with the path to your implementation
  (when (file-exists-p path-to-inferior-lisp-program)
    (setq inferior-lisp-program path-to-inferior-lisp-program))
  (when (fboundp 'slime)
    (add-hook 'slime-mode-hook
	      (lambda () (local-set-key (kbd "C-j") #'slime-eval-print-last-expression)))))

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
