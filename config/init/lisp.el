
;; Emacs lisp 
(electric-indent-mode -1)
(add-hook 'emacs-lisp-mode-hook
	  (lambda () (local-set-key (kbd "C-c C-j") #'eval-print-last-sexp)))
(add-to-list 'auto-mode-alist '("\\.el\\.gz\\'" . emacs-lisp-mode))
;; (add-to-list 'auto-mode-alist '(".el\\.gz\\'" . emacs-lisp-mode))


;; Common lisp
(let ((path-to-slime-helper (machine-config-get-first 'path-to-slime-helper))
      (path-to-inferior-lisp-program (machine-config-get-first 'path-to-inferior-lisp-program)))
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

(when (functionp 'paredit-mode)
  ;; ParEdit
  ;; http://wikemacs.org/wiki/Paredit-mode
  ;; http://danmidwood.com/content/2014/11/21/animated-paredit.html

  (require 'paredit)

  (autoload 'enable-paredit-mode "paredit"
    "Turn on pseudo-structural editing of Lisp code."
    t)
  (progn
    (add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)
    (add-hook 'lisp-mode-hook             'enable-paredit-mode)
    (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
    (add-hook 'scheme-mode-hook           'enable-paredit-mode)
    (add-hook 'hy-mode-hook               'enable-paredit-mode))
  
  (progn
    (define-key paredit-mode-map (kbd "C-M-p") 'backward-list)
    (define-key paredit-mode-map (kbd "C-M-n") 'forward-list)

    (progn
      (defhydra paredit-slurp-barf-sexp ()
	"paredit slurp/barf"
	(")" paredit-forward-slurp-sexp)
	("(" paredit-backward-slurp-sexp)
	("}" paredit-forward-barf-sexp)
	("{" paredit-backward-barf-sexp)
	("0" paredit-forward-barf-sexp)
	("9" paredit-backward-barf-sexp))

      (key-chord-define paredit-mode-map "()" #'paredit-slurp-barf-sexp/body)
      (key-chord-define paredit-mode-map "{}" #'paredit-slurp-barf-sexp/body))

    (progn
      (defun copy-and-forward-sexp (&optional arg)
	"Save the sexp following point to the kill ring.
ARG has the same meaning as for `kill-sexp'."
	(interactive "p")
	(save-excursion
	  (let ((orig-point (point)))
	    (forward-sexp (or arg 1))
	    (if (eq last-command 'copy-and-forward-sexp)
		(kill-append (buffer-substring orig-point (point)) nil)
	      (kill-ring-save orig-point (point)))))
	(forward-sexp arg)
	(setq last-command 'copy-and-forward-sexp))

      (key-chord-define paredit-mode-map "kk" (make-repeatable-command #'copy-and-forward-sexp)))

    (progn
      (define-key paredit-mode-map (kbd "C-M-w") #'paredit-kill-region))

    (progn
      (define-key paredit-mode-map (kbd "M-D") (make-repeatable-command #'paredit-backward-down))
      (define-key paredit-mode-map (kbd "M-U") (make-repeatable-command #'paredit-forward-up)))

    (progn
      (define-key paredit-mode-map (kbd "C-M-p") 'backward-list)
      (define-key paredit-mode-map (kbd "C-M-n") 'forward-list)))

  (comment
   (when (fboundp 'highlight-map)
     ;; paredit overwrites M-s and M-S bindings
     (define-key paredit-mode-map (kbd "C-c h") 'highlight-map))))

(comment
 (when (package-installed-p 'highlight-parentheses)
   (require 'highlight-parentheses)

   (define-globalized-minor-mode global-highlight-parentheses-mode highlight-parentheses-mode
     (lambda nil (highlight-parentheses-mode t)))

   (global-highlight-parentheses-mode t)))

(comment
 (when (fboundp 'show-paren-mode)
   (setq show-paren-delay 0)
   (add-hook 'paredit-mode-hook 'show-paren-mode)))

(progn
  ;; hissp & lissp
  (add-to-list 'auto-mode-alist '("\\.lissp\\'" . lisp-mode)))
