
;; Emacs lisp 
(electric-indent-mode -1)
(add-hook 'emacs-lisp-mode-hook
	  (lambda () (local-set-key (kbd "C-c C-j") #'eval-print-last-sexp)))
(add-to-list 'auto-mode-alist '("\\.el\\.gz\\'" . emacs-lisp-mode))
;; (add-to-list 'auto-mode-alist '(".el\\.gz\\'" . emacs-lisp-mode))


;; Common lisp
(let ((path-to-slime-helper (dhnam/machine-config-get-first 'path-to-slime-helper))
      (path-to-inferior-lisp-program (dhnam/machine-config-get-first 'path-to-inferior-lisp-program)))
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

    (comment
     (defhydra paredit-forward-backward ()
       "paredit backward-down/forward-up"
       ("f" paredit-forward)
       ("b" paredit-backward))
     (define-key paredit-mode-map (kbd "C-M-f") #'paredit-forward-backward/paredit-forward)
     (define-key paredit-mode-map (kbd "C-M-b") #'paredit-forward-backward/paredit-backward))

    (comment
     (define-key global-map (kbd "M-F") #'paredit-forward)
     (define-key global-map (kbd "M-B") #'paredit-backward))

    (comment
     (if (and nil (package-installed-p 'hydra))
	 (progn
	   (defhydra paredit-slurp-barf-sexp ()
	     "paredit slurp/barf"
	     (")" paredit-forward-slurp-sexp)
	     ("(" paredit-backward-slurp-sexp)
	     ("}" paredit-forward-barf-sexp)
	     ("{" paredit-backward-barf-sexp))
	   (define-key paredit-mode-map (kbd "C-c )") #'paredit-slurp-barf-sexp/paredit-forward-slurp-sexp)
	   (define-key paredit-mode-map (kbd "C-c (") #'paredit-slurp-barf-sexp/paredit-backward-slurp-sexp)
	   (define-key paredit-mode-map (kbd "C-c }") #'paredit-slurp-barf-sexp/paredit-forward-barf-sexp)
	   (define-key paredit-mode-map (kbd "C-c {") #'paredit-slurp-barf-sexp/paredit-backward-barf-sexp))
       (progn
         (define-key paredit-mode-map (kbd "C-c )") (make-repeatable-command #'paredit-forward-slurp-sexp))
	 (define-key paredit-mode-map (kbd "C-c (") (make-repeatable-command #'paredit-backward-slurp-sexp))
	 (define-key paredit-mode-map (kbd "C-c }") (make-repeatable-command #'paredit-forward-barf-sexp))
	 (define-key paredit-mode-map (kbd "C-c {") (make-repeatable-command #'paredit-backward-barf-sexp)))))

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
      (defun copy-sexp-as-kill (&optional arg)
	"Save the sexp following point to the kill ring.
ARG has the same meaning as for `kill-sexp'."
	(interactive "p")
	(save-excursion
	  (let ((orig-point (point)))
	    (forward-sexp (or arg 1))
	    (if (eq last-command 'copy-and-forward-sexp)
		(kill-append (buffer-substring orig-point (point)) nil)
	      (kill-ring-save orig-point (point))))))

      (defun copy-and-forward-sexp (&optional arg)
	"Save the sexp following point to the kill ring.
ARG has the same meaning as for `kill-sexp'."
	(interactive "p")
	(if (eq last-command 'copy-and-forward-sexp)
	 (copy-sexp-as-kill))
	(forward-sexp)
	(setq last-command 'copy-and-forward-sexp))
      (comment (define-key paredit-mode-map (kbd "C-M-w") (make-repeatable-command #'copy-and-forward-sexp)))
      (key-chord-define paredit-mode-map "kk" (make-repeatable-command #'copy-and-forward-sexp)))
    (progn
      (define-key paredit-mode-map (kbd "C-M-w") #'paredit-kill-region))

    (if (and nil(package-installed-p 'hydra))
	(progn
	  (defhydra paredit-backward-down-forward-up ()
	    "paredit backward-down/forward-up"
	    ("n" paredit-backward-down)
	    ("p" paredit-forward-up))
	  (define-key paredit-mode-map (kbd "C-c p") #'paredit-backward-down-forward-up/paredit-backward-down)
	  (define-key paredit-mode-map (kbd "C-c n") #'paredit-backward-down-forward-up/paredit-forward-up))
      (progn
	(progn
	  (define-key paredit-mode-map (kbd "M-D") (make-repeatable-command #'paredit-backward-down))
	  (define-key paredit-mode-map (kbd "M-U") (make-repeatable-command #'paredit-forward-up)))
	(comment
	 (key-chord-define paredit-mode-map "DD" (make-repeatable-command #'paredit-backward-down))
	 (key-chord-define paredit-mode-map "UU" (make-repeatable-command #'paredit-forward-up)))))

    (progn
      (define-key paredit-mode-map (kbd "C-M-p") 'backward-list)
      (define-key paredit-mode-map (kbd "C-M-n") 'forward-list))
  
    (comment
     (comment
      (define-key global-map (kbd "C-M-f") 'paredit-forward)
      (define-key global-map (kbd "C-M-b") 'paredit-backward)
      (define-key global-map (kbd "C-M-d") 'paredit-forward-down)
      (define-key global-map (kbd "C-M-u") 'paredit-forward-up))

     (progn
       (comment (define-key global-map (kbd "C-M-P") 'paredit-backward-down))
       (comment (define-key global-map (kbd "C-M-U") 'paredit-backward-up))
       (define-key global-map (kbd "C-M-p") 'backward-list)
       (define-key global-map (kbd "C-M-n") 'forward-list))
     (define-key global-map (kbd "C-c p") #'paredit-backward-down-forward-up/paredit-backward-down)
     (define-key global-map (kbd "C-c n") #'paredit-backward-down-forward-up/paredit-forward-up)))

  (comment
   (when (fboundp 'highlight-map)
     ;; paredit overwrites M-s and M-S bindings
     (define-key paredit-mode-map (kbd "C-c h") 'highlight-map))))

(progn
  (comment
   (defhydra forward-backward-list ()
     "paredit backward-down/forward-up"
     ("f" forward-list)
     ("b" backward-list))
   (define-key global-map (kbd "C-M-f") #'forward-backward-list/forward-list)
   (define-key global-map (kbd "C-M-b") #'forward-backward-list/backard-list))
  (comment
    (define-key global-map (kbd "M-F") #'forward-sexp)
    (define-key global-map (kbd "M-B") #'backward-sexp)))

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
