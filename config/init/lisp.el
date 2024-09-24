
(require 'dhnam-make-repeatable-command)

(progn
  ;; Emacs lisp 
  (electric-indent-mode -1)
  (add-hook 'emacs-lisp-mode-hook
	        (lambda () (local-set-key (kbd "C-c C-j") #'eval-print-last-sexp)))
  (add-to-list 'auto-mode-alist '("\\.el\\.gz\\'" . emacs-lisp-mode))
  (comment (add-to-list 'auto-mode-alist '(".el\\.gz\\'" . emacs-lisp-mode)))

  (comment
    ;; replace emacs-lisp's indentation with common-lisp's
    ;; https://stackoverflow.com/a/22167050
    (setq lisp-indent-function 'common-lisp-indent-function)

    (require 'cl-indent)
    (progn
      ;; https://emacs.stackexchange.com/a/13202
      (put 'comment 'common-lisp-indent-function
           (get 'progn 'common-lisp-indent-function))
      (put 'cl-flet 'common-lisp-indent-function
           (get 'flet 'common-lisp-indent-function))
      (put 'cl-labels 'common-lisp-indent-function
           (get 'labels 'common-lisp-indent-function))
      (progn
        ;; use emacs's default `if' indentation
        (put 'if 'common-lisp-indent-function 2))))

  (progn
    ;; Lisp indentation fix for plist
    ;; https://emacs.stackexchange.com/a/34757
    (require 'dhnam-lisp-indentation-fix))

  (progn
    (comment
      ;; replace emacs-lisp's indentation with common-lisp's
      ;; https://stackoverflow.com/a/22167050
      (setq lisp-indent-function 'common-lisp-indent-function))
    (progn
      ;; fix emacs-lisp's indentation as common-lisp's
      ;; https://emacs.stackexchange.com/a/13202
      (require 'cl-indent)
      (put 'comment 'lisp-indent-function
           (get 'progn 'common-lisp-indent-function))
      (put 'defun-override 'lisp-indent-function
           (get 'defun 'lisp-indent-function))
      (comment
        (put 'cl-flet 'lisp-indent-function
             (get 'flet 'common-lisp-indent-function))
        (put 'cl-labels 'lisp-indent-function
             (get 'labels 'common-lisp-indent-function))))
    (progn
      ;; common-lisp's indentation
      (require 'cl-indent)
      (put 'comment 'common-lisp-indent-function
           (get 'progn 'common-lisp-indent-function))))

  (with-eval-after-load 'edebug
    (define-key edebug-mode-map (kbd "C-q C-e") 'edebug-eval-last-sexp)))

(progn
  ;; Common lisp
  (add-to-list 'auto-mode-alist '("\\.lisp\\'" . lisp-mode))

  (progn
    ;; SLIME

    (defvar using-manual-slime-helper nil)
    (when using-manual-slime-helper
      (let ((path-to-slime-helper (dhnam/machine-config-get-first 'path-to-slime-helper)))
        (when path-to-slime-helper
          (load (expand-file-name path-to-slime-helper)))))

    (let ((path-to-inferior-lisp-program (dhnam/machine-config-get-first 'path-to-inferior-lisp-program)))
      (when path-to-inferior-lisp-program
        ;; replace "sbcl" with the path to your implementation
        (setq inferior-lisp-program path-to-inferior-lisp-program)))

    (when (fboundp 'slime)
      (progn
        ;; enable slime features
        ;; https://lispcookbook.github.io/cl-cookbook/editor-support.html#installing-slime
        (slime-setup '(slime-fancy slime-quicklisp slime-asdf)))

      (progn
        ;; ivy-mode for slime
        ;; https://www.reddit.com/r/emacs/comments/dkz7tm/comment/f4nf696/?utm_source=share&utm_medium=web2x&context=3
        (defun ora-slime-completion-in-region (_fn completions start end)
          (let ((completion-in-region-function #'ivy-completion-in-region))
            (funcall completion-in-region-function start end completions)))

        (advice-add 'slime-display-or-scroll-completions
                    :around #'ora-slime-completion-in-region))

      (progn
        ;; Common lisp hyperspec
        (progn
          ;; Opening hyperspec document in eww
          ;; https://www.reddit.com/r/lisp/comments/oo37mr/comment/h5vosl2/?utm_source=share&utm_medium=web2x&context=3
          ;;
          ;; related commands
          ;; - common-lisp-hyperspec
          ;; - slime-documentation-lookup (C-c C-d h)
          (setq browse-url-browser-function
	            '(("hyperspec" . eww-browse-url)
	              ("." . browse-url-default-browser))))

        (progn
          ;; CLHS offline
          ;; https://lispcookbook.github.io/cl-cookbook/emacs-ide.html#consult-the-clhs-offline
          ;;
          ;; Run the following code in slime REPL to install CLHS
          ;; > (ql:quickload "clhs")
          ;; > (clhs:print-emacs-setup-form)
          ;; > (clhs:install-clhs-use-local)
          ;;
          ;; Run the below command again to find the path of CLHS
          ;; > (clhs:print-emacs-setup-form)
          (let ((clhs-use-local-path "~/quicklisp/clhs-use-local.el"))
            (when (file-exists-p clhs-use-local-path)
              (load clhs-use-local-path)))))

      (progn
        ;; Bindings

        (defun dhnam/slime-define-keys (map)
          (define-key map (kbd "C-q C-e") #'slime-eval-last-expression)
          (comment (define-key map (kbd "C-q C-j") #'slime-eval-last-expression-in-repl)) ; it's already mapped
          (comment (define-key map (kbd "C-q J") #'slime-eval-print-last-expression))
          (define-key map (kbd "C-c C-d H") #'slime-documentation)
          (define-key map (kbd "C-c H") #'slime-hyperspec-lookup))

        (with-eval-after-load 'slime
          (dhnam/slime-define-keys slime-mode-map))
        (with-eval-after-load 'slime-repl
          (dhnam/slime-define-keys slime-repl-mode-map))))))

(progn
  ;; hissp & lissp
  (add-to-list 'auto-mode-alist '("\\.lissp\\'" . lisp-mode))

  (defun dhnam/insert-lissp-prelude ()
    (interactive)
    (insert "(hissp.basic.._macro_.prelude)")))

(progn
  ;; Parenthesis-related modes for global activations
  (progn
    ;; `show-paren-mode' as a local-mode
    ;; https://stackoverflow.com/a/10268394
    (defun show-paren-local-mode ()
      (interactive)
      (make-local-variable 'show-paren-mode) ;; The value of shom-paren-mode will be local to this buffer.
      (setq show-paren-mode t))

    (setq show-paren-delay 0)

    (comment
      (add-hook 'prog-mode-hook 'show-paren-local-mode)))
  
  (comment
    (when (package-installed-p 'highlight-parentheses)
      (require 'highlight-parentheses)

      (define-globalized-minor-mode global-highlight-parentheses-mode highlight-parentheses-mode
        (lambda nil (highlight-parentheses-mode t)))

      (global-highlight-parentheses-mode t))))

(progn
  ;; global keys
  (global-set-key (kbd "M-P") 'backward-list)
  (global-set-key (kbd "M-N") 'forward-list)

  (global-set-key (kbd "<C-M-backspace>") 'backward-kill-sexp))

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
    (add-hook 'hy-mode-hook               'enable-paredit-mode)
    (add-hook 'slime-repl-mode-hook       'enable-paredit-mode))
  
  (progn
    (define-key paredit-mode-map (kbd "C-M-p") 'backward-list)
    (define-key paredit-mode-map (kbd "C-M-n") 'forward-list)

    (define-key paredit-mode-map (kbd "M-r") 'move-to-window-line-top-bottom)
    (define-key paredit-mode-map (kbd "M-a") 'paredit-raise-sexp)

    (define-key paredit-mode-map (kbd "{") 'paredit-open-curly)
    (define-key paredit-mode-map (kbd "M-)") 'paredit-split-sexp)
    (comment
      ;; when "M-[" is bound to a command, paste "some-text" in terminal emacs insert "[200~some-text [201~]]"
      ;; https://emacs.stackexchange.com/a/28864
      (define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-square)
      (define-key paredit-mode-map (kbd "M-]") 'paredit-split-sexp))
    (define-key paredit-mode-map (kbd "M-{") 'paredit-wrap-curly)
    (define-key paredit-mode-map (kbd "M-}") 'paredit-split-sexp)

    (progn
      (defhydra paredit-slurp-barf-sexp ()
	    "paredit slurp/barf"
	    (")" paredit-forward-slurp-sexp)
	    ("(" paredit-backward-slurp-sexp)
	    ("}" paredit-forward-barf-sexp)
	    ("{" paredit-backward-barf-sexp)
	    ("0" paredit-forward-barf-sexp)
	    ("9" paredit-backward-barf-sexp))

      ;; disable any hint message
      (hydra-set-property 'paredit-slurp-barf-sexp :verbosity 0)

      (key-chord-define paredit-mode-map "()" #'paredit-slurp-barf-sexp/body)
      (key-chord-define paredit-mode-map "{}" #'paredit-slurp-barf-sexp/body))

    (progn
      (define-key paredit-mode-map (kbd "C-w") #'paredit-kill-region)
      (define-key paredit-mode-map (kbd "C-M-w") #'paredit-kill-region))

    (progn
      (require 'dhnam-elisp)
      (require 'dhnam-paredit)

      (comment (key-chord-define paredit-mode-map "kk" (make-repeatable-command #'dhnam/copy-and-forward-sexp)))
      (comment (define-key paredit-mode-map (kbd "M-k") 'dhnam/copy-and-forward-sexp))
      (define-key paredit-mode-map (kbd "M-k") 'dhnam/paredit-copy)
      (define-key paredit-mode-map (kbd "M-w") #'dhnam/paredit-kill-ring-save)
      (define-key paredit-mode-map (kbd "M-;") #'dhnam/paredit-comment-dwim)
      (comment
        (define-key paredit-mode-map (kbd "C-M-u") #'dhnam/paredit-backward-up-or-down)
        (define-key paredit-mode-map (kbd "C-M-d") #'dhnam/paredit-forward-up-or-down))
      (define-key paredit-mode-map (kbd "DEL") #'dhnam/paredit-backward-delete)

      (comment
        (define-key paredit-mode-map (kbd dhnam/xcape-left-alt) 'dhnam-paredit-iokl/body)))

    (progn
      (define-key paredit-mode-map (kbd "M-D") (make-repeatable-command #'paredit-backward-down))
      (define-key paredit-mode-map (kbd "M-U") (make-repeatable-command #'paredit-forward-up)))

    (progn
      (define-key paredit-mode-map (kbd "C-M-p") 'backward-list)
      (define-key paredit-mode-map (kbd "C-M-n") 'forward-list))

    (progn
      (define-key paredit-mode-map (kbd "C-M-b") 'backward-sexp)
      (define-key paredit-mode-map (kbd "C-M-f") 'forward-sexp))

    (comment
      ;; instead of `backward-sexp' and `forward-sexp'
      (define-key global-map (kbd "C-M-b") 'paredit-backward)
      (define-key global-map (kbd "C-M-f") 'paredit-forward)))

  (comment
    (when (fboundp 'dhnam/highlight-map)
      ;; paredit overwrites M-s and M-S bindings
      (define-key paredit-mode-map (kbd "C-c h") 'dhnam/highlight-map))))

(with-eval-after-load 'edebug-mode-map
  (require 'dhnam-elisp)

  (define-key edebug-mode-map (kbd "e") #'dhnam/edebug-eval-at-point)
  (define-key edebug-mode-map (kbd "E") #'edebug-eval-expression))

(provide 'init-lisp)
