
(require 'dhnam-python)

(use-existing-pkg pyvenv
  :init
  (progn
    (comment
      ;; directory-local
      ;; https://blog.allardhendriksen.nl/posts/tracking-project-virtual-environments-with-pyvenv-tracking-mode/
      ;; https://endlessparentheses.com/a-quick-guide-to-directory-local-variables.html
      ;;
      ;; - Move to the project root
      ;; - M-x add-dir-local-variable
      ;;   - python-mode
      ;;   - pyvenv-workon --> some-python-environment
      )
    (let ((pyvenv-workon-home-path (dhnam/machine-config-get-first 'pyvenv-workon-home-path)))
      (when pyvenv-workon-home-path
	    (setenv "WORKON_HOME" pyvenv-workon-home-path)
	    (pyvenv-mode 1)
	    (comment
	      ;; pyvenv-tracking-mode is very slow
	      (pyvenv-tracking-mode 1))))))

(if (package-installed-p 'highlight-indentation)
    (progn
      (add-hook 'python-mode-hook 'highlight-indentation-mode))
  (when (package-installed-p 'highlight-indent-guides)
    (custom-set-variables
     '(highlight-indent-guides-method 'column))
    (add-hook 'python-mode-hook 'highlight-indent-guides-mode)))

(with-eval-after-load 'python
  (define-key python-mode-map (kbd "C-c C-p") 'dhnam/run-python)
  (define-key python-mode-map (kbd "C-c C-c") 'dhnam/python-shell-send-region-or-buffer)
  (define-key python-mode-map (kbd "C-c C-f") 'dhnam/python-shell-send-from-package-import-all)
  (define-key python-mode-map (kbd "C-c M-w") 'dhnam/py-repl-kill-ring-save-without-empty-lines)
  (define-key python-mode-map (kbd "C-c M-W") 'dhnam/python-copy-code-from-docstring)
  (define-key python-mode-map (kbd "C-c p") 'dhnam/copy-full-module-name)
  (define-key python-mode-map (kbd "C-c P") 'dhnam/copy-statement-of-importing-all-from-full-module-name)
  (define-key python-mode-map (kbd "C-c I") 'dhnam/copy-statement-of-importing-symbol-at-point)
  (define-key python-mode-map (kbd "C-c f") 'dhnam/run-python-module-function)
  (define-key python-mode-map (kbd "C-c r") 'dhnam/run-python-dhnamlib-doctesting)
  (define-key python-mode-map (kbd "C-c c") 'dhnam/run-python-doc-code)
  (define-key python-mode-map (kbd "C-c C") 'dhnam/copy-python-doc-code-command)

  (define-key inferior-python-mode-map (kbd "C-c M-W") 'dhnam/python-copy-code-from-docstring)
  (define-key python-mode-map (kbd "C-c M-;") 'dhnam/python-toggle-doc-string-code))

(progn
  ;; `comment-dwim' update for python
  ;; https://stackoverflow.com/a/27073434
  ;;
  ;; At least two space character between code and #
  (add-hook 'python-mode-hook
            (lambda () (set (make-local-variable 'comment-inline-offset) 2))))

(when (fboundp 'sphinx-doc-mode)
  ;; python docstring
  ;; https://github.com/naiquevin/sphinx-doc.el
  ;;
  ;; The command is mapped to "C-c M-d"
  (add-hook 'python-mode-hook (lambda ()
				                (require 'sphinx-doc)
				                (sphinx-doc-mode t))))

(progn
  ;; pdb setup
  (custom-set-variables
   '(gud-pdb-command-name "python -m pdb")))

(progn
  (defun dhnam/kill-ring-save-pdb-breakpoint ()
    ;; https://stackoverflow.com/a/2178975
    "Copying a file path and a line number for pdb's breakpoint"

    (interactive)
    (kill-new (concat (buffer-file-name) ":" (number-to-string (line-number-at-pos)))))

  (add-hook 'python-mode-hook (lambda () (local-set-key (kbd "C-c C-x C-p") #'dhnam/kill-ring-save-pdb-breakpoint))))

;; prettify-symbols-mode
(comment
  (defun dhnam/init-python-prettify-symbols-alist ()
    "make some word or string show as pretty Unicode symbols"
    ;; http://ergoemacs.org/emacs/emacs_pretty_lambda.html

    (setq prettify-symbols-alist
	      `(
	        ("lambda" . 955)
	        )))
  (add-hook 'python-mode-hook 'prettify-symbols-mode)
  (add-hook 'python-mode-hook 'dhnam/init-python-prettify-symbols-alist)
  )

(progn
  ;; config for `dhnam/elpy-occur-definitions'
  ;;
  ;; extended regex
  ;; - "with block:" is added
  (defvar dhnam/python-definition-regex "^\s*\\(\\(\\(async\s\\|\\)def\\|class\\)\s\\)\\|with block:"))

(if (package-installed-p 'realgud)
    (progn
      (require 'dhnam-realgud)
      (progn
        ;; Disable the default gud pdb when a breakpoint is activated in a *Python* buffer.
        (add-hook 'inferior-python-mode-hook 'dhnam/unset-python-pdbtrack-setup-tracking)))
  (progn
    ;; pdbtrace
    ;; https://stackoverflow.com/questions/26285046/how-do-i-enable-pdbtrack-python-debugging-in-emacs-24-x-shell
    ;;
    ;; This trick doesn't work when the path to the source code includes parenthesis ("(" and ")"):
    ;; e.g. "(env) user@computer:~/path/to/some(source)dir$"
    (require 'python)


    (defun dhnam/pdbtrace-shell-mode-hook ()
      (add-hook 'comint-output-filter-functions 'python-pdbtrack-comint-output-filter-function nil t))

    (defun dhnam/enable-pdbtrace-shell-mode ()
      (interactive)
      (add-hook 'shell-mode-hook 'dhnam/pdbtrace-shell-mode-hook))

    (comment
      (defun dhnam/disable-pdbtrace-shell-mode ()
        (interactive)
        (remove-hook 'shell-mode-hook 'dhnam/pdbtrace-shell-mode-hook t)))

    (dhnam/enable-pdbtrace-shell-mode)))

(comment
  (when (package-installed-p 'conda)
    (require 'conda)
    ;; if you want interactive shell support, include:
    (conda-env-initialize-interactive-shells)
    ;; if you want eshell support, include:
    (conda-env-initialize-eshell)
    ;; if you want auto-activation (see below for details), include:
    (conda-env-autoactivate-mode t)))

(comment
  (require 'dhnam-make-repeatable-command)
  (define-key python-mode-map (kbd "C-c >") (make-repeatable-command 'python-indent-shift-right))
  (define-key python-mode-map (kbd "C-c <") (make-repeatable-command 'python-indent-shift-left)))

(progn
  (define-key python-mode-map (kbd "C-c <") 'dhnam/python-indent/python-indent-shift-left)
  (define-key python-mode-map (kbd "C-c >") 'dhnam/python-indent/python-indent-shift-right))

(when (package-installed-p 'yaml-mode)
  ;; https://github.com/yoshiki/yaml-mode
  (require 'yaml-mode)
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(when (dhnam/machine-config-get-first 'python-doc-code-temp-file-format)
  (setq dhnam/python-doc-code-temp-file-format
        (dhnam/machine-config-get-first 'python-doc-code-temp-file-format)))

(when (fboundp 'ein:run)
  ;; EIN: Emacs IPython Notebook
  ;; https://github.com/millejoh/emacs-ipython-notebook

  (comment
	;; run IPython notebook server with specific port number
	;; https://github.com/tkf/emacs-ipython-notebook/issues/109#issuecomment-16874676
	;;
	;; $ ipython notebook --port 9999
	;;
	;; then login EIN --> M-x ein:login RET 9999 RET
	)

  (progn
	(custom-set-variables
	 '(ein:polymode t)  ; enable other modes such as Elpy
	 ;; '(ein:cell-input-area ((t (:background "black"))))
	 '(ein:cell-input-area ((t (:background "gray10"))))
	 )
    (custom-set-variables
     '(ein:jupyter-default-kernel 'python3))

	(add-hook 'ein:notebook-mode-hook 'display-line-numbers-mode)
	(setq ein:worksheet-enable-undo t)))

(comment
  (when (package-installed-p 'anaconda-mode)
    (comment
      (add-hook 'python-mode-hook 'anaconda-mode)
      (add-hook 'python-mode-hook 'anaconda-eldoc-mode)

      (when (package-installed-p 'company-anaconda)
        (eval-after-load 'company
          '(add-to-list 'company-backends 'company-anaconda))))

    (progn
      ;; replace `xref-find-definitions' with `anaconda-mode-find-definitions'
      (autoload 'anaconda-mode-find-definitions "anaconda-mode" "Find definitions for thing at point." t nil)
      ;; (add-hook 'python-mode-hook (lambda () (require 'anaconda-mode)))
      (define-key python-mode-map (kbd "M-.") 'anaconda-mode-find-definitions))))

(provide 'init-python)
