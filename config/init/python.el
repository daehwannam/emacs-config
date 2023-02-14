
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

(defun dhnam/py-repl-kill-ring-save-without-empty-lines (beg end &optional region)
  (interactive (list (mark) (point)
		             (prefix-numeric-value current-prefix-arg)))
  ;; https://stackoverflow.com/questions/605846/how-do-i-access-the-contents-of-the-current-region-in-emacs-lisp
  ;; https://stackoverflow.com/questions/6236196/elisp-split-string-function-to-split-a-string-by-character
  (let (lines char-lists indent-sizes min-indent result)
    (setq lines (nbutlast (split-string (buffer-substring-no-properties beg end) "$") 1)) ; https://stackoverflow.com/a/605931
    (setf (car lines) (concat " " (car lines)))
    (setq lines (mapcar (lambda (x) (substring x 1)) lines))
    (setq lines (seq-filter (lambda (x) (not (string-empty-p (string-trim x)))) lines))
    (setq char-lists nil)
    (dolist (line lines)
      (setq char-lists (cons (mapcar (lambda (x) (char-to-string x)) line) char-lists)))
    (setq char-lists (reverse char-lists))
    (setq indent-sizes (mapcar
			            (lambda (x) (let ((size 0) (li x))
				                      (while (string= (car x) " ")
					                    (setq size (+ 1 size))
					                    (setq x (cdr x)))
				                      size))
			            char-lists))
    (setq min-indent (seq-min indent-sizes))
    (setq result "")
    (dolist (line lines)
      (setq result (concat result (substring line min-indent) "\n")))
    (kill-new result)
    (setq deactivate-mark t)))

(with-eval-after-load 'python
  (define-key python-mode-map (kbd "C-c M-w") 'dhnam/py-repl-kill-ring-save-without-empty-lines))

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
  ;; command copying file path and line number for pdb's breakpoint
  (defun dhnam/kill-ring-save-pdb-breakpoint ()
    ;; https://stackoverflow.com/a/2178975
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

  (dhnam/enable-pdbtrace-shell-mode))

(progn
  (defun dhnam/convert-path-to-package ()
    ""
    ;; https://stackoverflow.com/a/25886353
    (interactive)
    (save-excursion
      (let* ((bounds (bounds-of-thing-at-point 'symbol))
             (start (car bounds))
             (end (cdr bounds)))
	    (if (use-region-p)
	        (replace-regexp "/" "." nil (region-beginning) (region-end))
	      (replace-regexp "/" "." nil start end)))))

  (defun dhnam/convert-package-to-path ()
    ""
    ;; https://stackoverflow.com/a/25886353
    (interactive)
    (save-excursion
      (let* ((bounds (bounds-of-thing-at-point 'symbol))
             (start (car bounds))
             (end (cdr bounds)))
	    (if (use-region-p)
	        (replace-regexp "." "/" nil (region-beginning) (region-end))
	      (replace-regexp "." "/" nil start end))))))

(comment
  (when (package-installed-p 'conda)
    (require 'conda)
    ;; if you want interactive shell support, include:
    (conda-env-initialize-interactive-shells)
    ;; if you want eshell support, include:
    (conda-env-initialize-eshell)
    ;; if you want auto-activation (see below for details), include:
    (conda-env-autoactivate-mode t)))

(progn
  (require 'dhnam-make-repeatable-command)
  (define-key python-mode-map (kbd "C-c >") (make-repeatable-command 'python-indent-shift-right))
  (define-key python-mode-map (kbd "C-c <") (make-repeatable-command 'python-indent-shift-left)))

(when (package-installed-p 'yaml-mode)
  ;; https://github.com/yoshiki/yaml-mode
  (require 'yaml-mode)
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))


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
	 '(ein:polymode t)		; enable other modes such as Elpy
	 ;; '(ein:cell-input-area ((t (:background "black"))))
	 '(ein:cell-input-area ((t (:background "gray10"))))
	 )
    (custom-set-variables
     '(ein:jupyter-default-kernel 'python3))

	(add-hook 'ein:notebook-mode-hook 'display-line-numbers-mode)
	(setq ein:worksheet-enable-undo t)))

(defun dhnam/insert-ipdb-config-example ()
  (interactive)
  (insert (dhnam/get-string-from-file "~/.emacs.d/config/init/dependent/ipdb-config-example.sh")))

(provide 'dhnam-python)
