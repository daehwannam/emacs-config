
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

                                        ;---;;; python-mode
                                        ;---;; https://www.emacswiki.org/emacs/ProgrammingWithPythonModeDotEl
                                        ;---;; https://gitlab.com/python-mode-devs/python-mode
                                        ;---(autoload 'python-mode "python-mode" "Python Mode." t)
                                        ;---(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
                                        ;---add-to-list 'interpreter-mode-alist '("python" . python-mode))

;;; python-mode linum-mode setting
(comment (add-hook 'python-mode-hook 'linum-mode))

(if (package-installed-p 'highlight-indentation)
    (progn
      (add-hook 'python-mode-hook 'highlight-indentation-mode))
  (when (package-installed-p 'highlight-indent-guides)
    (custom-set-variables
     '(highlight-indent-guides-method 'column))
    (add-hook 'python-mode-hook 'highlight-indent-guides-mode)))


(progn
  ;; use C-j as newline-and-indent
  (defun dhnam/non-electric-indent-mode ()
    (electric-indent-mode -1))
  ;; (add-hook 'python-mode-hook 'dhnam/non-electric-indent-mode) ; https://github.com/jorgenschaefer/elpy/issues/195
  )


;;; python 3 setting
;; https://askubuntu.com/questions/460668/how-to-use-python3-in-emacs-on-ubuntu-14-04
;; (setq python-shell-interpreter "python3")  ;; it occurs error for virtual environment of python2


;;; ipython
;; https://stackoverflow.com/a/17817119
;; (defun dhnam/run-ipython ()
;;   (interactive)
;;   (term "ipython")
;;   (rename-buffer "*IPython*"))
(comment
  (defun dhnam/run-ipython ()
    (interactive)
;;; count windows: https://emacs.stackexchange.com/questions/3494/how-to-count-all-of-the-windows-in-a-frame
    (if (< (length (mapcar #'window-buffer (window-list))) 2)
        (split-window-right))
    (other-window 1)
    (named-term "ipython" "*IPython*")))

;; (when (executable-find "ipython")
;;   (setq python-shell-interpreter "ipython"))

;; (cond
;;  ((string-equal machine-domain "vbox") ; Microsoft Windows
;;   (progn
;;     (setenv "WORKON_HOME" "~/bin/anaconda2/envs/")
;;     (pyvenv-mode 1)))
;;  ((string-equal machine-domain "engels") ; vbox linux
;;   (progn
;;     (setenv "WORKON_HOME" "~/bin/anaconda3/envs/")
;;     (pyvenv-mode 1)))
;; )

(defun dhnam/how-many-str (regexp str)
  (loop with start = 0
        for count from 0
        while (string-match regexp str start)
        do (setq start (match-end 0))
        finally return count))

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

(progn
  (require 'python)  ; to access python-mode-map
  (define-key python-mode-map (kbd "C-c M-w") 'dhnam/py-repl-kill-ring-save-without-empty-lines))

;; python docstring
;; https://github.com/naiquevin/sphinx-doc.el
;; 
;; The command is mapped to "C-c M-d"
(when (fboundp 'sphinx-doc-mode)
  (add-hook 'python-mode-hook (lambda ()
				                (require 'sphinx-doc)
				                (sphinx-doc-mode t))))

;; Hy lang setup
(when (fboundp 'hy-mode)
  (add-to-list 'auto-mode-alist '("\\.hy\\'" . hy-mode))
  ;; (add-hook 'hy-mode-hook #'run-jedhy)
  ;; (require 'hy-mode)
  ;; (run-jedhy)
  (condition-case nil
      ;; first call of 'run-jedhy raises an error, so the error is processed first here.
      ;; afterwards, it raises not errors
      (run-jedhy)
    (error nil))
  (setq hy-shell--interpreter-args '("--spy" "--repl-output-fn" "hy.contrib.hy-repr.hy-repr"))  ; simplified representation
  ;; (setq hy-shell--interpreter-args '("--spy"))  ; default arguments
  )

;; pdb setup
(custom-set-variables
 '(gud-pdb-command-name "python -m pdb"))

;; command copying file path and line number for pdb's breakpoint
(defun dhnam/kill-ring-save-pdb-breakpoint ()
  ;; https://stackoverflow.com/a/2178975
  (interactive)
  (kill-new (concat (buffer-file-name) ":" (number-to-string (line-number-at-pos)))))

(add-hook 'python-mode-hook (lambda () (local-set-key (kbd "C-c C-x C-p") #'dhnam/kill-ring-save-pdb-breakpoint)))
(add-hook 'hy-mode-hook (lambda () (local-set-key (kbd "C-c C-x C-p") #'dhnam/kill-ring-save-pdb-breakpoint)))

;; pdb setup for Hy
(setq pdb-hy-input-frame-left-side "import hy; from hy.contrib.hy_repr import hy_repr; print(hy_repr(hy.eval(hy.read_str(\"\"\"")
(setq pdb-hy-input-frame-right-side "\"\"\"))))")

(comment
  ;; not-used
  (defun dhnam/insert-py-code-to-hy-output-frame ()
    (interactive)
    (setq back-move-len (length "\"))))"))
    (insert pdb-hy-input-frame-left-side)
    (insert pdb-hy-input-frame-right-side)
    (backward-char back-move-len))

  (add-hook 'pdb-mode-hook
	        (lambda () (local-set-key (kbd "C-c C-j") #'dhnam/insert-py-code-to-hy-output-frame))))

(comment
  (defun dhnam/hy-pdb-send-input ()
    (interactive)
    (move-beginning-of-line 1)
    (insert pdb-hy-input-frame-left-side)
    (move-end-of-line 1)
    (insert pdb-hy-input-frame-right-side)
    (comint-send-input))

  (add-hook 'pdb-mode-hook
	        (lambda () (local-set-key (kbd "M-j") #'dhnam/hy-pdb-send-input))))

(progn
  (defun dhnam/comint-send-input-for-hy (&optional no-newline artificial)
    "comint-send-input for Hy

Below is the original docstring:

  Send input to process.
After the process output mark, sends all text from the process mark to
point as input to the process.  Before the process output mark, calls
value of variable `comint-get-old-input' to retrieve old input, copies
it to the process mark, and sends it.

This command also sends and inserts a final newline, unless
NO-NEWLINE is non-nil.

Any history reference may be expanded depending on the value of the variable
`comint-input-autoexpand'.  The list of function names contained in the value
of `comint-input-filter-functions' is called on the input before sending it.
The input is entered into the input history ring, if the value of variable
`comint-input-filter' returns non-nil when called on the input.

If variable `comint-eol-on-send' is non-nil, then point is moved to the
end of line before sending the input.

After the input has been sent, if `comint-process-echoes' is non-nil,
then `comint-send-input' waits to see if the process outputs a string
matching the input, and if so, deletes that part of the output.
If ARTIFICIAL is non-nil, it inhibits such deletion.
Callers sending input not from the user should use ARTIFICIAL = t.

The values of `comint-get-old-input', `comint-input-filter-functions', and
`comint-input-filter' are chosen according to the command interpreter running
in the buffer.  E.g.,

If the interpreter is the csh,
    `comint-get-old-input' is the default:
	If `comint-use-prompt-regexp' is nil, then
	either return the current input field, if point is on an input
	field, or the current line, if point is on an output field.
	If `comint-use-prompt-regexp' is non-nil, then
	return the current line with any initial string matching the
	regexp `comint-prompt-regexp' removed.
    `comint-input-filter-functions' monitors input for \"cd\", \"pushd\", and
	\"popd\" commands.  When it sees one, it cd's the buffer.
    `comint-input-filter' is the default: returns t if the input isn't all white
	space.

If the Comint is Lucid Common Lisp,
    `comint-get-old-input' snarfs the sexp ending at point.
    `comint-input-filter-functions' does nothing.
    `comint-input-filter' returns nil if the input matches input-filter-regexp,
	which matches (1) all whitespace (2) :a, :c, etc.

Similarly for Soar, Scheme, etc."
    (interactive)
    ;; If we're currently completing, stop.  We're definitely done
    ;; completing, and by sending the input, we might cause side effects
    ;; that will confuse the code running in the completion
    ;; post-command-hook.
    (when completion-in-region-mode
      (completion-in-region-mode -1))
    ;; Note that the input string does not include its terminal newline.
    (let ((proc (get-buffer-process (current-buffer))))
      (if (not proc) (user-error "Current buffer has no process")
	    (widen)
	    (let* ((pmark (process-mark proc))
	           (intxt (if (>= (point) (marker-position pmark))
			              (progn (if comint-eol-on-send
				                     (if comint-use-prompt-regexp
					                     (end-of-line)
				                       (goto-char (field-end))))
				                 (buffer-substring pmark (point)))
			            (let ((copy (funcall comint-get-old-input)))
			              (goto-char pmark)
			              (insert copy)
			              copy)))
	           (input (if (not (eq comint-input-autoexpand 'input))
			              ;; Just whatever's already there.
			              intxt
			            ;; Expand and leave it visible in buffer.
			            (comint-replace-by-expanded-history t pmark)
			            (buffer-substring pmark (point))))
	           (history (if (not (eq comint-input-autoexpand 'history))
			                input
			              ;; This is messy 'cos ultimately the original
			              ;; functions used do insertion, rather than return
			              ;; strings.  We have to expand, then insert back.
			              (comint-replace-by-expanded-history t pmark)
			              (let ((copy (buffer-substring pmark (point)))
				                (start (point)))
			                (insert input)
			                (delete-region pmark start)
			                copy))))

	      (unless no-newline
	        (insert ?\n))

	      (comint-add-to-input-history history)

	      (run-hook-with-args 'comint-input-filter-functions
			                  (if no-newline input
				                (concat input "\n")))

	      (let ((beg (marker-position pmark))
		        (end (if no-newline (point) (1- (point)))))
	        (with-silent-modifications
	          (when (> end beg)
		        (add-text-properties beg end
				                     '(front-sticky t
						                            font-lock-face comint-highlight-input))
		        (unless comint-use-prompt-regexp
		          ;; Give old user input a field property of `input', to
		          ;; distinguish it from both process output and unsent
		          ;; input.  The terminating newline is put into a special
		          ;; `boundary' field to make cursor movement between input
		          ;; and output fields smoother.
		          (add-text-properties
		           beg end
		           '(mouse-face highlight
				                help-echo "mouse-2: insert after prompt as new input"))))
	          (unless (or no-newline comint-use-prompt-regexp)
		        ;; Cover the terminating newline
		        (add-text-properties end (1+ end)
				                     '(rear-nonsticky t
						                              field boundary
						                              inhibit-line-move-field-capture t)))))

	      (comint-snapshot-last-prompt)

	      (setq comint-save-input-ring-index comint-input-ring-index)
	      (setq comint-input-ring-index nil)
	      ;; Update the markers before we send the input
	      ;; in case we get output amidst sending the input.
	      (set-marker comint-last-input-start pmark)
	      (set-marker comint-last-input-end (point))
	      (set-marker (process-mark proc) (point))
	      ;; clear the "accumulation" marker
	      (set-marker comint-accum-marker nil)
	      (let ((comint-input-sender-no-newline no-newline))
	        (funcall comint-input-sender proc (concat pdb-hy-input-frame-left-side
						                              input
						                              pdb-hy-input-frame-right-side)))  ; modified by dhnam

	      ;; Optionally delete echoed input (after checking it).
	      (when (and comint-process-echoes (not artificial))
	        (let ((echo-len (- comint-last-input-end
			                   comint-last-input-start)))
	          ;; Wait for all input to be echoed:
	          (while (and (> (+ comint-last-input-end echo-len)
			                 (point-max))
			              (accept-process-output proc)
			              (zerop
			               (compare-buffer-substrings
			                nil comint-last-input-start
			                (- (point-max) echo-len)
			                ;; Above difference is equivalent to
			                ;; (+ comint-last-input-start
			                ;;    (- (point-max) comint-last-input-end))
			                nil comint-last-input-end (point-max)))))
	          (if (and
		           (<= (+ comint-last-input-end echo-len)
		               (point-max))
		           (zerop
		            (compare-buffer-substrings
		             nil comint-last-input-start comint-last-input-end
		             nil comint-last-input-end
		             (+ comint-last-input-end echo-len))))
		          ;; Certain parts of the text to be deleted may have
		          ;; been mistaken for prompts.  We have to prevent
		          ;; problems when `comint-prompt-read-only' is non-nil.
		          (let ((inhibit-read-only t))
		            (delete-region comint-last-input-end
				                   (+ comint-last-input-end echo-len))
		            (when comint-prompt-read-only
		              (save-excursion
			            (goto-char comint-last-input-end)
			            (comint-update-fence)))))))

	      ;; This used to call comint-output-filter-functions,
	      ;; but that scrolled the buffer in undesirable ways.
	      (run-hook-with-args 'comint-output-filter-functions "")))))

  (add-hook 'pdb-mode-hook
	        (lambda () (local-set-key (kbd "M-j") #'dhnam/comint-send-input-for-hy))))

(progn
  ;; dhnam/hy-shell-set-project-root
  (defun dhnam/hy-get-project-root-change-expr-string (new-root)
    (format "(do (import sys) (sys.path.insert 0 \"%s\"))" (file-truename new-root)))

  (defun dhnam/hy-shell-set-project-root (new-root)
    ;; (interactive "DNew project root: ")
    (interactive (list (read-directory-name "New project root: " (dhnam/get-python-default-project-root))))
    (hy-shell--eval-1 (dhnam/hy-get-project-root-change-expr-string new-root)))

  (add-hook 'hy-mode-hook
	        (lambda () (local-set-key (kbd "C-c Z") 'dhnam/hy-shell-set-project-root)))

  (defun dhnam/get-python-default-project-root ()
    (expand-file-name (locate-dominating-file default-directory ".src")))

  (comment
    (defun dhnam/run-hy-with-default-project-root ()
      "modified version of 'run-hy"
      (interactive)

      (hy-shell--with
        (switch-to-buffer-other-window (current-buffer)))
      (let ((project-root (locate-dominating-file default-directory ".src")))
        (when project-root
	      (hy-shell--with-live
	        ;; TODO Force the initial/end cases in a nicer way if possible
	        (hy-shell--send "\n")
	        (hy-shell--send (dhnam/hy-get-project-root-change-expr-string project-root))
	        (hy-shell--send "\n")))))

    (add-hook 'hy-mode-hook
	          (lambda () (local-set-key (kbd "C-c C-Z") 'dhnam/run-hy-with-default-project-root))))

  (defun dhnam/get-py-default-package-name ()
    (let ((project-root (dhnam/get-python-default-project-root))
	      (file-path (buffer-file-name)))
      (when project-root
	    (substring file-path (length project-root)
		           (- -1 (length (file-name-extension file-path)))))))

  (defun dhnam/kill-py-default-package-name ()
    (interactive)
    (let ((package-name (dhnam/get-python-default-project-root)))
      (if package-name
	      (kill-new (replace-regexp-in-string "/" "." (dhnam/get-py-default-package-name)))
	    (message "Cannot find project root"))))

  (defun dhnam/kill-hy-default-package-name ()
    (interactive)
    (let ((package-name (dhnam/get-python-default-project-root)))
      (if package-name
	      (kill-new (replace-regexp-in-string "_" "-"
		                                      (replace-regexp-in-string "/" "." (dhnam/get-py-default-package-name))))
	    (message "Cannot find project root"))))

  (require 'cl-lib)
  (defun dhnam/find-python-package-at-point (filename)
    (interactive
     (list
      (read-file-name
       "Find a package: " 
       (let ((package-name (cl-destructuring-bind
			                   (start end) (if (use-region-p)
					                           (list (region-beginning) (region-end))
					                         (progn
					                           (let* ((bounds (bounds-of-thing-at-point 'symbol)))
						                         (list (car bounds) (cdr bounds)))))
			                 (buffer-substring-no-properties start end))))
	     (if (string/starts-with package-name ".")
	         (let ((num-dot-prefixs (+ (string-match "\\.[^\\.]" package-name) 1)))
	           (concat (file-name-directory buffer-file-name)
		               (string-join (mapcar (lambda (x) "..")
					                        (number-sequence 0 (- num-dot-prefixs 2))) "/")
		               (if (> num-dot-prefixs 1) "/" "")
		               (replace-regexp-in-string
			            "\\." "/" (replace-regexp-in-string
				                   "-" "_" (substring package-name num-dot-prefixs)))))
	       (let ((python-default-project-root (dhnam/get-python-default-project-root)))
	         (if python-default-project-root
		         (concat python-default-project-root
			             (replace-regexp-in-string
			              "\\." "/" (replace-regexp-in-string
				                     "-" "_" package-name)))
	           python-default-project-root)))))))
    (find-file filename))

  (add-hook 'python-mode-hook (lambda () (local-set-key (kbd "C-c C-x C-f") 'dhnam/find-python-package-at-point)))
  (add-hook 'hy-mode-hook (lambda () (local-set-key (kbd "C-c C-x C-f") 'dhnam/find-python-package-at-point)))
  )

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


(defun dhnam/load-commands-for-pdb-with-hy ()
  (interactive)
  (local-set-key (kbd "M-j") #'dhnam/comint-send-input-for-hy))


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

(defun dhnam/insert-source-conda ()
  (interactive)
  (insert "source $(dirname $(which conda))/../etc/profile.d/conda.sh"))
