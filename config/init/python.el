
;---;;; python-mode
;---;; https://www.emacswiki.org/emacs/ProgrammingWithPythonModeDotEl
;---;; https://gitlab.com/python-mode-devs/python-mode
;---(autoload 'python-mode "python-mode" "Python Mode." t)
;---(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;---add-to-list 'interpreter-mode-alist '("python" . python-mode))

;;; python-mode linum-mode setting
(add-hook 'python-mode-hook 'linum-mode)

;; use C-j as newline-and-indent
(defun non-electric-indent-mode ()
  (electric-indent-mode -1))
;; (add-hook 'python-mode-hook 'non-electric-indent-mode) ; https://github.com/jorgenschaefer/elpy/issues/195

;;; python 3 setting
;; https://askubuntu.com/questions/460668/how-to-use-python3-in-emacs-on-ubuntu-14-04
;; (setq python-shell-interpreter "python3")  ;; it occurs error for virtual environment of python2


;;; ipython
;; https://stackoverflow.com/a/17817119
;; (defun run-ipython ()
;;   (interactive)
;;   (term "ipython")
;;   (rename-buffer "*IPython*"))
(defun run-ipython ()
  (interactive)
  ;;; count windows: https://emacs.stackexchange.com/questions/3494/how-to-count-all-of-the-windows-in-a-frame
  (if (< (length (mapcar #'window-buffer (window-list))) 2)
      (split-window-right))
  (other-window 1)
  (named-term "ipython" "*IPython*"))

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

(defun how-many-str (regexp str)
  (loop with start = 0
        for count from 0
        while (string-match regexp str start)
        do (setq start (match-end 0))
        finally return count))

(defun py-repl-kill-ring-save-without-empty-lines (beg end &optional region)
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

(unless (fboundp 'elpy-shell--region-without-indentation)
  (defun elpy-shell--region-without-indentation (beg end)
    "Return the current region as a string, but without indentation."
    (if (= beg end)
	""
      (let ((region (buffer-substring beg end))
	    (indent-level nil)
	    (indent-tabs-mode nil))
	(with-temp-buffer
	  (insert region)
	  (goto-char (point-min))
	  (while (< (point) (point-max))
	    (cond
	     ((or (elpy-shell--current-line-only-whitespace-p)
		  (python-info-current-line-comment-p)))
	     ((not indent-level)
	      (setq indent-level (current-indentation)))
	     ((and indent-level
		   (< (current-indentation) indent-level))
	      (error (message "X%sX" (thing-at-point 'line)))))
            ;; (error "Can't adjust indentation, consecutive lines indented less than starting line")))
	    (forward-line))
	  (indent-rigidly (point-min)
			  (point-max)
			  (- indent-level))
	  ;; 'indent-rigidly' introduces tabs despite the fact that 'indent-tabs-mode' is nil
	  ;; 'untabify' fix that
	  (untabify (point-min) (point-max))
	  (buffer-string))))))

(defun py-repl-kill-ring-save (beg end &optional region)
  (interactive (list (mark) (point)
		     (prefix-numeric-value current-prefix-arg)))
  (kill-new (elpy-shell--region-without-indentation beg end))
  (setq deactivate-mark t))

;; https://stackoverflow.com/a/14230685
(add-hook 'python-mode-hook
	  (lambda () (local-set-key (kbd "C-c W") #'py-repl-kill-ring-save)))


;; python docstring
;; https://github.com/naiquevin/sphinx-doc.el
;; 
;; The command is mapped to "C-c M-d"
(when (fboundp 'sphinx-doc-mode)
  (add-hook 'python-mode-hook (lambda ()
				(require 'sphinx-doc)
				(sphinx-doc-mode t))))

;; EIN: Emacs IPython Notebook
;; https://github.com/millejoh/emacs-ipython-notebook

(when (fboundp 'ein:run)
  (custom-set-variables
   '(ein:polymode t)  ; enable Elpy
   ;; '(ein:cell-input-area ((t (:background "black"))))
   )
  (add-hook 'ein:notebook-mode-hook 'linum-mode)
  (setq ein:worksheet-enable-undo t)
)

;; run IPython notebook server with specific port number
;; https://github.com/tkf/emacs-ipython-notebook/issues/109#issuecomment-16874676
;;
;; $ ipython notebook --port 9999
;;
;; then login EIN --> M-x ein:login RET 9999 RET


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
