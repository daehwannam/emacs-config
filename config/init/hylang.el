
(when (fboundp 'hy-mode)
  ;; Hy lang setup

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


(add-hook 'hy-mode-hook (lambda () (local-set-key (kbd "C-c C-x C-p") #'dhnam/kill-ring-save-pdb-breakpoint)))

(progn
  ;; pdb setup for Hy
  (setq pdb-hy-input-frame-left-side "import hy; from hy.contrib.hy_repr import hy_repr; print(hy_repr(hy.eval(hy.read_str(\"\"\"")
  (setq pdb-hy-input-frame-right-side "\"\"\"))))"))

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
	     (if (dhnam/string-starts-with package-name ".")
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
  (add-hook 'hy-mode-hook (lambda () (local-set-key (kbd "C-c C-x C-f") 'dhnam/find-python-package-at-point))))


(provide 'init-hylang)
