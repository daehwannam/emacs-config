
(use-existing-pkg elpy
  ;; elpy : https://github.com/jorgenschaefer/elpy

  ;; https://elpy.readthedocs.io/en/latest/introduction.html
  ;; 
  ;; In order to use all the features (such as navigation with M-.),
  ;; you’ll need to install some python libraries:
  ;; --> python3-jedi black autopep8 yapf

  :init
  (progn
    ;; elpy setting
    ;; (package-initialize)
    (elpy-enable)

    (progn
     ;; jedi is the default value
     (setq elpy-rpc-backend "jedi"))

    (let ((pyvenv-workon-home-path (machine-config-get-first 'pyvenv-workon-home-path)))
      (when pyvenv-workon-home-path
	(progn
	  (setenv "WORKON_HOME" pyvenv-workon-home-path)
	  (pyvenv-mode 1))

	(progn
	  (setq elpy-rpc-virtualenv-path 'current)
	  (comment
	   ;; RPC setup
	   (setq elpy-rpc-virtualenv-name "elpy-rpc")
	   (let ((default-elpy-rpc-virtualenv-path elpy-rpc-virtualenv-path))
	     (comment (assert (eq default-elpy-rpc-virtualenv-path 'default)))
	     (setq elpy-rpc-virtualenv-path
		   (or (machine-config-get-first 'elpy-rpc-virtualenv-path)
		       (joindirs pyvenv-workon-home-path elpy-rpc-virtualenv-name))))))
	(comment
	 ;; use the current virtual environment as RPC server
	 ;; https://github.com/jorgenschaefer/elpy/issues/1878#issuecomment-761518001
	 ;; (this code is not tested yet)
	 (setq elpy-rpc-virtualenv-path 'current))))

    (when (machine-config-get-first 'pyvenv-name)
      (pyvenv-workon (machine-config-get-first 'pyvenv-name))) ; use "M-x pyvenv-deactivate" to deactivate
    

    (defun elpy-set-project-root (new-root) ; redefined
      "Set the Elpy project root to NEW-ROOT."
      (interactive "DNew project root: ")
      (setq elpy-project-root (file-truename new-root)))

    
    (defun elpy-shell-switch-to-shell-with-new-root (new-root)
      "Switch to inferior Python process buffer."
      (interactive "DNew project root: ")
      (setq elpy-project-root (file-truename new-root)) ; modified from 'elpy-set-project-root
      (setq elpy--shell-last-py-buffer (buffer-name))
      (pop-to-buffer (process-buffer (elpy-shell-get-or-create-process)))

      ;; 	(let ((dir-path default-directory))
      ;; 	  (setq elpy--shell-last-py-buffer (buffer-name))
      ;; 	  (pop-to-buffer (process-buffer (elpy-shell-get-or-create-process)))
      ;; 	  (insert (concat "import sys
      ;; sys.path.append('" dir-path "')"))
      ;; 	  (comint-send-input))
      )

    (add-hook 'python-mode-hook
	      (lambda () (local-set-key (kbd "C-c Z") 'elpy-set-project-root)))

    (progn
      ;; change default proejct source
      (setq elpy-project-root-finder-functions
	    '(elpy-project-find-src-root
	      elpy-project-find-projectile-root
	      elpy-project-find-python-root
	      elpy-project-find-git-root
	      elpy-project-find-hg-root
	      elpy-project-find-svn-root))

      (defun elpy-project-find-src-root ()
	"Return the current src repository root, if any."
	(locate-dominating-file default-directory ".src"))
      )

    (when (machine-config-get-first 'elpy-flycheck-activate)
      ;; https://realpython.com/blog/python/emacs-the-best-python-editor/
      (use-existing-pkg flycheck
	:init
	(progn
	  ;; (assert (require 'flycheck nil t))
	  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
	  (add-hook 'elpy-mode-hook 'flycheck-mode)
	  ;; (defun my-flycheck-disable-checker ()
	  ;;   flycheck-disable-checker 'python-pylint)
	  ;; (add-hook 'flycheck-mode-hook 'my-flycheck-disable-checker)
	  ;; (add-to-list 'flycheck-disabled-checkers 'python-pylint)
	  )))

    ;; (elpy-use-ipython)  # deprecated
    ;; (setq python-shell-interpreter "ipython"
    ;;       python-shell-interpreter-args "-i --simple-prompt")

    (when (machine-config-get-first 'elpy-autopep8-activate)
      (use-existing-pkg py-autopep8
	:init
	(progn
	  ;; (assert (require 'py-autopep8 nil t))
	  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

	  ;; https://github.com/paetzke/py-autopep8.el
	  (setq py-autopep8-options '("--max-line-length=97")))))

    (progn
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
	(setq deactivate-mark t)))

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
	 '(ein:polymode t)  ; enable Elpy
	 ;; '(ein:cell-input-area ((t (:background "black"))))
	 )
	(add-hook 'ein:notebook-mode-hook 'linum-mode)
	(setq ein:worksheet-enable-undo t)))

    (defun get-python-default-project-root ()
      (locate-dominating-file default-directory ".src"))

    (setq elpy-project-root-finder-functions
	  (cons 'get-python-default-project-root elpy-project-root-finder-functions)))

  :bind (("C-c Z" . elpy-set-project-root)
	 ("C-c W" . py-repl-kill-ring-save)))

(comment
 ;; Useful elpy command
 ;; C-c C-o runs the command elpy-occur-definitions


;;; For conda environment, "source activate <env>" should be done before running emacs.
 ;; https://emacs.stackexchange.com/a/16638


 ;; [Flake setting]
 ;; http://flake8.pycqa.org/en/latest/user/configuration.html
 ;; add follwoing text to the ~/.config/flake8
 ;; ------------------- ~/.config/flake8 -------------------------
 ;; [flake8]
 ;; ignore = E501
 ;; --------------------------------------------------------------


 ;; [pdb setting]
 ;; https://stackoverflow.com/questions/38346577/debuggin-with-pdb-within-a-conda-environment
 ;; ex.
 ;;     cp /usr/bin/pdb ~/bin/anaconda3/bin/pdb
 ;;     cp /usr/lib/python3.4/pdb.py ~/bin/anaconda3/bin/pdb.py
 )

