;;; elpy : https://github.com/jorgenschaefer/elpy

(dolist (fname (directory-files "~/.emacs.d/elpa/"))
  (if (cl-search "elpy" fname)  ; if fname contains "elpy"
      (progn
	; elpy setting
	(package-initialize)
	(elpy-enable)
	(setq elpy-rpc-backend "jedi")
	;(setq elpy-rpc-python-command "python3")

	(cond
	 ;; ((string-equal machine-domain "ms") ; Microsoft Windows
	 ;;  (progn
	 ;;    (do-something bla bla bla)))
	 ;; ((string-equal machine-domain "vbox") ; vbox linux
	 ;;  (progn
	 ;;    (do-something bla bla bla)))
	 ((string-equal machine-domain "hegel") ; hegel
	  (progn
	    (setenv "WORKON_HOME" "~/bin/anaconda2/envs/")
	    (pyvenv-mode 1)))
	 ((string-equal machine-domain "engels") ; engels
	  (progn
	    (setenv "WORKON_HOME" "~/bin/anaconda3/envs/")
	    (pyvenv-mode 1)
	    (pyvenv-workon "py3"))))

	;; https://realpython.com/blog/python/emacs-the-best-python-editor/
	(when (require 'flycheck nil t)
	  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
	  (add-hook 'elpy-mode-hook 'flycheck-mode))

	(when (require 'py-autopep8 nil t)
	  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))

	;; (elpy-use-ipython)  # deprecated
	;; (setq python-shell-interpreter "ipython"
	;;       python-shell-interpreter-args "-i --simple-prompt")
	)))

;;; For conda environment, "source activate <env>" should be done before running emacs.
;; https://emacs.stackexchange.com/a/16638


;; [Flake setting]
;; http://flake8.pycqa.org/en/latest/user/configuration.html
;; add follwoing text to the ~/.config/flake8
;; ------------------- ~/.config/flake8 -------------------------
;; [flake8]
;; ignore = E501
;; --------------------------------------------------------------
