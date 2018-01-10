;;; elpy : https://github.com/jorgenschaefer/elpy

(dolist (fname (directory-files "~/.emacs.d/elpa/"))
  (if (cl-search "elpy" fname)  ; if fname contains "elpy"
      (progn
	; elpy setting
	(package-initialize)
	(elpy-enable)
	(setq elpy-rpc-backend "jedi")
	;(setq elpy-rpc-python-command "python3")

	(let (domain-name)
	  (setq domain-name (trim-string (get-string-from-file "~/.emacs.d/config/domain.txt")))
	  (cond
	   ;; ((string-equal domain-name "ms\n") ; Microsoft Windows
	   ;;  (progn
	   ;;    (do-something bla bla bla)))
	   ;; ((string-equal domain-name "vbox\n") ; vbox linux
	   ;;  (progn
	   ;;    (do-something bla bla bla)))
	   ((string-equal domain-name "hegel") ; hegel
	    (progn
	      (setenv "WORKON_HOME" "~/bin/anaconda2/envs/")
	      (pyvenv-mode 1)))
	   ((string-equal domain-name "engels") ; engels
	    (progn
	      (setenv "WORKON_HOME" "~/bin/anaconda3/envs/")
	      (pyvenv-mode 1)))))

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
