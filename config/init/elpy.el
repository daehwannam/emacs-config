;;; elpy : https://github.com/jorgenschaefer/elpy

(dolist (fname (directory-files "~/.emacs.d/elpa/"))
  (if (cl-search "elpy" fname)  ; if fname contains "elpy"
      (progn
	; elpy setting
	(package-initialize)
	(elpy-enable)
	(setq elpy-rpc-backend "jedi")
	;(setq elpy-rpc-python-command "python3")
	)))

;;; For conda environment, "source activate <env>" should be done before running emacs.
;; https://emacs.stackexchange.com/a/16638
