;;; elpy : https://github.com/jorgenschaefer/elpy

(dolist (fname (directory-files "~/.emacs.d/elpa/"))
  (if (cl-search "elpy" fname)  ; if fname contains "elpy"
      (progn
	(package-initialize)
	(elpy-enable)
	(setq elpy-rpc-backend "jedi"))))
