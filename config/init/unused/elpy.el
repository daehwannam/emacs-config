;;; elpy : https://github.com/jorgenschaefer/elpy

(dolist (fname (directory-files "~/.emacs.d/elpa/"))
  (if (cl-search "elpy" fname)  ; if fname contains "elpy"
      (progn
	(package-initialize)
	(elpy-enable)
	(setq elpy-rpc-backend "jedi"))))


;;; elpy setting for python 3
;; https://emacs.stackexchange.com/questions/16637/how-to-set-up-elpy-to-use-python3/16638#16638
