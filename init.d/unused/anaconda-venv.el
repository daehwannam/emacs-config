;;; virtual environment setting
;; http://emacs.stackexchange.com/questions/20092/using-conda-environments-in-emacs
;; usage: M-x pyvenv-workon virtual_env_name (ex. tensorflow)
(setenv "WORKON_HOME" "~/bin/anaconda2/envs/")
(pyvenv-mode 1)
