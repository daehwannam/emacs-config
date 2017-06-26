;;; virtual environment setting
;; http://emacs.stackexchange.com/questions/20092/using-conda-environments-in-emacs
;; usage: M-x pyvenv-workon virtual_env_name (ex. tensorflow)

(let (domain_name (get-string-from-file "~/.emacs.d/config/domain.txt"))
  (cond
   ;; ((string-equal domain_name "ms\n") ; Microsoft Windows
   ;;  (progn
   ;;    (do-something bla bla bla)))
   ;; ((string-equal domain_name "vbox\n") ; vbox linux
   ;;  (progn
   ;;    (do-something bla bla bla)))
   ;; ((string-equal system-type "gnu/linux") ; linux
   ((string-equal domain-name "hegel\n")
    (progn
      (setenv "WORKON_HOME" "~/bin/anaconda2/envs/")
      (pyvenv-mode 1)))))
