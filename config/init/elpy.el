;; (dolist (fname (directory-files "~/.emacs.d/elpa/"))
;;   (if (cl-search "elpy" fname)  ; if fname contains "elpy"
;;       (progn
;; 	nil  ;; old ely conditional code
;; 	)))


;;; elpy : https://github.com/jorgenschaefer/elpy

(if (or (require 'elpy nil t) (fboundp 'elpy-enable)) ; 'require is used when loading without elpa package (then from "config/package/elpy")
    (progn
      ;; elpy setting
      ;; (package-initialize)
      (elpy-enable)
      (setq elpy-rpc-backend "jedi")
      ;; (setq elpy-rpc-python-command "python3")
      ;; (cond
      ;;  ;; ((string-equal machine-domain "ms") ; Microsoft Windows
      ;;  ;;  (progn
      ;;  ;;    (do-something bla bla bla)))
      ;;  ;; ((string-equal machine-domain "vbox") ; vbox linux
      ;;  ;;  (progn
      ;;  ;;    (do-something bla bla bla)))
      ;;  ((string-equal machine-domain "hegel") ; hegel
      ;; 	(setenv "WORKON_HOME" "~/bin/anaconda2/envs/")
      ;; 	(pyvenv-mode 1))
      ;;  ((string-equal machine-domain "engels") ; engels
      ;; 	(setenv "WORKON_HOME" "~/bin/anaconda3/envs/")
      ;; 	(pyvenv-mode 1)
      ;; 	(pyvenv-workon "py3"))		       ; use "M-x pyvenv-deactivate" to deactivate
      ;;  ((string-equal machine-domain "vbox") ; vbox
      ;; 	(setenv "WORKON_HOME" "~/bin/anaconda3/envs/")
      ;; 	(pyvenv-mode 1)
      ;; 	(pyvenv-workon "py2"))		       ; use "M-x pyvenv-deactivate" to deactivate
      ;;  )
      (when (machine-config-get 'pyvenv-workon-home-path)
	(setenv "WORKON_HOME" (machine-config-get 'pyvenv-workon-home-path))
	(pyvenv-mode 1))
      (when (machine-config-get 'pyvenv-name)
	(pyvenv-workon (machine-config-get 'pyvenv-name))) ; use "M-x pyvenv-deactivate" to deactivate
      

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

      ;; (add-hook 'python-mode-hook
      ;; 		(lambda () (local-set-key (kbd "C-c Z") 'elpy-shell-switch-to-shell-with-new-root)))
      (add-hook 'python-mode-hook
		(lambda () (local-set-key (kbd "C-c Z") 'elpy-set-project-root)))

      ;; (add-hook 'python-mode-hook
      ;; 		(lambda () (local-set-key (kbd "C-c C-g") 'elpy-goto-definition)))

      ;; https://realpython.com/blog/python/emacs-the-best-python-editor/
      ;; (unless (member machine-domain '("vbox" "machine here don't activate below"))
      (when (and (not (machine-config-get 'elpy-flycheck-deactivate)) (require 'flycheck nil t))
	(setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
	(add-hook 'elpy-mode-hook 'flycheck-mode)
	;; (defun my-flycheck-disable-checker ()
	;;   flycheck-disable-checker 'python-pylint)
	;; (add-hook 'flycheck-mode-hook 'my-flycheck-disable-checker)
	;; (add-to-list 'flycheck-disabled-checkers 'python-pylint)
	)

	;; (elpy-use-ipython)  # deprecated
	;; (setq python-shell-interpreter "ipython"
	;;       python-shell-interpreter-args "-i --simple-prompt")
      (when (and (not (machine-config-get 'elpy-autopep8-deactivate)) (require 'py-autopep8 nil t))
	(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))

      ;; (elpy-use-ipython) ; deprecated

      ;; https://github.com/paetzke/py-autopep8.el
      (setq py-autopep8-options '("--max-line-length=97"))
      ))

;; Usefule elpy command
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
