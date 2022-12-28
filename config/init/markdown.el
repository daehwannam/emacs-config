

(when (fboundp 'markdown-mode)
  (autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (autoload 'gfm-mode "markdown-mode"
    "Major mode for editing GitHub Flavored Markdown files" t)
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode)))

;; CHANGE ABOVE CODE BY USING FOLLOWING CODE

;; (dolist (fname (directory-files "~/.emacs.d/elpa/"))
;;   (if (cl-search "elpy" fname)  ; if fname contains "elpy"
;;       (progn
;; 	; elpy setting
;; 	(package-initialize)
;; 	(elpy-enable)
;; 	(setq elpy-rpc-backend "jedi")
;; 	;(setq elpy-rpc-python-command "python3")

;; 	(cond
;; 	 ;; ((string-equal machine-domain "ms") ; Microsoft Windows
;; 	 ;;  (progn
;; 	 ;;    (do-something bla bla bla)))
;; 	 ;; ((string-equal machine-domain "vbox") ; vbox linux
;; 	 ;;  (progn
;; 	 ;;    (do-something bla bla bla)))
;; 	 ((string-equal machine-domain "hegel") ; hegel
;; 	  (progn
;; 	    (setenv "WORKON_HOME" "~/bin/anaconda2/envs/")
;; 	    (pyvenv-mode 1)))
;; 	 ((string-equal machine-domain "engels") ; engels
;; 	  (progn
;; 	    (setenv "WORKON_HOME" "~/bin/anaconda3/envs/")
;; 	    (pyvenv-mode 1)))))

;;     ))

(provide 'dhnam-markdown)
