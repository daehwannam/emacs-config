
;;; initialize machine options if it's not exist.
(unless (file-exists-p "~/.emacs.d/config/options.txt")
  ;; https://stackoverflow.com/questions/17376706/in-emacs-lisp-how-can-i-append-a-string-to-a-file-that-i-dont-like-to-open
  ;; https://stackoverflow.com/questions/14071991/how-to-create-an-empty-file-by-elisp/14072295
  ;;
  ;; options are sorted in descending order of priority
  (write-region "(basic other-machine option1 option2)" nil "~/.emacs.d/config/options.txt"))
(defvar machine-options (car (read-from-string (get-string-from-file "~/.emacs.d/config/options.txt"))))

(defvar machine-config-list)
(setq machine-config-list
  '((theme-style (white-theme-machine white))
    (ai-edu-key-binding (vbox t))
    (path-to-slime-helper
     (vbox "~/quicklisp/slime-helper.el")
     (ubuntu-laptop "~/quicklisp/slime-helper.el"))
    (path-to-inferior-lisp-program
     (vbox "/usr/bin/sbcl")
     (ubuntu-laptop "/home/dhnam/program/miniconda3/envs/default/bin/sbcl"))
    (etags-list (vbox ("/usr/share/emacs/25.3/lisp" "~/.emacs.d")))
    (org-agenda-directory-pattern
     (ms-desktop ("e:/data/Dropbox/org/schedule/" ".*\\.org\\(\\.txt\\)?$"))
     (vbox ("~/data/Dropbox/org/schedule/" ".*\\.org\\(\\.txt\\)?$"))
     (descartes ("~/data/Dropbox/org/schedule/" ".*\\.org\\(\\.txt\\)?$")))
    (pyvenv-workon-home-path
     (hegel "~/bin/anaconda2/envs/")
     (engels "~/program/miniconda3/envs/")
     (vbox "~/program/miniconda3/envs/")
     (kant "~/program/miniconda3/envs/")
     (erasmus "~/program/miniconda3/envs/")
     (anaconda-env "~/program/anaconda3/envs/")
     (miniconda-env "~/program/miniconda3/envs/"))
    (elpy-simple-config
     (min-elpy-setup t))
    (pyvenv-name
     ;; (py3-env "py3")
     ;; (engels "default")
     ;; (engels "py3")
     ;; (vbox "py3")
     ;; (kant "py3")
     )
    (elpy-flycheck-activate
     (engels nil)
     )
    (elpy-autopep8-activate
     (engels nil)
     )
    (eaf-path
     (eaf-setup "~/.emacs.d/package/emacs-application-framework/"))
    (exwm-multiple-physical-monitor-layout
     (descartes descartes-triple)
     )
    (exwm-multiple-monitor-layout-type
     (descartes triple)
     (ubuntu-laptop nil))
    (exwm-font-height
     (descartes 145))

    (installable-packages
     ;; (base (vlf magit key-chord modalka evil avy hydra dash counsel multi-term))
     (basic (vlf magit dash multi-term counsel async wgrep which-key paredit poporg))
     (documentation (org-fragtog auctex latex-preview-pane pdf-tools))
     (vbox (vlf magit key-chord modalka evil dash  multi-term auctex latex-preview-pane material-theme elpy flycheck py-autopep8 blacken sphinx-doc workgroups2 perspective counsel iedit wgrep which-key counsel-projectile persp-projectile paredit markdown-mode tuareg ein hy-mode)) ; removed: highlight
     ;; (engels (vlf magit key-chord modalka evil dash elpy flycheck py-autopep8 blacken wgrep which-key projectile flx-ido))
     (engels (vlf magit key-chord modalka evil dash  multi-term elpy flycheck py-autopep8 blacken sphinx-doc wgrep which-key counsel counsel-projectile markdown-mode hy-mode))
     ;; (engels (vlf magit key-chord modalka evil dash elpy py-autopep8 blacken))
     (programming (markdown-mode sphinx-doc ein hy-mode jsonnet-mode))
     (python-setup (pyvenv highlight-indentation))
     (elpy-setup (elpy flycheck py-autopep8 blacken))
     (min-elpy-setup (elpy))
     (lsp-setup (dash lsp-mode lsp-ui flycheck company lsp-treemacs lsp-ivy dap-mode))
     (eglot-setup (eglot company))
     (ms-laptop (vlf magit key-chord modalka evil dash))
     (ms-desktop (vlf magit key-chord modalka evil dash))
     (semparse (jsonnet-mode))
     (web-edit (atomic-chrome))
     (exwm-setup (exwm volume hide-mode-line ivy-posframe))
     )))

;;; emacs configuration association list
(comment
 (defun machine-config-get (key)
   (car (cdr (cl-assoc machine-options (cdr (assoc key machine-config-list))
		       :test (lambda (x y) (member y x))))))

 (defun machine-config-get (key)
   (car (cdr (cl-assoc machine-options (cdr (assoc key machine-config-list))
		       :test (lambda (x y) (eq (car x) y)))))))

(defun machine-config-get-first (key)
  (car (machine-config-get-all key)))

(defun machine-config-get-all (key)
  (mapcar (lambda (x) (car (cdr x)))
	  (cl-remove-if-not (lambda (x) (member (car x) machine-options))
			    (cdr (assoc key machine-config-list)))))

(print (machine-config-get-all 'installable-packages))

(assoc 'installable-packages machine-config-list)
