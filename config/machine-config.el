
;;; initialize machine options if it's not exist.
(unless (file-exists-p "~/.emacs.d/config/options.txt")
  ;; https://stackoverflow.com/questions/17376706/in-emacs-lisp-how-can-i-append-a-string-to-a-file-that-i-dont-like-to-open
  ;; https://stackoverflow.com/questions/14071991/how-to-create-an-empty-file-by-elisp/14072295
  ;;
  ;; options are sorted in descending order of priority
  (write-region "(base other-machine option1 option2)" nil "~/.emacs.d/config/options.txt"))
(defvar machine-options (car (read-from-string (get-string-from-file "~/.emacs.d/config/options.txt"))))

(defvar machine-config-list)
(setq machine-config-list
  '((theme-style (white-theme-machine white))
    (ai-edu-key-binding (vbox t))
    (path-to-slime-helper (vbox "~/quicklisp/slime-helper.el"))
    (path-to-inferior-lisp-program (vbox "/usr/bin/sbcl"))
    (etags-list (vbox ("/usr/share/emacs/25.3/lisp" "~/.emacs.d")))
    (org-agenda-directory-pattern
     (ms-desktop ("e:/data/Dropbox/org/schedule/" ".*\\.org\\(\\.txt\\)?$"))
     (vbox ("~/data/Dropbox/org/schedule/" ".*\\.org\\(\\.txt\\)?$")))
    (pyvenv-workon-home-path
     (hegel "~/bin/anaconda2/envs/")
     (engels "~/program/anaconda3/envs/")
     (vbox "~/program/miniconda3/envs/")
     (kant "~/program/miniconda3/envs/")
     (py3-env "~/bin/anaconda3/envs/"))
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
    (installable-packages
     ;; (base (vlf magit key-chord modalka evil avy hydra dash counsel multi-term))
     (basic (vlf magit dash multi-term counsel))
     (vbox (vlf magit key-chord modalka evil dash  multi-term auctex latex-preview-pane material-theme elpy flycheck py-autopep8 blacken sphinx-doc workgroups2 perspective counsel iedit wgrep which-key counsel-projectile persp-projectile paredit markdown-mode tuareg ein hy-mode)) ; removed: highlight
     ;; (engels (vlf magit key-chord modalka evil dash elpy flycheck py-autopep8 blacken wgrep which-key projectile flx-ido))
     (engels (vlf magit key-chord modalka evil dash  multi-term elpy flycheck py-autopep8 blacken sphinx-doc wgrep which-key counsel counsel-projectile markdown-mode hy-mode))
     ;; (engels (vlf magit key-chord modalka evil dash elpy py-autopep8 blacken))
     (programming (vlf magit key-chord modalka evil dash multi-term counsel elpy flycheck py-autopep8 blacken))
     (lsp-setup (lsp-mode lsp-ui flycheck company lsp-treemacs lsp-ivy dap-mode))
     (ms-laptop (vlf magit key-chord modalka evil dash))
     (ms-desktop (vlf magit key-chord modalka evil dash))
     (semparse (jsonnet-mode))
     )))

;;; emacs configuration association list
(comment
 (defun machine-config-get (key)
   (car (cdr (cl-assoc machine-options (cdr (assoc key machine-config-list))
		       :test (lambda (x y) (member y x)))))))

(defun machine-config-get (key)
   (car (cdr (cl-assoc machine-options (cdr (assoc key machine-config-list))
		       :test (lambda (x y) (eq (car x) y))))))

(defun machine-config-get-all (key)
  (mapcar (lambda (x) (car (cdr x)))
	  (cl-remove-if-not (lambda (x) (member (car x) machine-options))
			    (cdr (assoc key machine-config-list)))))

(print (machine-config-get-all 'installable-packages))

(assoc 'installable-packages machine-config-list)
