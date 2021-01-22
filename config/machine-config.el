
;;; initialize machine options if it's not exist.
(unless (file-exists-p "~/.emacs.d/config/options.txt")
  ;; https://stackoverflow.com/questions/17376706/in-emacs-lisp-how-can-i-append-a-string-to-a-file-that-i-dont-like-to-open
  ;; https://stackoverflow.com/questions/14071991/how-to-create-an-empty-file-by-elisp/14072295
  ;;
  ;; options are sorted in descending order of priority
  (write-region "(default-machine option1 option2)" nil "~/.emacs.d/config/options.txt"))
(defvar machine-options (car (read-from-string (get-string-from-file "~/.emacs.d/config/options.txt"))))

(defvar machine-config-list)
(setq machine-config-list
  '((theme-style (white-theme-machine white))
    (ai-edu-key-binding (vbox t))
    (path-to-slime-helper (vbox "~/quicklisp/slime-helper.el"))
    (path-to-inferior-lisp-program (vbox "/usr/bin/sbcl"))
    (etags-list (vbox ("/usr/share/emacs/25.3/lisp" "~/.emacs.d")))
    (org-agenda-directory-pattern
     (ms ("e:/data/Dropbox/org/schedule/" ".*\\.org\\(\\.txt\\)?$"))
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
     (basic (vlf magit key-chord modalka evil multi-term counsel))
     (vbox (vlf magit key-chord modalka evil  multi-term auctex latex-preview-pane material-theme elpy flycheck py-autopep8 blacken sphinx-doc workgroups2 perspective counsel iedit wgrep which-key counsel-projectile eyebrowse persp-projectile paredit markdown-mode tuareg ein hy-mode)) ; removed: highlight
     ;; (engels (vlf magit key-chord modalka evil elpy flycheck py-autopep8 blacken wgrep which-key projectile flx-ido  eyebrowse))
     (engels (vlf magit key-chord modalka evil  multi-term elpy flycheck py-autopep8 blacken sphinx-doc wgrep which-key counsel counsel-projectile eyebrowse markdown-mode hy-mode))
     ;; (engels (vlf magit key-chord modalka evil elpy py-autopep8 blacken))
     (programming (vlf magit key-chord modalka evil multi-term counsel elpy flycheck py-autopep8 blacken))
     (ms-laptop (vlf magit key-chord modalka evil doom-modeline))
     (ms-desktop (vlf magit key-chord modalka evil doom-modeline))
     )))

;;; emacs configuration association list
(defun machine-config-get (key)
  (car (cdr (cl-assoc machine-options (cdr (assoc key machine-config-list))
		      :test (lambda (x y) (member y x))))))
