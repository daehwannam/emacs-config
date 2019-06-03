
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
  '((theme-style (white-style-machine white))
    (ai-edu-key-binding (vbox t))
    (path-to-slime-helper (vbox "~/quicklisp/slime-helper.el"))
    (path-to-inferior-lisp-program (vbox "/usr/bin/sbcl"))
    (etags-list (vbox ("/usr/share/emacs/25.3/lisp" "~/.emacs.d")))
    (org-agenda-directory-pattern
     (ms ("e:/data/Dropbox/org/schedule/" ".*\\.org\\(\\.txt\\)?$"))
     (vbox ("~/data/Dropbox/org/schedule/" ".*\\.org\\(\\.txt\\)?$")))
    (pyvenv-workon-home-path
     (hegel "~/bin/anaconda2/envs/")
     (engels "~/bin/anaconda3/envs/")
     (vbox "~/bin/anaconda3/envs/")
     (py3-env "~/bin/anaconda3/envs/"))
    (pyvenv-name
     (py3-env "py3")
     (engels "py3")
     (vbox "py3"))
    (elpy-flycheck-deactivate
     (vbox t))
    (elpy-autopep8-deactivate
     (vbox t))
    (installable-packages
     (basic (vlf magit))
     (vbox (vlf magit auctex material-theme elpy flycheck py-autopep8 workgroups2 latex-preview-pane perspective counsel iedit wgrep which-key projectile flx-ido counsel-projectile eyebrowse persp-projectile paredit markdown-mode highlight))
     ;; (engels (vlf magit elpy flycheck py-autopep8 wgrep which-key projectile flx-ido  eyebrowse))
     (engels (vlf magit elpy flycheck py-autopep8 wgrep which-key projectile flx-ido counsel counsel-projectile eyebrowse sphinx-doc markdown-mode))
     ;; (engels (vlf magit elpy py-autopep8))
     (programming (vlf magit counsel elpy flycheck py-autopep8))
     (ms-laptop (vlf magit))
     (ms-desktop (vlf magit))
     )))

;;; emacs configuration association list
(defun machine-config-get (key)
  (car (cdr (cl-assoc machine-options (cdr (assoc key machine-config-list))
		      :test (lambda (x y) (member y x))))))
