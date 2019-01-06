
;;; initialize machine id if it's not exist.
(unless (file-exists-p "~/.emacs.d/config/id.txt")
  ;; https://stackoverflow.com/questions/17376706/in-emacs-lisp-how-can-i-append-a-string-to-a-file-that-i-dont-like-to-open
  ;; https://stackoverflow.com/questions/14071991/how-to-create-an-empty-file-by-elisp/14072295
  (write-region "default-machine" nil "~/.emacs.d/config/id.txt"))
(defvar machine-id (car (read-from-string (get-string-from-file "~/.emacs.d/config/id.txt"))))


(defvar machine-config-list
  `((theme-style . dark)
    (ai-edu-key-binding
     . ,(cond ((eq machine-id 'vbox) t)))
    (path-to-slime-helper
     . ,(cond ((eq machine-id 'vbox) "~/quicklisp/slime-helper.el")
	     (t "~/NO-PATH-POSSIBLE")))
    (path-to-inferior-lisp-program
     . ,(cond ((eq machine-id 'vbox) "/usr/bin/sbcl")
	     (t "/NO-PATH-POSSIBLE")))
    (etags-list
     . ,(cond ((eq machine-id 'vbox) '("/usr/share/emacs/25.3/lisp" "~/.emacs.d"))))
    (org-agenda-directory-pattern
     . ,(cond ((eq machine-id 'ms) '("e:/data/Dropbox/org/schedule/" ".*\\.org\\(\\.txt\\)*$"))
	     ((eq machine-id 'vbox) '("~/data/Dropbox/org/schedule/" ".*\\.org\\(\\.txt\\)*$"))))
    (pyvenv-workon-home-path
     . ,(cond ((eq machine-id 'hegel) "~/bin/anaconda2/envs/")
	     ((eq machine-id 'engels) "~/bin/anaconda3/envs/")
	     ((eq machine-id 'vbox) "~/bin/anaconda3/envs/")))
    (pyvenv-name
     . ,(cond ((eq machine-id 'engels) "py3")
	     ((eq machine-id 'vbox) "py2")))
    (elpy-flycheck-activate
     . ,(cond ((eq machine-id 'vbox) t)))
    (elpy-autopep8-activate
     . ,(cond ((eq machine-id 'vbox) t)))
    (installable-packages
     . ,(cond ((eq machine-id 'basic) '(vlf magit))
	     ((eq machine-id 'vbox) '(vlf auctex magit material-theme elpy flycheck py-autopep8 workgroups2 latex-preview-pane perspective counsel iedit wgrep which-key projectile flx-ido counsel-projectile eyebrowse persp-projectile paredit markdown-mode highlight))
	     ;; ((eq machine-id 'engels) '(vlf magit elpy flycheck py-autopep8 wgrep which-key projectile flx-ido  eyebrowse))
	     ((eq machine-id 'engels) '(vlf magit elpy flycheck py-autopep8 wgrep which-key projectile flx-ido counsel counsel-projectile eyebrowse sphinx-doc markdown-mode))
	     ;; ((eq machine-id 'engels) '(vlf magit elpy py-autopep8))
	     ((eq machine-id 'programming) '(elpy))
	     ((eq machine-id 'ms-laptop) '(vlf magit))
	     ((eq machine-id 'ms-desktop) '(vlf magit))
	     ))
					; vbox: (vlf auctex magit material-theme elpy flycheck py-autopep8 workgroups2 latex-preview-pane perspective counsel iedit wgrep which-key projectile flx-ido counsel-projectile eyebrowse persp-projectile paredit markdown-mode highlight)
					; engels: (vlf magit elpy flycheck py-autopep8 wgrep which-key projectile flx-ido counsel counsel-projectile eyebrowse sphinx-doc markdown-mode)
    ))

;;; emacs configuration association list
(defun machine-config-get (key)
  (cdr (assoc key machine-config-list)))
