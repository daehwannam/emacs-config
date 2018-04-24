;;; load emacs 24's package system. Add MELPA repository.
;; http://ergoemacs.org/emacs/emacs_package_system.html
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   ;; '("melpa" . "http://stable.melpa.org/packages/") ; many packages won't show if using stable
   '("melpa" . "http://melpa.milkbox.net/packages/")
   t))

;;; package initialization
(package-initialize)

;;; package install
;; https://realpython.com/blog/python/emacs-the-best-python-editor/
(defvar installable-domain-specific-packages) ; (defvar machine-domain-specific-packages some-value)
(setq installable-domain-specific-packages
  (cond
   ((string-equal machine-domain "default") '())
   ((string-equal machine-domain "basic") '(vlf magit workgroups2))
   ((string-equal machine-domain "vbox") '(vlf auctex magit material-theme elpy flycheck py-autopep8 workgroups2 latex-preview-pane))
   ((string-equal machine-domain "engels") '(vlf magit elpy flycheck py-autopep8 workgroups2))
   ;; ((string-equal machine-domain "engels") '(vlf magit elpy py-autopep8))
   ((string-equal machine-domain "programming") '(elpy))
   ))

(mapc #'(lambda (package)
	  (unless (package-installed-p package)
	    (package-install package)))
      installable-domain-specific-packages)
