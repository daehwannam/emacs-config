
(progn
  ;; package install
  ;; https://realpython.com/blog/python/emacs-the-best-python-editor/
  (defvar installable-domain-specific-packages) ; (defvar machine-domain-specific-packages some-value)
  (setq installable-domain-specific-packages (machine-config-get 'installable-packages))

  (let ((package-content-refreshed nil))
    (mapc #'(lambda (package)
	      (unless (package-installed-p package)
		(print "------------------------------------------------------------------")
		(print package)
		(unless package-content-refreshed
		  ;; https://magit.vc/manual/magit/Installing-from-Melpa.html
		  (package-refresh-contents)
		  (setq package-content-refreshed t))
		(package-install package)))
	  installable-domain-specific-packages)))
