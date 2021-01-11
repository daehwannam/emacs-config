
(progn
  ;; package install
  ;; https://realpython.com/blog/python/emacs-the-best-python-editor/
  (defvar installable-domain-specific-packages) ; (defvar machine-domain-specific-packages some-value)
  (setq installable-domain-specific-packages (machine-config-get 'installable-packages))

  (mapc #'install-package-unless-installed
	installable-domain-specific-packages))
