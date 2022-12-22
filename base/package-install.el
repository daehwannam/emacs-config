
(progn
  ;; package install
  ;; https://realpython.com/blog/python/emacs-the-best-python-editor/
  (defvar installable-domain-specific-packages) ; (defvar machine-domain-specific-packages some-value)
  (setq installable-domain-specific-packages (apply 'cl-concatenate (cons 'list (dhnam/machine-config-get-all 'installable-packages))))

  (mapc (lambda (package)
          (if (consp package)
              (unless (package-installed-p (car package))
                (quelpa package))
            (install-package-unless-installed package)))
	    installable-domain-specific-packages))
