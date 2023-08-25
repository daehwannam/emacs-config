
(progn
  ;; package install
  ;; https://realpython.com/blog/python/emacs-the-best-python-editor/
  (progn
    (defvar dhnam/installable-domain-specific-packages nil)
    (setq dhnam/installable-domain-specific-packages
          (apply 'cl-concatenate (cons 'list (dhnam/machine-config-get-all 'installable-packages)))))

  (progn
    (defvar dhnam/installable-machine-specific-packages nil)
    (let ((package-file-name (concat dhnam/emacs-root-dir "config/packages.txt")))
      (unless (file-exists-p package-file-name)
        (write-region "(vlf magit)" nil package-file-name))
      (setq dhnam/installable-machine-specific-packages
            (car (read-from-string (dhnam/get-string-from-file package-file-name))))))

  (mapc (lambda (package)
          (if (consp package)
              (let ((package-type (car package))
                    (package-args (cdr package)))
                (cond
                 ((eq package-type 'quelpa)
                  (let ((package-name (car package-args)))
                    (unless (package-installed-p package-name)
                      (quelpa package-args))))
                 ((eq package-type 'refreshing)
                  (let ((package-name (car package-args)))
                    (dhnam/install-package-unless-installed package-name t)))))
            (dhnam/install-package-unless-installed package)))
	    (append dhnam/installable-domain-specific-packages
                dhnam/installable-machine-specific-packages)))
