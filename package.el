
(progn
  ;; load emacs 24's package system. Add MELPA repository.
  ;; http://ergoemacs.org/emacs/emacs_package_system.html

  (when (>= emacs-major-version 24)
    (require 'package)
    ;; (add-to-list
    ;;  'package-archives
    ;;  ;; '("melpa" . "http://stable.melpa.org/packages/") ; many packages won't show if using stable
    ;;  '("melpa" . "http://melpa.milkbox.net/packages/")
    ;;  ;; '("melpa" . "http://melpa.org/packages/")
    ;;  t)
    ;; (setq package-archives
    ;; 	'(("gnu" . "http://elpa.gnu.org/packages/")
    ;; 	  ("melpa" . "http://melpa.org/packages/")
    ;; 	  ("marmalade" . "https://marmalade-repo.org/packages/")
    ;; 	  ;; ("milkbox" . "http://melpa.milkbox.net/packages/") ; it doesn't work
    ;; 	  ("org" . "http://orgmode.org/elpa/") ; https://emacs.stackexchange.com/a/27599
    ;; 	  ))
    (let ((package-archive-list '(("melpa" . "http://melpa.org/packages/")
				  ("marmalade" . "https://marmalade-repo.org/packages/")
				  ("org" . "http://orgmode.org/elpa/") ; https://emacs.stackexchange.com/a/27599
				  )))
      (mapc #'(lambda (archive-pair) (add-to-list 'package-archives archive-pair))
	    package-archive-list)))

  ;; package initialization
  ;; (package-initialize)

  ;; fetch the list of packages available
  ;; https://stackoverflow.com/a/10093312
  (unless package-archive-contents
    (package-refresh-contents))

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
	    installable-domain-specific-packages))))

(progn
  ;; use-package initialization
  (require 'use-package)
  (defmacro use-existing-pkg (name &rest args)
    `(when (package-installed-p ',name)
       (use-package ,name ,@args)))

  (progn
    ;; change argument colors of use-existing-pkg
    (defconst use-existing-pkg-font-lock-keywords
      '(("(\\(use-existing-pkg\\)\\_>[ \t']*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
	 (1 font-lock-keyword-face)
	 (2 font-lock-constant-face nil t))))

    (font-lock-add-keywords 'emacs-lisp-mode use-existing-pkg-font-lock-keywords))

  ;; change indentation
  (function-put 'use-existing-pkg 'lisp-indent-function '1))
