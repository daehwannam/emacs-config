
(progn
  ;; load emacs 24's package system. Add MELPA repository.
  ;; http://ergoemacs.org/emacs/emacs_package_system.html

  (when (>= emacs-major-version 24)
    (require 'package)
    (let ((package-archive-list '(("melpa" . "https://melpa.org/packages/")
				   ; https://emacs.stackexchange.com/a/27599
				  )))
      (comment
       ;; skipped repositories
       ("marmalade" . "https://marmalade-repo.org/packages/")
       ("org" . "https://orgmode.org/elpa/"))
	       
      (mapc #'(lambda (archive-pair) (add-to-list 'package-archives archive-pair))
	    package-archive-list)))

  (comment
   ;; package initialization
   ;; Warning (package): Unnecessary call to ‘package-initialize’ in init file
   (package-initialize))

  ;; fetch the list of packages available
  ;; https://stackoverflow.com/a/10093312
  (unless package-archive-contents
    (package-refresh-contents)))

(progn
  (unless (package-installed-p 'use-package)
    (package-install 'use-package))

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
