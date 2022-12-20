
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
  (comment
   (unless package-archive-contents
     (package-refresh-contents))))

(progn
  (defvar package-content-refreshed nil)
  (defun install-package-unless-installed (package)
    (unless (package-installed-p package)
      (unless package-content-refreshed
	    ;; https://magit.vc/manual/magit/Installing-from-Melpa.html
	    (package-refresh-contents)
	    (setq package-content-refreshed t))
      (package-install package))))

(progn
  (install-package-unless-installed 'use-package)

  ;; use-package initialization
  (require 'use-package)
  (defmacro use-existing-pkg (name &rest args)
    `(when (package-installed-p ',name)
       (use-package ,name ,@args)))

  (progn
    ;; change argument colors of use-existing-pkg
    (defconst use-existing-pkg-font-lock-keywords
      ;; It's the same definition with `use-package-font-lock-keywords'
      '(("(\\(use-existing-pkg\\)\\_>[ \t']*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
	     (1 font-lock-keyword-face)
	     (2 font-lock-constant-face nil t))))

    (font-lock-add-keywords 'emacs-lisp-mode use-existing-pkg-font-lock-keywords))

  (progn
    ;; change indentation
    (function-put 'use-package 'lisp-indent-function '1)
    (function-put 'use-existing-pkg 'lisp-indent-function '1)))

(progn
  (install-package-unless-installed 'quelpa)
  (setq quelpa-update-melpa-p nil)  ; disabling auto-updating
  )
