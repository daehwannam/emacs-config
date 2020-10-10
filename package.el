;;; load emacs 24's package system. Add MELPA repository.
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
	  package-archive-list))
  )

;;; package initialization
;; (package-initialize)

;;; fetch the list of packages available
;;; https://stackoverflow.com/a/10093312
(unless package-archive-contents
  (package-refresh-contents))


;;; package install
;; https://realpython.com/blog/python/emacs-the-best-python-editor/
(defvar installable-domain-specific-packages) ; (defvar machine-domain-specific-packages some-value)
(setq installable-domain-specific-packages (machine-config-get 'installable-packages))
;; (setq installable-domain-specific-packages
;;   (cond
;;    ((string-equal machine-domain "default") '())
;;    ((string-equal machine-domain "basic") '(vlf magit))
;;    ((string-equal machine-domain "vbox") '(vlf auctex magit material-theme elpy flycheck py-autopep8 workgroups2 latex-preview-pane perspective counsel iedit wgrep which-key projectile flx-ido counsel-projectile eyebrowse persp-projectile paredit markdown-mode highlight))
;;    ;; ((string-equal machine-domain "engels") '(vlf magit elpy flycheck py-autopep8 wgrep which-key projectile flx-ido  eyebrowse))
;;    ((string-equal machine-domain "engels") '(vlf magit elpy flycheck py-autopep8 wgrep which-key projectile flx-ido counsel counsel-projectile eyebrowse sphinx-doc markdown-mode))
;;    ;; ((string-equal machine-domain "engels") '(vlf magit elpy py-autopep8))
;;    ((string-equal machine-domain "programming") '(elpy))
;;    ((string-equal machine-domain "ms-laptop") '(vlf magit))
;;    ((string-equal machine-domain "ms-desktop") '(vlf magit))
;;    ))

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
	installable-domain-specific-packages))
