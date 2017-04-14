
;;; load-path setting
;(add-to-list 'load-path "/home/dhnam/.emacs.d/package/")

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/config/package/")
;(add-to-list 'load-path "~/.emacs.d/setting/")

;;; recursively add packages from a directory to load-path
;; http://stackoverflow.com/questions/7322246/adding-subdirectories-to-load-path
(let ((default-directory "~/.emacs.d/config/package/"))
  (normal-top-level-add-subdirs-to-load-path))

(add-to-list 'load-path "~/.emacs.d/config/script/")
(let ((default-directory "~/.emacs.d/config/script/"))
  (normal-top-level-add-subdirs-to-load-path))

;;; load all init files
;; https://www.emacswiki.org/emacs/LoadingLispFiles
(require 'load-directory)
(load-directory "~/.emacs.d/config/init")
