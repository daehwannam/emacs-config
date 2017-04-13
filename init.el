
;;; load-path setting
;(add-to-list 'load-path "/home/dhnam/.emacs.d/package/")
(add-to-list 'load-path "~/.emacs.d/package/")
;(add-to-list 'load-path "~/.emacs.d/setting/")

;;; recursively add packages from a directory to load-path
;; http://stackoverflow.com/questions/7322246/adding-subdirectories-to-load-path
(let ((default-directory "~/.emacs.d/package/"))
  (normal-top-level-add-subdirs-to-load-path))

;;; load all init files
;; https://www.emacswiki.org/emacs/LoadingLispFiles
(require 'load-directory)
(load-directory "~/.emacs.d/init.d")
