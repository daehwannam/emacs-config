;;; load theme.el
;; (load "~/.emacs.d/config/init/theme.el")

;;; load-path setting
(package-initialize)

(defun add-to-load-path-recursively (path)
  (add-to-list 'load-path path)

  ;; recursively add packages from a directory to load-path
  ;; http://stackoverflow.com/questions/7322246/adding-subdirectories-to-load-path
  (let ((default-directory path))
    (normal-top-level-add-subdirs-to-load-path)))

(add-to-load-path-recursively "~/.emacs.d/config/bin/")
(add-to-load-path-recursively "~/.emacs.d/config/package/")
(add-to-load-path-recursively "~/.emacs.d/config/script/")

;;; load useful functions
(require 'load-directory)
(load-directory "~/.emacs.d/config/function")

;;; load my-machine-config-setting.el
(load "~/.emacs.d/config/machine-config.el")

;;; package setting
(load "~/.emacs.d/package.el")

;;; load all init files
;; https://www.emacswiki.org/emacs/LoadingLispFiles
;; (require 'load-directory)
(load-directory "~/.emacs.d/config/init")
