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

;;; base utility
(require 'load-directory)
(load-directory "~/.emacs.d/base/utility")

;;; load my-machine-config-setting.el
(load "~/.emacs.d/config/machine-config.el")

;; base setup
(load "~/.emacs.d/base/package-init.el")
(comment (load "~/.emacs.d/base/evil-setup.el"))
(load "~/.emacs.d/base/key-chord-setup.el")
(progn
  ;; hydra may be laggy and it's not compatible with key-chord
  (load "~/.emacs.d/base/hydra-setup.el"))
(load "~/.emacs.d/base/key-config.el")
(comment (load "~/.emacs.d/base/modalka-setup.el"))
(load "~/.emacs.d/base/modalka-simple-setup.el")

;;; common utility
(load-directory "~/.emacs.d/config/utility")

;; install packages
(load "~/.emacs.d/base/package-install.el")

;;; load all init files
;; https://www.emacswiki.org/emacs/LoadingLispFiles
;; (require 'load-directory)
(load-directory "~/.emacs.d/config/init")
