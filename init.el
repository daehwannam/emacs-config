
(progn
  ;; adding to load-path recursively
  (package-initialize)

  (defun add-to-load-path-recursively (path)
    (add-to-list 'load-path path)

    ;; recursively add packages from a directory to load-path
    ;; http://stackoverflow.com/questions/7322246/adding-subdirectories-to-load-path
    (let ((default-directory path))
      (normal-top-level-add-subdirs-to-load-path)))

  ;; large files
  (add-to-load-path-recursively "~/.emacs.d/config/bin/")

  ;; archived packages
  (add-to-load-path-recursively "~/.emacs.d/config/package/")

  ;; some functions used to initialized emacs
  (add-to-load-path-recursively "~/.emacs.d/config/script/")

  ;; non-archived packages
  (add-to-load-path-recursively "~/.emacs.d/package/"))

(progn
  ;; base utility
  (require 'load-directory)
  (load-directory "~/.emacs.d/base/utility"))

(progn
  ;; load my-machine-config-setting.el
  (load "~/.emacs.d/config/machine-config.el"))

(progn
  ;; base setup
  (load "~/.emacs.d/base/package-init.el")
  (comment (load "~/.emacs.d/base/evil-setup.el"))
  (load "~/.emacs.d/base/key-chord-setup.el")
  (progn
    ;; hydra may be laggy and it's not compatible with key-chord
    (load "~/.emacs.d/base/hydra-setup.el"))
  (load "~/.emacs.d/base/key-config.el")
  (comment (load "~/.emacs.d/base/modalka-setup.el"))
  (comment (load "~/.emacs.d/base/modalka-simple-setup.el")))

(progn
  ;; bug fix
  (load "~/.emacs.d/bug-fix/emacs-28-bug-fix.el"))

(progn
  ;; common utility
  (load-directory "~/.emacs.d/config/utility"))

(progn
  ;; install packages
  (load "~/.emacs.d/base/package-install.el"))

(progn
  ;; load all init files
  ;; https://www.emacswiki.org/emacs/LoadingLispFiles
  ;; (require 'load-directory)
  (load-directory "~/.emacs.d/config/init")
  (load-directory "~/.emacs.d/config/init/last"))

(progn
  ;; starting page

  (progn
    ;; disable to load emacs startup screen
    (setq inhibit-splash-screen t))

  (switch-to-buffer "*scratch*"))
