
(progn
  (package-initialize)

  (defun dhnam/add-to-load-path (path)
    (add-to-list 'load-path path))

  (defun dhnam/add-to-load-path-recursively (path)
    ;; adding to load-path recursively
    (add-to-list 'load-path path)

    (let ((default-directory path))
      ;; recursively add packages from a directory to load-path
      ;; https://stackoverflow.com/a/7322812
      (normal-top-level-add-subdirs-to-load-path)))

  ;; large files
  (dhnam/add-to-load-path-recursively "~/.emacs.d/config/bin/")

  ;; archived packages
  (dhnam/add-to-load-path-recursively "~/.emacs.d/config/archive/")

  ;; my personal library
  (dhnam/add-to-load-path-recursively "~/.emacs.d/config/dhnamlib/")

  ;; other libraries
  (dhnam/add-to-load-path-recursively "~/.emacs.d/config/library/")

  ;; non-archived packages
  (dhnam/add-to-load-path-recursively "~/.emacs.d/package/"))

;; use functions to load files and require features
(require 'dhnam-file-loading)

(progn
  ;; define a function to load features in the "init" directory
  (define-localized-require dhnam/require-from-init "init-" "~/.emacs.d/config/init/"))

(progn
  ;; common utility
  (dhnam/require-directory "~/.emacs.d/config/dhnamlib/utility"))

(progn
  ;; load machine-config-setting.el
  (load "~/.emacs.d/config/machine-config.el"))

(progn
  ;; theme
  (add-to-list 'custom-theme-load-path "~/.emacs.d/config/init/theme-collection")
  (let ((theme-style (dhnam/machine-config-get-first 'theme-style)))
    (when (or (eq theme-style 'dark) (eq theme-style nil))
      (load-theme 'my-manoj-dark t))))

(progn
  ;; base setup
  (load "~/.emacs.d/base/cmd-line-arg.el")
  (load "~/.emacs.d/base/package-init.el")
  (load "~/.emacs.d/base/key-chord-setup.el")
  (load "~/.emacs.d/base/hydra-setup.el")
  (load "~/.emacs.d/base/key-config.el")
  (comment (load "~/.emacs.d/base/modalka-setup.el"))
  (comment (load "~/.emacs.d/base/modalka-simple-setup.el")))

(progn
  ;; bug fix
  (load "~/.emacs.d/bug-fix/emacs-28-bug-fix.el"))

(progn
  ;; install packages
  (load "~/.emacs.d/base/package-install.el"))

(progn
  (defvar dhnam/no-config-init-cmd-line-arg-passed
    (cmd-line-arg/register-then-get "--no-config-init" nil))

  (unless dhnam/no-config-init-cmd-line-arg-passed
    ;; load all init files
    (dhnam/require-directory-with-prefix "~/.emacs.d/config/init" "init-")
    (dhnam/require-directory-with-prefix "~/.emacs.d/config/init/last" "init-")))

(progn
  ;; starting page

  (progn
    ;; disable to load emacs startup screen
    (setq inhibit-splash-screen t))

  (switch-to-buffer "*scratch*"))
