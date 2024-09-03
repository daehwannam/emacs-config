
(progn
  ;; path of emacs config
  (defconst dhnam/emacs-root-dir (file-name-directory load-file-name)))

(progn
  ;; initialize dhnamlib
  (require 'dhnam-lib-init (concat dhnam/emacs-root-dir "dhnamlib/dhnam-lib-init.el")))

(progn
  (package-initialize)

  ;; large files
  (dhnam/add-to-load-path-recursively (concat dhnam/emacs-root-dir "config/bin/"))

  ;; archived packages
  (dhnam/add-to-load-path-recursively (concat dhnam/emacs-root-dir "config/archive/"))

  ;; other libraries
  (dhnam/add-to-load-path-recursively (concat dhnam/emacs-root-dir "config/library/"))

  ;; non-archived packages
  (dhnam/add-to-load-path-recursively (concat dhnam/emacs-root-dir "package/")))

(progn
  ;; define a function to load features in the "init" directory
  (define-localized-require dhnam/require-from-init "init-" (concat dhnam/emacs-root-dir "config/init/")))

(progn
  ;; load machine-config-setting.el
  (load (concat dhnam/emacs-root-dir "config/machine-config.el")))

(progn
  ;; theme
  (add-to-list 'custom-theme-load-path (concat dhnam/lib-root-dir "theme"))
  (let ((theme-style (dhnam/machine-config-get-first 'theme-style)))
    (when (or (eq theme-style 'dark) (eq theme-style nil))
      (load-theme 'dhnam-manoj-dark t))))

(progn
  ;; base setup
  (require 'dhnam-cmd-line-arg)
  (load (concat dhnam/emacs-root-dir "base/package-init.el"))
  (load (concat dhnam/emacs-root-dir "base/package-install.el"))
  (load (concat dhnam/emacs-root-dir "base/key-chord-setup.el"))
  (load (concat dhnam/emacs-root-dir "base/hydra-setup.el"))
  (load (concat dhnam/emacs-root-dir "base/key-config.el"))
  (comment (load (concat dhnam/emacs-root-dir "base/modalka-setup.el")))
  (comment (load (concat dhnam/emacs-root-dir "base/modalka-simple-setup.el"))))

(progn
  ;; bug fix
  (load (concat dhnam/emacs-root-dir "bug-fix/emacs-28-bug-fix.el"))
  (load (concat dhnam/emacs-root-dir "bug-fix/emacs-bug-fix.el")))

(progn
  (defvar dhnam/no-config-init-cmd-line-arg-passed
    (cmd-line-arg/register-then-get "--no-config-init" nil))

  (unless dhnam/no-config-init-cmd-line-arg-passed
    ;; load all init files
    (dhnam/require-directory-with-prefix (concat dhnam/emacs-root-dir "config/init") "init-")
    (dhnam/require-directory-with-prefix (concat dhnam/emacs-root-dir "config/init/last") "init-")))

(progn
  ;; starting page

  (progn
    ;; disable to load emacs startup screen
    (setq inhibit-splash-screen t))

  (switch-to-buffer "*scratch*"))
