
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
  (dhnam/add-to-load-path-recursively "~/.emacs.d/config/package/")

  ;; some functions used to initialize emacs
  (dhnam/add-to-load-path-recursively "~/.emacs.d/config/script/")

  ;; utility
  (dhnam/add-to-load-path-recursively "~/.emacs.d/config/utility/")

  ;; my library
  (dhnam/add-to-load-path-recursively "~/.emacs.d/config/library/")

  ;; initialization
  ;; (dhnam/add-to-load-path "~/.emacs.d/config/init/")
  ;; (dhnam/add-to-load-path "~/.emacs.d/config/init/last/")

  ;; non-archived packages
  (dhnam/add-to-load-path-recursively "~/.emacs.d/package/"))

;; use functions to load files and require features
(require 'dhnam-file-loading)

(progn
  ;; utility
  (dhnam/require-directory "~/.emacs.d/config/utility"))

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
  (progn
    ;; hydra is not compatible with key-chord
    (load "~/.emacs.d/base/hydra-setup.el"))
  (load "~/.emacs.d/base/key-config.el")
  (comment (load "~/.emacs.d/base/modalka-setup.el"))
  (comment (load "~/.emacs.d/base/modalka-simple-setup.el")))

(progn
  ;; bug fix
  (load "~/.emacs.d/bug-fix/emacs-28-bug-fix.el"))

(progn
  ;; common utility
  (dhnam/load-directory "~/.emacs.d/config/utility"))

(progn
  ;; install packages
  (load "~/.emacs.d/base/package-install.el"))

(progn
  (defvar dhnam/no-config-init-cmd-line-arg-passed
    (cmd-line-arg/register-then-get "--no-config-init" nil))

  (unless dhnam/no-config-init-cmd-line-arg-passed
    ;; load all init files
    (dhnam/require-directory-with-prefix "~/.emacs.d/config/init" "dhnam-")
    (dhnam/require-directory-with-prefix "~/.emacs.d/config/init/last" "dhnam-")))

(progn
  ;; starting page

  (progn
    ;; disable to load emacs startup screen
    (setq inhibit-splash-screen t))

  (switch-to-buffer "*scratch*"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(org cmake-mode yaml-mode which-key-posframe wgrep vterm volume vlf vertico-posframe valign use-package sphinx-doc realgud quelpa pyvenv poporg pdfgrep pdf-tools paredit org-fragtog orderless multi-term mini-frame markdown-mode marginalia magit lice latex-preview-pane langtool key-chord jsonnet-mode ivy-posframe hydra hy-mode highlight-indentation hide-mode-line exwm expand-region embark-consult ein eglot define-word ctrlf csv-mode counsel company clipetty biblio auctex async)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
