
;;; ergoemacs-mode configuration
;;; https://github.com/ergoemacs/ergoemacs-mode#changing-your-own-bindings

(install-package-unless-installed 'ergoemacs-mode)

(progn
  (setq ergoemacs-theme nil)
  (setq ergoemacs-keyboard-layout "us")
  (require 'ergoemacs-mode)
  (ergoemacs-mode 1))
