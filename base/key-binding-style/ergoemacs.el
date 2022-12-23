
;;; ergoemacs-mode configuration
;;; https://github.com/ergoemacs/ergoemacs-mode#changing-your-own-bindings
;;;
;;; ergoemacs-mode wiki
;;; https://www.emacswiki.org/emacs/ErgoemacsMode
;;;
;;; overriding ergoemacs bindings
;;; https://github.com/ergoemacs/ergoemacs-mode/issues/147#issuecomment-36071299


(install-package-unless-installed 'ergoemacs-mode)

(progn
  (setq ergoemacs-theme nil)
  (setq ergoemacs-keyboard-layout "us")
  (require 'ergoemacs-mode)
  (ergoemacs-mode 1))
