
;; https://www.emacswiki.org/emacs/KeyboardMacros
;;
;; <Keyboard macro manual>
;;
;; `C-x (’ or <f3>– start defining a keyboard macro
;; `C-x )’ or <f4>– stop defining the keyboard macro
;; ‘M-x name-last-kbd-macro’ – Name the last-defined keyboard macro.
;; ‘M-x insert-kbd-macro’ – Insert a named keyboard macro at point.
;; (global-set-key (kbd "C-c a") 'my-macro)

(when (dhnam/machine-config-get-first 'ai-edu-key-binding)
   (fset 'find-and-region-_x_
	 "\C-s_X_\C-m\C-@\C-b\C-b\C-b\C-x\C-x")
   (add-hook 'python-mode-hook
	     (lambda () (local-set-key (kbd "C-c _") 'find-and-region-_x_))))

(provide 'dhnam-key-macro)
