
(when (fboundp 'tab-bar-mode)
  (require 'make-repeatable-command)
  (defun tab-bar-move-tab-reverse (&optional arg)
    (interactive "p")
    (tab-bar-move-tab (- (or arg 1))))

  (progn
    ;; advice for tab-next and tab-previous
    (defun tab-bar-switch-to-next-tab-advice (orig-fun &rest args)
      (apply orig-fun args)
      (progn
	;; to fix the problem of disappeared text with vertical
	;; windows when moving a cursor vertically after tab-next or
	;; tab-previous in emacs terminal mode
	;;
	;; https://emacs.stackexchange.com/a/29226
	(let ((seconds 0.01))
	  ;; delayed command execution
	  (run-with-timer seconds nil #'force-mode-line-update 't))
	(comment (force-mode-line-update t))
	(comment
	 (dotimes (number (count-windows))
	   (other-window 1)))))

    (advice-add 'tab-bar-switch-to-next-tab :around #'tab-bar-switch-to-next-tab-advice))
    
  (progn
    (define-key tab-prefix-map "8" 'tab-new)
    (define-key tab-prefix-map "9" 'tab-close-other)
    (define-key tab-prefix-map "o" (make-repeatable-command 'tab-next))
    (define-key tab-prefix-map "O" (make-repeatable-command 'tab-previous))
    (define-key tab-prefix-map "m" (make-repeatable-command 'tab-bar-move-tab))
    (define-key tab-prefix-map "M" (make-repeatable-command 'tab-bar-move-tab-reverse)))

  ;; Default tab-bar-mode key map
  ;;
  ;; (comment
  ;;  (define-key tab-prefix-map "2" 'tab-new)
  ;;  (define-key tab-prefix-map "1" 'tab-close-other)
  ;;  (define-key tab-prefix-map "0" 'tab-close)
  ;;  (define-key tab-prefix-map "o" 'tab-next)
  ;;  (define-key tab-prefix-map "m" 'tab-move)
  ;;  (define-key tab-prefix-map "r" 'tab-rename)
  ;;  (define-key tab-prefix-map "\r" 'tab-bar-select-tab-by-name)
  ;;  (define-key tab-prefix-map "b" 'switch-to-buffer-other-tab)
  ;;  (define-key tab-prefix-map "f" 'find-file-other-tab)
  ;;  (define-key tab-prefix-map "\C-f" 'find-file-other-tab))

  (fset 'tab-prefix-map tab-prefix-map)
  (define-key global-map (kbd "C-z") 'tab-prefix-map)

  (tab-bar-mode))
