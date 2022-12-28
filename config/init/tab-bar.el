
(when (fboundp 'tab-bar-mode)
  (progn
    ;; hide tabs
    (setq tab-bar-show nil))

  (defun dhnam/tab-bar-move-tab-reverse (&optional arg)
    (interactive "p")
    (tab-bar-move-tab (- (or arg 1))))

  (progn
    (require 'dhnam-make-repeatable-command)
    ;; (dhnam/define-self-insert-commands-unless-bound tab-prefix-map "ii")
    (define-key tab-prefix-map "8" 'tab-new)
    (define-key tab-prefix-map "9" 'tab-close-other)
    (define-key tab-prefix-map "o" (make-repeatable-command 'tab-next))
    (define-key tab-prefix-map "O" (make-repeatable-command 'tab-previous))
    (define-key tab-prefix-map "m" (make-repeatable-command 'tab-bar-move-tab))
    (define-key tab-prefix-map "M" (make-repeatable-command 'dhnam/tab-bar-move-tab-reverse))
    (define-key tab-prefix-map "a" 'self-insert-command)

    (fset 'tab-prefix-map tab-prefix-map)
    (define-key global-map (kbd "C-z") 'tab-prefix-map)
    (progn
      (comment (dhnam/define-self-insert-commands-unless-bound tab-prefix-map "ww"))
      (key-chord-define-global "qe" 'tab-prefix-map)))

  (progn
    ;; advice for tab-next and tab-previous
    (defun dhnam/tab-bar-redisplay-advice (orig-fun &rest args)
      (apply orig-fun args)
      (progn
	;; to fix the problem of disappeared text with vertical
	;; windows when moving a cursor vertically after tab-next or
	;; tab-previous in emacs terminal mode
	;;
	;; https://emacs.stackexchange.com/a/29226
	(progn
	  (let ((seconds 0.01))
	    ;; delayed command execution
	    (run-with-timer seconds nil #'force-mode-line-update 't)
	    (comment (run-with-timer seconds nil
				     (lambda () (dotimes (number (count-windows)) (other-window 1)))))))
	(comment (force-mode-line-update t))
	(comment
	 (dotimes (number (count-windows))
	   (other-window 1)))))

    (comment (dolist (tab-bar-func '(tab-new tab-close tab-close-other tab-next tab-previous)) ...))
    (comment (dolist (tab-bar-func '(tab-bar-new-tab tab-bar-close-tab tab-bar-close-other-tabs tab-bar-switch-to-next-tab))))
    (dolist (tab-bar-func '(tab-new tab-close tab-close-other tab-next tab-previous))
      (advice-add tab-bar-func :around #'dhnam/tab-bar-redisplay-advice)))

  (comment
   (let ((hydra-def
	  (defhydra dhnam/hydra-tab-bar ()
	    "tab-bar"

	    ;; from default key bindings
            ("2" tab-new)
            ("1" tab-close-other)
            ("0" tab-close)
            ("o" tab-next)
            ("m" tab-move)
            ("r" tab-rename)
            ("<RET>" tab-bar-select-tab-by-name)
            ("b" switch-to-buffer-other-tab)
            ("f" find-file-other-tab)
            ("C-f" find-file-other-tab)

	    ;; by bindings
	    ("8" tab-new)
	    ("9" tab-close-other)
	    ("O" tab-previous)
	    ("M" dhnam/tab-bar-move-tab-reverse))))
     (global-set-key (kbd "C-z") hydra-def)))

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

  (comment (tab-bar-mode)))

(provide 'dhnam-tab-bar)
