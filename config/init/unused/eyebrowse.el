
(when (fboundp 'eyebrowse-mode)
  ;; (require 'dhnam-make-repeatable-command)
  (setq eyebrowse-keymap-prefix (kbd "C-z"))
  (setq eyebrowse-wrap-around t)

  ;; (setq eyebrowse-prev-window-config (make-repeatable-command 'eyebrowse-prev-window-config))
  ;; (setq eyebrowse-next-window-config (make-repeatable-command 'eyebrowse-next-window-config))

  (eyebrowse-mode t)

  (defun eyebrowse-prev-window-config-fixed (count)
    "Modified version of eyebrowse-prev-window-config to fix the error of line removal"
    (interactive "P")
    (eyebrowse-prev-window-config count)
    (other-window 1)
    (other-window -1))

  (defun eyebrowse-next-window-config-fixed (count)
    "Modified version of eyebrowse-next-window-config to fix the error of line removal"
    (interactive "P")
    (eyebrowse-next-window-config count)
    (other-window 1)
    (other-window -1))

  (defun eyebrowse-close-all-but-current ()
    (interactive)
    (let ((configs (eyebrowse--get 'window-configs))
	  (current (eyebrowse--get 'current-slot)))
      (dolist (config configs)
	(when (not (eq current (car config)))
	  (eyebrowse--delete-window-config (car config))))))

  (custom-set-faces
    '(eyebrowse-mode-line-active ((t (:inherit mode-line-emphasis :foreground "firebrick")))))

  (comment
   (custom-set-faces
    '(eyebrowse-mode-line-active ((t (:inherit mode-line-emphasis :foreground "color-160"))))
    ;; below config changes mode-line colors
    '(mode-line-buffer-id ((t (:background "grey65" :foreground "firebrick" :weight bold :height 0.9))))))

  (progn
    (defun flash-mode-line-by-inversion ()
      (invert-face 'mode-line)
      (run-with-timer 0.1 nil #'invert-face 'mode-line))

    (defun multiple-flash-mode-line-by-inversion ()
      ;; https://www.emacswiki.org/emacs/AlarmBell
      (let ((flash-sec (/ 1.0 20)))
	(invert-face 'mode-line)
	(run-with-timer flash-sec nil #'invert-face 'mode-line)
	(run-with-timer (* 2 flash-sec) nil #'invert-face 'mode-line)
	(run-with-timer (* 3 flash-sec) nil #'invert-face 'mode-line)
	(run-with-timer (* 4 flash-sec) nil #'invert-face 'mode-line)
	(run-with-timer (* 5 flash-sec) nil #'invert-face 'mode-line)))

    (defun eyebrowse-create-window-config-advice (orig-fun &rest args)
      (apply orig-fun args)
      (delete-other-windows)
      (flash-mode-line-by-inversion))

    (advice-add 'eyebrowse-create-window-config :around #'eyebrowse-create-window-config-advice)

    (defun eyebrowse-close-all-but-current-advice (orig-fun &rest args)
      (apply orig-fun args)
      (multiple-flash-mode-line-by-inversion))

    (advice-add 'eyebrowse-close-all-but-current :around #'eyebrowse-close-all-but-current-advice))

  (progn
    (progn
      ;; (define-key eyebrowse-mode-map (kbd "C-z c") 'eyebrowse-create-window-config)
      ;; (define-key eyebrowse-mode-map (kbd "C-z O") (make-repeatable-command 'eyebrowse-prev-window-config))
      ;; (define-key eyebrowse-mode-map (kbd "C-z o") (make-repeatable-command 'eyebrowse-next-window-config))

      (define-key eyebrowse-mode-map (kbd "C-z 2") 'eyebrowse-create-window-config)
      ;; (define-key eyebrowse-mode-map (kbd "C-z O") (make-repeatable-command 'eyebrowse-prev-window-config))
      ;; (define-key eyebrowse-mode-map (kbd "C-z o") (make-repeatable-command 'eyebrowse-next-window-config))

      (define-key eyebrowse-mode-map (kbd "C-z O") (make-repeatable-command 'eyebrowse-prev-window-config-fixed))
      (define-key eyebrowse-mode-map (kbd "C-z o") (make-repeatable-command 'eyebrowse-next-window-config-fixed))

      ;; (define-key eyebrowse-mode-map (kbd "C-z k") 'eyebrowse-close-window-config)
      ;; (define-key eyebrowse-mode-map (kbd "C-z K") 'eyebrowse-close-all-but-current)
      (define-key eyebrowse-mode-map (kbd "C-z 0") 'eyebrowse-close-window-config)
      (define-key eyebrowse-mode-map (kbd "C-z 1") 'eyebrowse-close-all-but-current))

    (progn
      (define-key eyebrowse-mode-map (kbd "C-z 9") 'eyebrowse-close-all-but-current)
      (define-key eyebrowse-mode-map (kbd "C-z 8") 'eyebrowse-create-window-config))

    (comment
      (key-chord-define eyebrowse-mode-map "l4" 'eyebrowse-close-window-config)
      (key-chord-define eyebrowse-mode-map "l1" 'eyebrowse-close-all-but-current)
      (key-chord-define eyebrowse-mode-map "l2" 'eyebrowse-create-window-config)
      (progn
       (key-chord-define eyebrowse-mode-map "lx" 'eyebrowse-next-window-config-fixed)
       (key-chord-define eyebrowse-mode-map "lc" 'eyebrowse-prev-window-config-fixed))
      (comment
       (key-chord-define eyebrowse-mode-map "lf" 'eyebrowse-next-window-config-fixed)
       (key-chord-define eyebrowse-mode-map "ld" 'eyebrowse-prev-window-config-fixed)))))


;; Key bindings
;; 
;; (define-key prefix-map (kbd "<") 'eyebrowse-prev-window-config)
;; (define-key prefix-map (kbd ">") 'eyebrowse-next-window-config)
;; (define-key prefix-map (kbd "'") 'eyebrowse-last-window-config)
;; (define-key prefix-map (kbd "\"") 'eyebrowse-close-window-config)
;; (define-key prefix-map (kbd ",") 'eyebrowse-rename-window-config)
;; (define-key prefix-map (kbd ".") 'eyebrowse-switch-to-window-config)
;; (define-key prefix-map (kbd "0") 'eyebrowse-switch-to-window-config-0)
;; (define-key prefix-map (kbd "1") 'eyebrowse-switch-to-window-config-1)
;; (define-key prefix-map (kbd "2") 'eyebrowse-switch-to-window-config-2)
;; (define-key prefix-map (kbd "3") 'eyebrowse-switch-to-window-config-3)
;; (define-key prefix-map (kbd "4") 'eyebrowse-switch-to-window-config-4)
;; (define-key prefix-map (kbd "5") 'eyebrowse-switch-to-window-config-5)
;; (define-key prefix-map (kbd "6") 'eyebrowse-switch-to-window-config-6)
;; (define-key prefix-map (kbd "7") 'eyebrowse-switch-to-window-config-7)
;; (define-key prefix-map (kbd "8") 'eyebrowse-switch-to-window-config-8)
;; (define-key prefix-map (kbd "9") 'eyebrowse-switch-to-window-config-9)
;; (define-key prefix-map (kbd "c") 'eyebrowse-create-window-config)
;; (define-key prefix-map (kbd "C-c") 'eyebrowse-create-window-config)
