
(when (fboundp 'god-mode)
  ;; https://github.com/chrisdone/god-mode
  (require 'god-mode)
  (global-set-key (kbd "M-'") 'god-mode-all) ; original binding was 'abbrev-prefix-mark

  ;; enable god-mode-all in all modes
  ;; (setq god-exempt-major-modes nil)
  ;; (setq god-exempt-predicates nil)

  (add-to-list 'god-exempt-major-modes 'dired-mode)  ;; god-mode disabled mode list

  ;; https://emacs.stackexchange.com/a/38318
  (defun god-mode-indicator ()
    (cond (god-local-mode
	   (progn
	     (set-face-background 'mode-line "blue4")
	     (set-face-foreground 'mode-line "gray")
	     (set-face-background 'mode-line-inactive "gray30")
	     (set-face-foreground 'mode-line-inactive "blue")))
	  (t
	   (progn
	     (set-face-background 'mode-line-inactive "gray30")
	     (set-face-foreground 'mode-line-inactive "gray80")
	     (set-face-background 'mode-line "gray75")
	     (set-face-foreground 'mode-line "black")))))

  (add-hook 'god-mode-enabled-hook 'god-mode-indicator)
  (add-hook 'god-mode-disabled-hook 'god-mode-indicator)

  (defun c/god-mode-update-cursor ()
    (let ((limited-colors-p (> 257 (length (defined-colors)))))
      (cond (god-local-mode (progn
			      (set-face-background 'mode-line (if limited-colors-p "white" "#e9e2cb"))
			      (set-face-background 'mode-line-inactive (if limited-colors-p "white" "#e9e2cb"))))
	    (t (progn
		 (set-face-background 'mode-line (if limited-colors-p "black" "#0a2832"))
		 (set-face-background 'mode-line-inactive (if limited-colors-p "black" "#0a2832")))))))

  ;; Key bindings
  (global-set-key (kbd "C-x C-1") 'delete-other-windows)
  (global-set-key (kbd "C-x C-2") 'split-window-below)
  (global-set-key (kbd "C-x C-3") 'split-window-right)
  (global-set-key (kbd "C-x C-0") 'delete-window)

  (require 'god-mode-isearch)
  (define-key isearch-mode-map (kbd "M-'") 'god-mode-isearch-activate)
  (define-key god-mode-isearch-map (kbd "M-'") 'god-mode-isearch-disable)

  (define-key god-local-mode-map (kbd ".") 'repeat)
  )
