
(when (fboundp 'god-mode)
  ;; https://github.com/chrisdone/god-mode
  (require 'god-mode)
  (global-set-key (kbd "M-'") 'god-mode-all) ; original binding was 'abbrev-prefix-mark

  (setq god-exempt-major-modes nil)
  (setq god-exempt-predicates nil)

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
  )

