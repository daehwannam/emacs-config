
;;; poporg mode
;;; https://github.com/QBobWatson/poporg

(when (package-installed-p 'poporg)
  ;; (require 'poporg nil t)
  (autoload 'poporg-dwim "poporg" nil t)

  (progn
    ;; common
    (progn
      (defun poporg-kill-buffer-exit ()
	(interactive)
	(when (yes-or-no-p "Really abandon this edit? ")
	  (poporg-kill-buffer-routine))))
    (comment
     (require 'poporg nil t)
     (define-key poporg-mode-map (kbd "C-x k") #'poporg-kill-buffer-exit))

    (use-existing-pkg poporg
      :bind (:map poporg-mode-map
		  ("C-x k" . poporg-kill-buffer-exit))))

  (progn
    ;; org-mode
    (global-set-key (kbd "C-c / /") 'poporg-dwim))

  (progn
    ;; rst-mode
    (defun pop-rst-dwim ()
      (interactive)
      (poporg-dwim)
      (rst-mode)
      (poporg-mode +1))

    (global-set-key (kbd "C-c / r") 'pop-rst-dwim)))
  
