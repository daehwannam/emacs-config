
(when (fboundp 'elscreen-start)
  (elscreen-start)
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(elscreen-tab-current-screen-face ((t (:background "brightcyan" :foreground "black"))))
   '(elscreen-tab-other-screen-face ((t (:background "color-75" :foreground "black" :underline t)))))

  ;; elscreen save configuration
  ;; https://stackoverflow.com/questions/22445670/save-and-restore-elscreen-tabs-and-split-frames
  (progn
    ;; http://stackoverflow.com/questions/803812/emacs-reopen-buffers-from-last-session-on-startup
    (defvar emacs-configuration-directory
      "~/.emacs.d/layout/"
      "The directory where the emacs configuration files are stored.")

    ;; (defvar elscreen-tab-configuration-store-filename
    ;;   (concat emacs-configuration-directory ".elscreen")
    ;;   "The file where the elscreen tab configuration is stored.")

    (defun elscreen-store (dir)
      "Store the elscreen tab configuration."
      (interactive
       (list (file-name-as-directory
	      (read-file-name "Save the layout into: " emacs-configuration-directory
			      (concat (file-name-as-directory emacs-configuration-directory) "default")
			      nil nil))	; it gets input in a similar way in 'make-directory'.
	     ))
      (unless (file-exists-p dir)
	(make-directory dir))
      (if (desktop-save dir)
	  (with-temp-file (concat dir ".elscreen")
            (insert (prin1-to-string (elscreen-get-screen-to-name-alist))))))

    (defun elscreen-restore (dir)
      "Restore the elscreen tab configuration."
      (interactive
       (list (file-name-as-directory
	      (read-file-name "Load the layout into: " emacs-configuration-directory
			      (concat (file-name-as-directory emacs-configuration-directory) "default")
			      t nil))	; it gets input in a similar way in 'make-directory'.
	     ))
      (if (desktop-read dir)
	  (let ((screens (reverse
			  (read
			   (with-temp-buffer
			     (insert-file-contents (concat dir ".elscreen"))
			     (buffer-string))))))
            (while screens
	      (setq screen (car (car screens)))
	      (setq buffers (split-string (cdr (car screens)) ":"))
	      (if (eq screen 0)
		  (switch-to-buffer (car buffers))
		(elscreen-find-and-goto-by-buffer (car buffers) t t))
	      (while (cdr buffers)
		(switch-to-buffer-other-window (car (cdr buffers)))
		(setq buffers (cdr buffers)))
	      (setq screens (cdr screens))))))

    (defun elscreen-store-without-desktop (dir)
      "Store the elscreen tab configuration."
      (interactive
       (list (file-name-as-directory
	      (read-file-name "Save the layout into: " emacs-configuration-directory
			      (concat (file-name-as-directory emacs-configuration-directory) "default")
			      nil nil))	; it gets input in a similar way in 'make-directory'.
	     ))
      (unless (file-exists-p dir)
	(make-directory dir))
      (with-temp-file (concat dir ".elscreen")
	(insert (prin1-to-string (elscreen-get-screen-to-name-alist)))))

    (defun elscreen-restore-without-desktop (dir)
      "Restore the elscreen tab configuration."
      (interactive
       (list (file-name-as-directory
	      (read-file-name "Load the layout into: " emacs-configuration-directory
			      (concat (file-name-as-directory emacs-configuration-directory) "default")
			      t nil))	; it gets input in a similar way in 'make-directory'.
	     ))
      (let ((screens (reverse
		      (read
		       (with-temp-buffer
			 (insert-file-contents (concat dir ".elscreen"))
			 (buffer-string))))))
	(while screens
	  (setq screen (car (car screens)))
	  (setq buffers (split-string (cdr (car screens)) ":"))
	  (if (eq screen 0)
	      (switch-to-buffer (car buffers))
	    (elscreen-find-and-goto-by-buffer (car buffers) t t))
	  (while (cdr buffers)
	    (switch-to-buffer-other-window (car (cdr buffers)))
	    (setq buffers (cdr buffers)))
	  (setq screens (cdr screens))))))
  )
