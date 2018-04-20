
(when (require 'workgroups2 nil t)

  ;; Your settings here
  (setq wg-session-load-on-start nil)    ; default: (not (daemonp))

  ;; Change prefix key (before activating WG)
  (setq wg-prefix-key (kbd "C-c z"))

  ;; Change workgroups session file
  (setq wg-session-file "~/.emacs.d/layout/workgroups")

  ;; Default save directory
  (defvar wg-save-directory
    "~/.emacs.d/layout/"
    "The directory where the workgroup configuration files are stored.")

  ;; Load and save functions
  (defun wg-save-session-to (filename)
    "Store the elscreen tab configuration."
    (interactive
     (list (read-file-name "Save the workgroups into: " wg-save-directory
			   (concat (file-name-as-directory wg-save-directory) "default")
			   nil nil))	; it gets input in a similar way in 'make-directory'.
	   )
    (wg-save-session-as filename))

  (defun wg-load-session (filename)
    "Reload current workgroups session."
    (interactive
     (list (read-file-name "Load workgroups from: " wg-save-directory
			   (concat (file-name-as-directory wg-save-directory) "default")
			   t nil)	; it gets input in a similar way in 'make-directory'.
	   ))
    (let ((exists (file-exists-p filename)))
      (condition-case err
	  (wg-open-session filename)
	(progn
	  (wg-create-first-wg)
	  (message "Error loading session-file: %s" err))))
    ;; TODO: print what exactly happened
    (wg-create-first-wg))

  ;; Set your own keyboard shortcuts to reload/save/switch WGs:
  ;; "s" == "Super" or "Win"-key, "S" == Shift, "C" == Control
  (global-set-key (kbd "C-c F") 'wg-load-session)
  (global-set-key (kbd "C-c S") 'wg-save-session-to)
  (global-set-key (kbd "C-c o") (make-repeatable-command 'wg-switch-to-workgroup-right))
  (global-set-key (kbd "C-c O") (make-repeatable-command 'wg-switch-to-workgroup-left))
  (global-set-key (kbd "C-c 2") 'wg-create-workgroup)
  (global-set-key (kbd "C-c 2") 'wg-create-workgroup)
  (global-set-key (kbd "C-c 0") 'wg-kill-workgroup)
  (global-set-key (kbd "C-c 1") 'wg-delete-other-workgroups)

  ;; Enable workgroups
  (workgroups-mode 1)			; should be last
  (wg-create-workgroup "wg0")

  ;; To disable auto-save when emacs exits, change wg-emacs-exit-save-behavior
  ;; ex) (defcustom wg-emacs-exit-save-behavior 'nothing ...)
  )
