

;; http://pragmaticemacs.com/emacs/dired-human-readable-sizes-and-sort-by-size/
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired-Updating.html
(require 'dired )
(setq dired-listing-switches "-alh")


;; https://www.emacswiki.org/emacs/DiredSortDirectoriesFirst
(defun mydired-sort ()
  "Sort dired listings with directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header 
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))

(defadvice dired-readin
  (after dired-after-updating-hook first () activate)
  "Sort dired listings with directories first before adding marks."
  (mydired-sort))


;; https://superuser.com/questions/397806/emacs-modify-quit-window-to-delete-buffer-not-just-bury-it
(defadvice quit-window (before quit-window-always-kill)
  "When running `quit-window', always kill the buffer."
  (ad-set-arg 0 t))
(ad-activate 'quit-window)

(fset 'dired-open-next
      "no\C-[-\C-xo")

(defun dired-open-next ()
  (interactive)
  (let ((window (selected-window)))
	(dired-next-line 1)
	(dired-find-file-other-window)
	(select-window window)))

(defun dired-open-prev ()
  (interactive)
  (let ((window (selected-window)))
	(dired-previous-line 1)
	(dired-find-file-other-window)
	(select-window window)))

(add-hook 'dired-mode-hook
	  (lambda () (local-set-key (kbd "M-n") #'dired-open-next)))

(add-hook 'dired-mode-hook
	  (lambda () (local-set-key (kbd "M-p") #'dired-open-prev)))

(fset 'dired-do-copy-into-other-window
   "\C-[xcopy-other-window-path-to-clipboard\C-mC\C-y\C-i") ;  C-i: Tab

(add-hook 'dired-mode-hook
	  (lambda () (local-set-key (kbd "M-C") #'dired-do-copy-into-other-window)))

(fset 'dired-do-rename-into-other-window
   "\C-[xcopy-other-window-path-to-clipboard\C-mR\C-y\C-i")

(add-hook 'dired-mode-hook
	  (lambda () (local-set-key (kbd "M-R") #'dired-do-rename-into-other-window)))

;;; functions to process pdf
(defun normalize-paper-name (str)
  (setq str (downcase str))
  (setq str (replace-regexp-in-string ":" "=" str))
  (setq str (replace-regexp-in-string "[ \n]" "_" str)))

(defun unnormalize-paper-name (str)
  (setq str (replace-regexp-in-string "=" ":" str))
  (setq str (replace-regexp-in-string "_" " " str)))

(defun message-normalized-paper-name (name)
  (interactive "sEnter paper name: ")
  (message "%s" (normalize-paper-name name)))

(defun dired-do-normalize-paper-name (&optional arg)
  (interactive "P")
  (let (new-file-paths original-point)
    (dolist (file-path (dired-get-marked-files nil arg))
      (let* ((dir-path (file-name-directory file-path))
	     (file-name (file-name-nondirectory file-path))
	     (new-file-path (concat dir-path (normalize-paper-name file-name))))
	(unless (equal file-path new-file-path)
	  (rename-file file-path new-file-path))
	(setq new-file-paths (cons new-file-path new-file-paths))))

    (setq original-point (point))
    (save-excursion
      (revert-buffer t)
      (setq original-point (point))
      (dolist (new-file-path new-file-paths)
	(beginning-of-buffer)
	(re-search-forward (file-name-nondirectory new-file-path))
	(dired-mark nil t)
	(end-of-buffer)
	))
    (revert-buffer)
    (goto-char original-point)
    ))

(defun dired-do-convert-pdf-to-txt (&optional arg)
  (interactive "P")
  (let (new-file-paths original-point)
    (dolist (file-path (dired-get-marked-files nil arg))
      (let* ((dir-path (file-name-directory file-path))
	     (file-name (file-name-nondirectory file-path))
	     (new-file-path (replace-regexp-in-string ".pdf" ".txt"
			     (concat dir-path (normalize-paper-name file-name)))))
	(unless (file-exists-p new-file-path)
	  (print file-path)
	  (print new-file-path)
	  (shell-command (concat "pdftotext" " " file-path " " new-file-path))))))
  (revert-buffer))

(defun grep-file (command-args)
  (interactive
   (progn
     (grep-compute-defaults)
     (let ((default (grep-default-command)))
       (list (read-shell-command "Run grep (like this): "
                                 (if current-prefix-arg default "grep --color -nH -i -m 1 -e "
)
                                 'grep-history
                                 (if current-prefix-arg nil default))))))

  ;; Setting process-setup-function makes exit-message-function work
  ;; even when async processes aren't supported.
  (compilation-start (if (and grep-use-null-device null-device)
			 (concat command-args " " null-device)
		       command-args)
		     'grep-mode))
