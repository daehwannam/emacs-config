

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

;; File path copy
;; https://stackoverflow.com/a/9414763
(defun copy-path-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "'%s'" filename))))

(defun copy-other-window-path-to-clipboard (count)
  "Copy the other window's path."
  (interactive "p")
  (let ((path (progn (other-window count)
		(let ((path default-directory))
		  (other-window (- count))
		  path))))
    (when path
      (kill-new path)
      (message "'%s'" path))))

(fset 'dired-do-copy-into-other-window
   "\C-[xcopy-other-window-path-to-clipboard\C-mC\C-y")

(add-hook 'dired-mode-hook
	  (lambda () (local-set-key (kbd "M-C") #'dired-do-copy-into-other-window)))

(fset 'dired-do-rename-into-other-window
   "\C-[xcopy-other-window-path-to-clipboard\C-mR\C-y")

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

(defun message-unnormalized-paper-name (name)
  (interactive "sEnter paper name: ")
  (message "%s" (unnormalize-paper-name name)))

(defun dired-mark-files-by-paths (file-paths)
  (let ((original-point (point)))
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

(defun dired-do-normalize-paper-name (&optional arg)
  (interactive "P")
  (let (new-file-paths)
    (dolist (file-path (dired-get-marked-files nil arg))
      (let* ((dir-path (file-name-directory file-path))
	     (file-name (file-name-nondirectory file-path))
	     (new-file-path (concat dir-path (normalize-paper-name file-name))))
	(unless (equal file-path new-file-path)
	  (rename-file file-path new-file-path))
	(setq new-file-paths (cons new-file-path new-file-paths))))
    (dired-mark-files-by-paths new-file-paths)))

(defun dired-do-convert-pdf-to-txt (&optional arg)
  (interactive "P")
  (let (new-file-paths)
    (dolist (file-path (dired-get-marked-files nil arg))
      (let* ((dir-path (file-name-directory file-path))
	     (file-name (file-name-nondirectory file-path))
	     (new-file-path (replace-regexp-in-string ".pdf" ".txt"
			     (concat dir-path (normalize-paper-name file-name)))))
	(unless (file-exists-p new-file-path)
	  (setq new-file-paths (cons new-file-path new-file-paths)) ; exclude existing files
	  (shell-command (concat "pdftotext" " " file-path " " new-file-path)))))
    (dired-unmark-all-marks)
    (dired-mark-files-by-paths new-file-paths)))


(defun xah-open-in-external-app (&optional @fname)
  "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference.

When called in emacs lisp, if @fname is given, open that.

URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2019-11-04"
  (interactive)
  (let* (
         ($file-list
          (if @fname
              (progn (list @fname))
            (if (string-equal major-mode "dired-mode")
                (dired-get-marked-files)
              (list (buffer-file-name)))))
         ($do-it-p (if (<= (length $file-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when $do-it-p
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda ($fpath)
           (w32-shell-execute "open" $fpath)) $file-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda ($fpath)
           (shell-command
            (concat "open " (shell-quote-argument $fpath))))  $file-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda ($fpath) (let ((process-connection-type nil))
                            (start-process "" nil "xdg-open" $fpath))) $file-list))))))

(add-hook 'dired-mode
	  (lambda () (local-set-key (kbd "C-c C-o") 'xah-open-in-external-app)))
