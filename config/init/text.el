

;; https://stackoverflow.com/questions/26478594/how-to-delete-empty-lines-in-a-file-by-emacs/26492924#26492924
(defun remove-empty-lines ()
  (interactive)
  (flush-lines "^[[:space:]]*$"))

;; (defun remove-trailing-white-space ()
;;   (interactive)
;;   (replace-regexp "[[:space:]]*$" ""))
;;
;; USE "delete-trailing-whitespace" instead
;; https://www.emacswiki.org/emacs/DeletingWhitespace#toc3

(defun shrink-empty-lines ()
  (interactive)
  (replace-regexp "^[[:space:]]*\n\\([[:space:]]*\n\\)+" "\n"))
(shrink-empty-lines)


;; https://stackoverflow.com/a/25886353
(defun toggle-camelcase-underscores ()
  "Toggle between camelcase and underscore notation for the symbol at point."
  (interactive)
  (save-excursion
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (start (car bounds))
           (end (cdr bounds))
           (currently-using-underscores-p (progn (goto-char start)
                                                 (re-search-forward "_" end t))))
      (if currently-using-underscores-p
          (progn
            (upcase-initials-region start end)
            (replace-string "_" "" nil start end)
            (downcase-region start (1+ start)))
        (replace-regexp "\\([A-Z]\\)" "_\\1" nil (1+ start) end)
        (downcase-region start (cdr (bounds-of-thing-at-point 'symbol)))))))

(global-set-key (kbd "M-_") 'toggle-camelcase-underscores)


(defun kill-ring-save-at-point ()
  "Copy the symbol at point."
  (interactive)
  (save-excursion
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (start (car bounds))
           (end (cdr bounds))
           (currently-using-underscores-p (progn (goto-char start)
                                                 (re-search-forward "_" end t))))
      (kill-ring-save start end)
      )))

(global-set-key (kbd "C-c M-w") 'kill-ring-save-at-point)


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
