

(require 'dired )

(progn
  ;; http://pragmaticemacs.com/emacs/dired-human-readable-sizes-and-sort-by-size/
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired-Updating.html
  (setq dired-listing-switches "-alh"))


(progn
  ;; https://www.emacswiki.org/emacs/DiredSortDirectoriesFirst
  (defun dhnam/dired-sort ()
    "Sort dired listings with directories first."
    (save-excursion
      (let (buffer-read-only)
        (forward-line 2) ;; beyond dir. header
        (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
      (set-buffer-modified-p nil)))

  (defadvice dired-readin
      (after dired-after-updating-hook first () activate)
    "Sort dired listings with directories first before adding marks."
    (dhnam/dired-sort)))

(comment
  ;; (This doesn't work)
  ;;
  ;; Change file name orders similarly to MS-Windows
  ;; https://emacs.stackexchange.com/a/50785
  (comment
    (custom-set-variables '(ls-lisp-UCA-like-collation nil)))
  (setq ls-lisp-UCA-like-collation nil))


;; https://superuser.com/questions/397806/emacs-modify-quit-window-to-delete-buffer-not-just-bury-it
(defadvice quit-window (before quit-window-always-kill)
  "When running `quit-window', always kill the buffer."
  (ad-set-arg 0 t))
(ad-activate 'quit-window)

(comment
  (fset 'dhnam/dired-open-next
        "no\C-[-\C-xo"))


(add-hook 'dired-mode-hook
	  (lambda () (local-set-key (kbd "M-n") #'dhnam/dired-open-next)))
(add-hook 'dired-mode-hook
	  (lambda () (local-set-key (kbd "M-p") #'dhnam/dired-open-prev)))

(progn
  ;; File path copy
  ;; https://stackoverflow.com/a/9414763

  (key-chord-define-global "wp" 'dhnam/kill-path-to-clipboard)
  (key-chord-define-global "wn" 'dhnam/kill-file-name-to-clipboard)
  (key-chord-define-global "wb" 'dhnam/kill-buffer-name-to-clipboard))

(fset 'dired-do-copy-into-other-window
   "\C-[xdhnam/kill-other-window-path-to-clipboard\C-mC\C-y")

(add-hook 'dired-mode-hook
	  (lambda () (local-set-key (kbd "M-C") #'dired-do-copy-into-other-window)))

(fset 'dired-do-rename-into-other-window
   "\C-[xdhnam/kill-other-window-path-to-clipboard\C-mR\C-y")

(add-hook 'dired-mode-hook
	  (lambda () (local-set-key (kbd "M-R") #'dired-do-rename-into-other-window)))

(progn
  ;;; functions to process pdf
  (defun dhnam/normalize-paper-name (str)
    (setq str (downcase str))
    (setq str (replace-regexp-in-string ":" "=" str))
    (setq str (replace-regexp-in-string "[ \n]" "_" str)))

  (defun dhnam/unnormalize-paper-name (str)
    (setq str (replace-regexp-in-string "=" ":" str))
    (setq str (replace-regexp-in-string "_" " " str)))

  (defun dhnam/message-normalized-paper-name (name)
    (interactive "sEnter paper name: ")
    (message "%s" (dhnam/normalize-paper-name name)))

  (defun dhnam/message-unnormalized-paper-name (name)
    (interactive "sEnter paper name: ")
    (message "%s" (dhnam/unnormalize-paper-name name)))

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

  (defun dhnam/dired-do-normalize-paper-name (&optional arg)
    (interactive "P")
    (let (new-file-paths)
      (dolist (file-path (dired-get-marked-files nil arg))
        (let* ((dir-path (file-name-directory file-path))
	           (file-name (file-name-nondirectory file-path))
	           (new-file-path (concat dir-path (dhnam/normalize-paper-name file-name))))
	      (unless (equal file-path new-file-path)
	        (rename-file file-path new-file-path))
	      (setq new-file-paths (cons new-file-path new-file-paths))))
      (dired-mark-files-by-paths new-file-paths)))

  (defun dhnam/dired-do-convert-pdf-to-txt (&optional arg)
    (interactive "P")
    (let (new-file-paths)
      (dolist (file-path (dired-get-marked-files nil arg))
        (let* ((dir-path (file-name-directory file-path))
	           (file-name (file-name-nondirectory file-path))
	           (new-file-path (replace-regexp-in-string ".pdf" ".txt"
			                                            (concat dir-path (dhnam/normalize-paper-name file-name)))))
	      (unless (file-exists-p new-file-path)
	        (setq new-file-paths (cons new-file-path new-file-paths)) ; exclude existing files
	        (shell-command (concat "pdftotext" " " file-path " " new-file-path)))))
      (dired-unmark-all-marks)
      (dired-mark-files-by-paths new-file-paths)))
)

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

(add-hook 'dired-mode-hook
	      (lambda () (local-set-key (kbd "C-c C-o") 'xah-open-in-external-app)))

(progn
  ;; `dired-x' has functions like `dired-do-relsymlink'.
  ;; `dired-do-relsymlink' is mapped to "Y" in `dired-mode-map'.
  (require 'dired-x))

(require 'dhnam-dired)

(progn
  (define-key dired-mode-map (kbd "C-c y") 'dhnam/dired-rsync/do))

(comment
  ;; It's not working well with ssh dired (cannot use a tramp path).
  ;; It doesn't show the progress and cannot cancel rsync
  (when (fboundp 'dired-rsync)
    ;; https://github.com/stsquad/dired-rsync
    (define-key dired-mode-map (kbd "Y") 'dired-rsync)
    (autoload 'dired-rsync "dired-rsync" "Asynchronously copy files in dired to `DEST' using rsync." t nil)))

(progn
  ;; `dhnam/dired-find-file-following-symlink' is mapped instead of `dired-find-file-other-window'
  (progn
    (define-key dired-mode-map (kbd "<C-return>") 'dhnam/dired-find-actual-file)
    (define-key dired-mode-map (kbd "C-c RET") 'dhnam/dired-find-actual-file))
  (define-key dired-mode-map (kbd "M-RET") 'dhnam/dired-find-actual-file))

(progn
  (comment (define-key global-map (kbd "C-x F") 'find-grep-dired))
  (define-key global-map (kbd "C-x F") 'find-name-dired))

(provide 'init-dired)
