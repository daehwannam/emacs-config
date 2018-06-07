

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
