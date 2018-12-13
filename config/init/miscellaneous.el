

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


(defun copy-other-window-path-to-clipboard ()
  "Copy the other window's path."
  (interactive)
  (let ((path (progn (other-window 1)
		(let ((path default-directory))
		  (other-window -1)
		  path))))
    (when path
      (kill-new path)
      (message "'%s'" path))))


;; highlight setting
;; (require 'highlight nil t)


;; Deletion with Trash
(setq delete-by-moving-to-trash t)

