
(defun dhnam/desktop-change-and-make-dir (dirname)
  "Change to desktop saved in DIRNAME.
Kill the desktop as specified by variables `desktop-save-mode' and
`desktop-save', then clear the desktop and load the desktop file in
directory DIRNAME."
  (interactive "FChange to directory: ")
  (setq dirname (file-name-as-directory (expand-file-name dirname desktop-dirname)))
  (make-directory dirname)
  (desktop-kill)
  (desktop-clear)
  (desktop-read dirname))
