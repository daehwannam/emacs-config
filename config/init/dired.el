

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
