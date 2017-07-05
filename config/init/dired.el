

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
