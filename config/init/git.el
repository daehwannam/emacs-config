
(defun insert-gitignore-example ()
  (interactive)
  (insert (get-string-from-file "~/.emacs.d/config/init/dependent/gitignore.gitignore")))


;;; Org-mode & tikz example
;;; https://www.homepages.ucl.ac.uk/~ucahjde/blog/tikz.html

(defun insert-org-tikz-output-frame ()
  (interactive)
  (insert (get-string-from-file "~/.emacs.d/config/init/dependent/org-tikz-output-frame.org")))

(defun insert-org-tikz-silent-frame ()
  (interactive)
  (insert (get-string-from-file "~/.emacs.d/config/init/dependent/org-tikz-silent-frame.org")))
