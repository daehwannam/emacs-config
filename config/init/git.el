
(defun dhnam/insert-gitignore-example ()
  (interactive)
  (insert (dhnam/get-string-from-file "~/.emacs.d/config/init/dependent/gitignore.gitignore")))


(progn
  ;; Org-mode & tikz example
  ;; https://www.homepages.ucl.ac.uk/~ucahjde/blog/tikz.html

  (defun dhnam/insert-org-tikz-output-frame ()
    (interactive)
    (insert (dhnam/get-string-from-file "~/.emacs.d/config/init/dependent/org-tikz-output-frame.org")))

  (defun dhnam/insert-org-tikz-silent-frame ()
    (interactive)
    (insert (dhnam/get-string-from-file "~/.emacs.d/config/init/dependent/org-tikz-silent-frame.org"))))

(progn
  ;; When a symlink indicates a file in a repository
  ;; https://stackoverflow.com/a/30900018
  (comment
    ;; to always follow the symlink (and edit the "actual" file directly)
    (setq vc-follow-symlinks t))
  (progn
    ;; to always edit the file as if it's at the symlink itself 
    (setq vc-follow-symlinks nil)))

(provide 'init-git)
