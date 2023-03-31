
(require 'dhnam-git)

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
