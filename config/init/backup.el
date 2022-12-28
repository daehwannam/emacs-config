(progn
  ;; Local file backup
  ;; https://www.emacswiki.org/emacs/BackupDirectory
  ;; https://www.emacswiki.org/emacs/ForceBackups
  (setq
   backup-by-copying t                   ; don't clobber symlinks
   backup-directory-alist
   '(("." . "~/.emacs.d/backup/"))       ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t))                   ; use versioned backups

(progn
  ;; Remote file backup
  ;; http://www.gnu.org/software/emacs/manual/html_node/tramp/Auto_002dsave-and-Backup.html
  (customize-set-variable
   'tramp-backup-directory-alist backup-directory-alist))

(provide 'dhnam-backup)
