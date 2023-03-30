
(progn
  ;; bookmark setting
  ;; http://ergoemacs.org/emacs/bookmark.html


  (setq bookmark-save-flag 1)  ; bookmark auto save

  (comment
    ;; set bookmark page as start page
    (setq inhibit-splash-screen t)
    (require 'bookmark)
    (bookmark-bmenu-list)
    (switch-to-buffer "*Bookmark List*"))

  ;; bookmark location
  (setq bookmark-default-file  (concat user-emacs-directory "bookmarks"))) ; location: ~/.emacs.d/bookmarks

(provide 'init-bookmark)
