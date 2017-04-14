;;; bookmark+
;(require 'bookmark+)

;;; bookmark setting
;; http://ergoemacs.org/emacs/bookmark.html
;;
;; bookmark auto save
(setq bookmark-save-flag 1)
;; set bookmark page as start page
(setq inhibit-splash-screen t)
(require 'bookmark)
(bookmark-bmenu-list)
(switch-to-buffer "*Bookmark List*")
;; bookmark location
(setq bookmark-default-file  (concat user-emacs-directory "bookmarks")) ; location: ~/.emacs.d/bookmarks
