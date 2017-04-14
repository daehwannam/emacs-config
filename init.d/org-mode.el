;;; org-mode
;; http://orgmode.org/worg/org-tutorials/orgtutorial_dto.html
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(setq org-agenda-files (directory-files-recursively
			"e:/Dropbox/org/" ".*\\.org\\(\\.txt\\)*$"))
;(directory-files "e:/Dropbox/org/" t ".*\\.org\\(\\.txt\\)*$")

;;; org-mode file extension change for dropbox access
;; https://www.reddit.com/r/emacs/comments/2rv4l3/i_love_org_files_but_how_do_i_edit_them_from_my/
;; https://www.emacswiki.org/emacs/AutoModeAlist
(add-to-list 'auto-mode-alist '("\\.org.txt\\'" . org-mode))

;;; org-mode agenda length
;; http://stackoverflow.com/questions/32423127/how-to-view-the-next-days-in-org-modes-agenda

(setq org-agenda-span 35
      org-agenda-start-on-weekday nil
      org-agenda-start-day "-7d")
