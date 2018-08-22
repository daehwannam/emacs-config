;;; org-mode
;; http://orgmode.org/worg/org-tutorials/orgtutorial_dto.html

(when (fboundp 'org-mode)
  (package-initialize)
  (require 'org)
  (require 'org-loaddefs)  ; No org-loaddefs.el file could be found from where org.el is loaded.

  (require 'directory-files-recursive)


  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (setq org-log-done t)


  ;; read file contents
  ;; http://ergoemacs.org/emacs/elisp_read_file_content.html
  (defun get-string-from-file (filePath)
    "Return filePath's file content."
    (with-temp-buffer
      (insert-file-contents filePath)
      (buffer-string)))



  ;; check OS type
  ;; http://ergoemacs.org/emacs/elisp_determine_OS_version.html

  (cond
					; ((string-equal system-type "windows-nt") ; Microsoft Windows
   ((string-equal machine-domain "ms") ; Microsoft Windows
    (progn
      (setq org-agenda-files (directory-files-recursively
			      "e:/data/Dropbox/org/schedule/" ".*\\.org\\(\\.txt\\)*$"))
      ))
					; ((string-equal system-type "gnu/linux") ; linux
   ((string-equal machine-domain "vbox") ; vbox linux
    (progn
      (setq org-agenda-files (directory-files-recursively
			      "~/data/Dropbox/org/schedule/" ".*\\.org\\(\\.txt\\)+$"))
      )))

					;(directory-files "e:/Dropbox/org/" t ".*\\.org\\(\\.txt\\)*$")


;;; org-mode file extension change for dropbox access
  ;; https://www.reddit.com/r/emacs/comments/2rv4l3/i_love_org_files_but_how_do_i_edit_them_from_my/
  ;; https://www.emacswiki.org/emacs/AutoModeAlist
  (add-to-list 'auto-mode-alist '("\\.org.txt\\'" . org-mode))


;;; org-mode agenda length
  ;; http://stackoverflow.com/questions/32423127/how-to-view-the-next-days-in-org-modes-agenda

  (setq org-agenda-span 70
	org-agenda-start-on-weekday nil
	org-agenda-start-day "-7d")


;;; line wrap
  ;; https://emacs.stackexchange.com/questions/18480/wrap-line-text-in-org-mode-when-i-re-open-a-file
  ;; it splits long lines
  (setq org-startup-truncated nil)


  ;; Code block setting
  (org-babel-do-load-languages
   'org-babel-load-languages '((python . t)))
  )