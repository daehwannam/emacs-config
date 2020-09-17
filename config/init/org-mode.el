
;; How to install latest org mode
;; [[https://www.reddit.com/r/emacs/comments/5sx7j0/how_do_i_get_usepackage_to_ignore_the_bundled/]]
;; we can install 'org-plus-contrib' to do together the latest version of 'org' which 'org-plus-contrib' depends on.

;;; org-mode
;; http://orgmode.org/worg/org-tutorials/orgtutorial_dto.html

(when (fboundp 'org-mode)
  ;; (package-initialize)
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

  (when (machine-config-get 'org-agenda-directory-pattern)
    (setq org-agenda-files (apply 'directory-files-recursively (machine-config-get 'org-agenda-directory-pattern))))
  ;; (cond
  ;; 					; ((string-equal system-type "windows-nt") ; Microsoft Windows
  ;;  ((string-equal machine-domain "ms") ; Microsoft Windows
  ;;   (progn
  ;;     (setq org-agenda-files (directory-files-recursively
  ;; 			      "e:/data/Dropbox/org/schedule/" ".*\\.org\\(\\.txt\\)*$"))
  ;;     ))
  ;; 					; ((string-equal system-type "gnu/linux") ; linux
  ;;  ((string-equal machine-domain "vbox") ; vbox linux
  ;;   (progn
  ;;     (setq org-agenda-files (directory-files-recursively
  ;; 			      "~/data/Dropbox/org/schedule/" ".*\\.org\\(\\.txt\\)*$"))
  ;;     )))

  ;; 					;(directory-files "e:/Dropbox/org/" t ".*\\.org\\(\\.txt\\)*$")


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
  ;; https://emacs.stackexchange.com/a/43768
  (let ((shell-symbol (if (version< emacs-version "26.0") 'sh 'shell)))
    (org-babel-do-load-languages
     'org-babel-load-languages `((python . t) (,shell-symbol . t))))

  ;; https://www.reddit.com/r/orgmode/comments/64tiq9/syntax_highlighting_in_code_blocks/dg548nx/
  ;; Code black highlighting and indentation
  (setq org-src-fontify-natively t
	org-src-tab-acts-natively t
	org-confirm-babel-evaluate nil
	org-edit-src-content-indentation 0)

  ;; python code black
  ;; https://emacs.stackexchange.com/a/12847
  (add-to-list 'org-structure-template-alist
	       '("py" "#+BEGIN_SRC python :results value\n?\n#+END_SRC"))

  ;; change code results to "output" (default is "value" for return)
  ;; https://emacs.stackexchange.com/questions/17926/python-org-mode-source-block-output-is-always-none
  (add-to-list 'org-structure-template-alist
	       '("pyo" "#+BEGIN_SRC python :results output\n?\n#+END_SRC"))

  ;; shell code black
  (add-to-list 'org-structure-template-alist
	       '("sh" "#+BEGIN_SRC sh :results value\n?\n#+END_SRC"))

  (add-to-list 'org-structure-template-alist
	       '("sho" "#+BEGIN_SRC sh :results output\n?\n#+END_SRC"))

  ;; virtualenv
  ;; https://emacs.stackexchange.com/a/38047
  (add-to-list 'org-structure-template-alist
	       '("pyto" "#+BEGIN_SRC elisp :session s-py3\n(pyvenv-workon \"py3\")\n#+END_SRC\n#+BEGIN_SRC python :results output :session spy3\n?\n#+END_SRC"))

  ;; org code black insert image
  ;; https://emacs.stackexchange.com/questions/44516/orgmode-ipython-output-image-not-show-in-results
  ;; 
  ;; [not added yet]

  ;;; table
  (add-hook 'org-mode-hook (lambda () (local-set-key (kbd "C-c C-x M-c") 'org-table-insert-column)))
  (add-hook 'org-mode-hook (lambda () (local-set-key (kbd "C-c C-x M-r") 'org-table-insert-row)))
  )
