
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


  (comment
   (define-key global-map "\C-cl" 'org-store-link)
   (define-key global-map "\C-ca" 'org-agenda))
  (progn
    (define-key global-map (kbd "C-c l") 'org-store-link)
    (define-key global-map (kbd "C-c a") 'org-agenda))
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

  (when (machine-config-get-first 'org-agenda-directory-pattern)
    (setq org-agenda-files (apply 'directory-files-recursively (machine-config-get-first 'org-agenda-directory-pattern))))
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


  (progn
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

    (if (version< emacs-version "27.1")
	;; comparing org-mode version would be better
	;; "easy template" does not work in org-mode 9.2
	(progn
	  ;; <python code black>
	  ;; - https://emacs.stackexchange.com/a/12847
	  ;;
	  ;; <easy template>
	  ;; - usage: <s + TAB
	  ;; - Caution: easy template would not work in the recent org-mode
	  ;;
	  ;; <easy template for latest org-mode: org-tempo>
	  ;; - https://emacs.stackexchange.com/a/46992
	  ;; - easy teamplte can be used for latest org-mode versions
	  ;; - however, it cannot use customized templates, such as 'pyo

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
	  )
      (progn
	;; <structured template>
	;; https://emacs.stackexchange.com/questions/46988/why-do-easy-templates-e-g-s-tab-in-org-9-2-not-work
	(require 'org-tempo)
	;; org-tempo
	;; "<s + TAB" --> code block
	))

    (progn
      ;; To keep python indentation in org-babel
      ;; https://stackoverflow.com/a/20903001
      ;;
      ;; This helps to keep indentation 4
      ;; However, it doesn't allow python code to be indented with
      ;; org-mode's headline.
      (setq org-src-preserve-indentation t))

    (progn
      ;; Disable automatic tab insertion
      ;;
      ;; https://www.emacswiki.org/emacs/NoTabs
      (setq-default indent-tabs-mode nil)))

  ;;; table
  (add-hook 'org-mode-hook (lambda () (local-set-key (kbd "C-c C-x M-c") 'org-table-insert-column)))
  (add-hook 'org-mode-hook (lambda () (local-set-key (kbd "C-c C-x M-r") 'org-table-insert-row)))


  (require 'make-repeatable-command)

  (if (and nil (package-installed-p 'hydra))
      (progn
	;; https://orgmode.org/worg/orgcard.html
	(defhydra hydra-org-motion ()
	  "buffer-move"
	  ("q" nil "quit")
	  ("n" org-next-visible-heading)
	  ("p" org-previous-visible-heading)
	  ("f" org-forward-heading-same-level)
	  ("b" org-backward-heading-same-level))

	(progn
	  (define-key org-mode-map (kbd "C-C C-n") 'hydra-org-motion/org-next-visible-heading)
	  (define-key org-mode-map (kbd "C-c C-p") 'hydra-org-motion/org-previous-visible-heading)
	  (define-key org-mode-map (kbd "C-c C-f") 'hydra-org-motion/org-forward-heading-same-level)
	  (define-key org-mode-map (kbd "C-c C-b") 'hydra-org-motion/org-backward-heading-same-level)))
    (progn
      (define-key org-mode-map (kbd "C-C C-n") (make-repeatable-command 'org-next-visible-heading))
      (define-key org-mode-map (kbd "C-c C-p") (make-repeatable-command 'org-previous-visible-heading))
      (define-key org-mode-map (kbd "C-c C-f") (make-repeatable-command 'org-forward-heading-same-level))
      (define-key org-mode-map (kbd "C-c C-b") (make-repeatable-command 'org-backward-heading-same-level))))

  (when (fboundp 'org-fragtog-mode)
    (add-hook 'org-mode-hook 'org-fragtog-mode))

  (defun org-format-latex-change-scale (scale)
    (interactive "nScale: " )
    (plist-put org-format-latex-options :scale scale))
  )
