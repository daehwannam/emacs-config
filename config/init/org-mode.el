
(comment
  ;; How to install latest org mode
  ;; [[https://www.reddit.com/r/emacs/comments/5sx7j0/how_do_i_get_usepackage_to_ignore_the_bundled/]]
  ;; we can install 'org-contrib' to do together the latest version of 'org' which 'org-contrib' depends on.
  ;; When using old version of org-mode, install 'org-plus-contrib' instead.
)

;;; org-mode
;; http://orgmode.org/worg/org-tutorials/orgtutorial_dto.html

(when (fboundp 'org-mode)
  ;; (package-initialize)
  (require 'org)
  (require 'org-loaddefs)  ; No org-loaddefs.el file could be found from where org.el is loaded.

  (require 'dhnam-directory-files-recursively)
  (require 'dhnam-org-mode)

  (when (package-installed-p 'valign)
    ;; Fix for table alignment with CJK characters
    ;;
    ;; valign-mode
    ;; https://github.com/casouri/valign
    (add-hook 'org-mode-hook #'valign-mode))

  (defvar dhnam/org-hard-indentation-enabled nil)

  (if dhnam/org-hard-indentation-enabled
      (progn
        (progn
          ;; enable indenting content of headings by
          ;; org-metaleft and org-metaright
          ;;
          ;; It's working with (setq org-startup-indented nil)
          (setq org-adapt-indentation t))

        (comment
          (progn
            ;; hard indentation setup
            ;; https://orgmode.org/manual/Hard-indentation.html
            (setq org-adapt-indentation t)
            (setq org-hide-leading-stars t)
            (setq org-odd-levels-only nil))))
    (progn
      ;; https://www.reddit.com/r/emacs/comments/97naje/what_is_everyones_org_mode_indentation_preferences/
      (setq org-startup-indented t)))

  (progn
    ;; Change applications used in org-mode
    ;; https://emacs.stackexchange.com/a/26881
    (setq org-file-apps
          '((auto-mode . emacs)
            (directory . emacs)
            ("\\.mm\\'" . default)
            ("\\.x?html?\\'" . "xdg-open %s")
            ("\\.pdf\\'" . default))))

  (comment
    (define-key global-map "\C-cl" 'org-store-link)
    (define-key global-map "\C-ca" 'org-agenda))
  (progn
    (define-key global-map (kbd "C-c l") 'org-store-link)
    (define-key global-map (kbd "C-c a") 'org-agenda))
  (setq org-log-done t)

  ;; check OS type
  ;; http://ergoemacs.org/emacs/elisp_determine_OS_version.html

  (when (dhnam/machine-config-get-first 'org-agenda-directory-pattern)
    (setq org-agenda-files (apply 'directory-files-recursively (dhnam/machine-config-get-first 'org-agenda-directory-pattern))))
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
       'org-babel-load-languages `((python . t)
                                   (,shell-symbol . t)
                                   (latex . t)
                                   (lisp . t))))

    (progn
      ;; https://www.reddit.com/r/orgmode/comments/64tiq9/syntax_highlighting_in_code_blocks/dg548nx/
      ;; Code black highlighting and indentation
      (setq org-src-fontify-natively t)
      (progn
        ;; In org-mode 9.3, the option has bug when making a new code block just before another code block.
        ;; e.g. "<s" => TAB => previous-line "<s" => TAB
        (setq org-src-tab-acts-natively t))
      (setq org-confirm-babel-evaluate nil)
      (setq org-edit-src-content-indentation 0))

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

        (progn
          ;; Code block template: <[code] + TAB
          ;; e.g. <S + TAB
          ;;      <py + TAB
          (comment
            ;; These are replaced as yasnippet
            (add-to-list 'org-structure-template-alist '("S" . "src <language> :results drawer"))
            (add-to-list 'org-structure-template-alist '("So" . "src <language> :results drawer output")))
          (add-to-list 'org-structure-template-alist '("ba" . "src bash"))
          (add-to-list 'org-structure-template-alist '("Ba" . "src bash :results output drawer"))
          (add-to-list 'org-structure-template-alist '("sh" . "src bash"))
          (add-to-list 'org-structure-template-alist '("Sh" . "src bash :results output drawer"))
          (add-to-list 'org-structure-template-alist '("py" . "src python"))
          (add-to-list 'org-structure-template-alist '("Py" . "src python :results output drawer")))))

    (comment
      (unless dhnam/org-hard-indentation-enabled
        ;; To keep python indentation in org-babel
        ;; https://stackoverflow.com/a/20903001
        ;;
        ;; This helps to keep indentation 4
        ;; However, it doesn't allow python code to be indented with
        ;; org-mode's headline.
        ;; e.g. It's not working with org-metaleft and org-metaright
        ;;
        ;; If you doesn't use hard indentation, this setting has no problem.
        (setq org-src-preserve-indentation t)))

    (progn
      ;; Disable automatic tab insertion
      ;;
      ;; https://www.emacswiki.org/emacs/NoTabs
      (setq-default indent-tabs-mode nil))


    (comment
      ;; Deprecated: this fix slows down loading a large org-mode file.
      ;;
      ;; fix the problem of unbalanced parentheses by "<" and ">"
      ;; https://emacs.stackexchange.com/a/52209
      (add-hook 'org-mode-hook #'dhnam/org-setup-<>-syntax-fix))

    (add-hook 'org-mode-hook #'dhnam/org-syntax-table-modify))

  (progn
    ;; table
    (add-hook 'org-mode-hook (lambda () (local-set-key (kbd "C-c C-x M-c") 'org-table-insert-column)))
    (add-hook 'org-mode-hook (lambda () (local-set-key (kbd "C-c C-x M-r") 'org-table-insert-row))))

  (progn
    ;; `org-time-stamp' is original "C-c ." of `org-mode-map'
    (define-key org-mode-map (kbd "C-c .") nil)
    (define-key org-mode-map (kbd "C-c t") 'org-time-stamp))

  (require 'dhnam-make-repeatable-command)

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
	      (define-key org-mode-map (kbd "C-c C-n") 'hydra-org-motion/org-next-visible-heading)
	      (define-key org-mode-map (kbd "C-c C-p") 'hydra-org-motion/org-previous-visible-heading)
	      (define-key org-mode-map (kbd "C-c C-f") 'hydra-org-motion/org-forward-heading-same-level)
	      (define-key org-mode-map (kbd "C-c C-b") 'hydra-org-motion/org-backward-heading-same-level)))
    (progn
      (define-key org-mode-map (kbd "C-c C-n") (make-repeatable-command 'org-next-visible-heading))
      (define-key org-mode-map (kbd "C-c C-p") (make-repeatable-command 'org-previous-visible-heading))
      (define-key org-mode-map (kbd "C-c C-f") (make-repeatable-command 'org-forward-heading-same-level))
      (define-key org-mode-map (kbd "C-c C-b") (make-repeatable-command 'org-backward-heading-same-level))))

  (when (fboundp 'org-fragtog-mode)
    ;; https://github.com/io12/org-fragtog
    ;; `org-fragtog-mode' automatically toggles a preview for each latex formula
    (defvar dhnam/org-latex-preview-enabled t)
    (add-hook 'org-mode-hook (lambda () (when dhnam/org-latex-preview-enabled (org-latex-preview '(16)))))
    ;; (add-hook 'org-mode-hook 'org-fragtog-mode)
    )

  (comment
    ;; https://github.com/gaoDean/org-imgtog
    ;; `org-imgtog' automatically toggles a preview for each image
    )

  (defun dhnam/org-format-latex-change-scale (scale)
    (interactive "nScale: " )
    (plist-put org-format-latex-options :scale scale))

  (progn
    (dhnam/org-format-latex-change-scale 1.5))

  (progn
    ;; Bibliography with BibTeX
    ;; https://orgmode.org/worg/exporters/anno-bib-template-worg.html
    (setq org-latex-pdf-process
          '("pdflatex -interaction nonstopmode -output-directory %o %f"
            "bibtex %b"
            "pdflatex -interaction nonstopmode -output-directory %o %f"
            "pdflatex -interaction nonstopmode -output-directory %o %f")))

  (progn
    ;; Latex syntax highlight
    ;; https://stackoverflow.com/a/29918961
    (setq org-highlight-latex-and-related '(latex script entities)))

  (progn
    (progn
      ;; define customization for background color of inline image
      (defcustom org-inline-image-background nil
        "The color used as the default background for inline images.
When nil, use the default face background."
        :group 'org
        :type '(choice color (const nil)))

      (defun dhnam/create-image-with-background-color (args)
        "Specify background color of Org-mode inline image through modify `ARGS'."
        (let* ((file (car args))
               (type (cadr args))
               (data-p (caddr args))
               (props (cdddr args)))
          ;; Get this return result style from `create-image'.
          (append (list file type data-p)
                  (list :background (or org-inline-image-background (face-background 'default)))
                  props)))

      (advice-add 'create-image :filter-args
                  #'dhnam/create-image-with-background-color))

    (progn
      ;; set image background color
      ;; (custom-set-variables '(org-inline-image-background "dark gray"))
      ;; (custom-set-variables '(org-inline-image-background "white"))
      (custom-set-variables '(org-inline-image-background "light gray"))
      ))

  (defun dhnam/org-kill-link-to-clipboard ()
    ;; https://emacs.stackexchange.com/a/63051
    (interactive)
    (let* ((context (org-element-context))
           (type (org-element-type context))
           (beg (org-element-property :begin context))
           (end (org-element-property :end context)))
      (when (eq type 'link)
        (copy-region-as-kill beg end))))

  (defun dhnam/org-kill-raw-link-to-clipboard ()
    ;; https://emacs.stackexchange.com/a/63051
    (interactive)
    (let* ((context (org-element-context))
           (type (org-element-type context)))
      (when (eq type 'link)
        (kill-new (org-element-property :raw-link context)))))

  (key-chord-define org-mode-map "wl" 'dhnam/org-kill-raw-link-to-clipboard)

  (comment
    (defun dhnam/insert-inline-math ()
      (interactive)
      (insert "\\\(\\\)")
      (backward-char 2))

    (key-chord-define org-mode-map "()" 'dhnam/insert-inline-math))

  (progn
    ;; hide chracters to emphasize text
    ;; https://www.reddit.com/r/emacs/comments/6pxh92/comment/dksxo09/?utm_source=share&utm_medium=web2x&context=3
    (setq org-hide-emphasis-markers t))

  (progn
    (defun dhnam/org-open-at-point (&optional arg)
      "Modified version of `org-open-at-point'"
      (interactive "P")

      (let ((browse-url-browser-function 'dhnam/eww-new))
        (org-open-at-point arg)))
    (define-key org-mode-map (kbd "C-c O") 'dhnam/org-open-at-point))

  (progn
    ;; https://github.com/astahlman/ob-async
    (require 'ob-async nil t))

  (progn
    (define-key org-mode-map (kbd "C-c r") 'dhnam/org-display-remaining-days)

    (with-eval-after-load 'org-agenda
      (define-key org-agenda-mode-map (kbd "C-c r") 'dhnam/agenda-display-remaining-days)
      (key-chord-define org-agenda-mode-map "rm" 'dhnam/agenda-display-remaining-days)
      (define-key org-agenda-mode-map (kbd "M-p") 'dhnam/agenda-move-to-prev-span)
      (define-key org-agenda-mode-map (kbd "M-n") 'dhnam/agenda-move-to-next-span)))

  (progn
    ;; disable `org-cycle-agenda-files' which is mapped to "C-," by default.
    (define-key org-mode-map (kbd "C-,") nil))

  (progn
    ;; Disable to make a "Validate" link at bottom when exporting to HTML
    ;; https://stackoverflow.com/a/15145594
    ;; https://stackoverflow.com/a/65730497
    (setq org-html-validation-link nil))

  (progn
    ;; Disable to make a postamble at bottom when exporting to HTML
    ;; The postamble includes "Author", "Created" and "Validate".
    ;; https://stackoverflow.com/a/32761956
    (setq org-html-postamble nil))

  (when (= emacs-major-version 27)
    ;; Temporary command that is used for emacs 27 where an error occurs.
    ;; When exporting an org-mode file that has a citation,
    ;; then copy the citation more than once,
    ;; the following error occurs:
    ;; <org-fold-core--buffer-substring-filter: Invalid read syntax: "#">
    (require 'dhnam-cmd-line-eval))

  (define-key org-mode-map (kbd "C-c C-o") 'dhnam/org-open-at-point-same-window))


(provide 'init-org-mode)
