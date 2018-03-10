
(cond
 ((string-equal machine-domain "vbox") ; vbox linux
  (progn
    (require 'org-ref)

    (setq reftex-default-bibliography '("~/data/Dropbox/org/bibliography/references.bib"))

    ;; see org-ref for use of these variables
    (setq org-ref-bibliography-notes "~/data/Dropbox/org/bibliography/notes.org"
	  org-ref-default-bibliography '("~/data/Dropbox/org/bibliography/references.bib")
	  org-ref-pdf-directory "~/data/Dropbox/org/bibliography/bibtex-pdfs/")

    ;; citation key setting
    (setq bibtex-completion-bibliography "~/data/Dropbox/org/bibliography/references.bib"
	  bibtex-completion-library-path "~/data/Dropbox/org/bibliography/bibtex-pdfs"
	  bibtex-completion-notes-path "~/data/Dropbox/org/bibliography/helm-bibtex-notes")

    ;; open pdf with system pdf viewer (works on mac)
    (setq bibtex-completion-pdf-open-function
	  (lambda (fpath)
	    (start-process "open" "*open*" "open" fpath)))
    )))



