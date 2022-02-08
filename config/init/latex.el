
(comment (add-hook 'LaTeX-mode-hook 'linum-mode))

;;; preview pane mode
;; https://www.emacswiki.org/emacs/LaTeXPreviewPane
;;
;; https://stackoverflow.com/questions/757564/in-emacs-lisp-how-do-i-check-if-a-variable-is-defined
(if (boundp 'latex-preview-pane-enable)
    (latex-preview-pane-enable))

;; https://tex.stackexchange.com/a/967
(add-hook 'LaTeX-mode-hook #'turn-on-flyspell)

(comment
  ;; Ignore crossref
  ;;
  ;; man bibtex : to avoid these automatic inclusions altogether, give this option a sufficiently large number
  ;; https://tex.stackexchange.com/a/123746
  ;;
  ;; how to configure bibtex command
  ;; https://tex.stackexchange.com/a/397667
  (eval-after-load "tex"
    '(setcdr (assoc "BibTeX" TeX-command-list)
             '("bibtex --min-crossrefs=10000 %s"
               TeX-run-BibTeX nil t :help "Run BibTeX with ..."))))

(progn
  ;; - Adding options -> https://tex.stackexchange.com/a/161303
  ;;
  ;; - Fixing errors below -> https://emacs-china.org/t/auctex-setup-synctex-with-pdf-tools-not-working/11257
  ;; ---------------------------------------------------------------------------------------------------------
  ;; pdf-info-query: epdfinfo: Unable to create synctex scanner, did you run latex with `--synctex=1' ?
  ;; ---------------------------------------------------------------------------------------------------------

  (comment (setq LaTeX-command-style '(("" "%(PDF)%(latex) -shell-escape %S%(PDFout)"))))
  (setq LaTeX-command-style '(("" "%(PDF)%(latex) -shell-escape %S%(PDFout) --synctex=1"))))


(when (package-installed-p 'pdf-tools)
  (progn
    ;; install pdf-tools if didin't yet
    (pdf-tools-install))

  (custom-set-variables
   ;; Fix for the following error:
   ;; ---------------------------------------------------------------------------------
   ;; Warning (pdf-view): These modes are incompatible with `pdf-view-mode',
   ;; 	please deactivate them (or customize pdf-view-incompatible-modes): linum-mode
   ;; ---------------------------------------------------------------------------------

   '(pdf-view-incompatible-modes
     '(linum-relative-mode helm-linum-relative-mode nlinum-mode nlinum-hl-mode nlinum-relative-mode yalinum-mode)))

  (comment
    ;; continuous scrolling
    ;; https://github.com/politza/pdf-tools/issues/27#issuecomment-927129868
    (assert (package-installed-p 'quelpa))
    (setq quelpa-update-melpa-p nil)    ; disabling auto-updating
    (quelpa '(pdf-continuous-scroll-mode
              :fetcher github
              :repo "dalanicolai/pdf-continuous-scroll-mode.el"))
    (add-hook 'pdf-view-mode-hook 'pdf-continuous-scroll-mode)))

(when (progn
        ;; to update `TeX-view-program-selection' with setcar,
        ;; `TeX-view-program-selection' should be loaded by (require 'tex)
        (require 'tex nil t))

  (progn
    ;; Using pdf-tools as the default view for AUCTeX
    ;;
    ;; https://emacs.stackexchange.com/a/19475
    ;; https://emacs.stackexchange.com/a/19475

    ;; to use pdfview with auctex
    (comment
      (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
            ;; TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
            ;; TeX-source-correlate-start-server t ; not sure if last line is neccessary
            ))
    (setcar (cdr (assoc 'output-pdf TeX-view-program-selection)) "PDF Tools")

    ;; to have the buffer refresh after compilation
    (add-hook 'TeX-after-compilation-finished-functions
              #'TeX-revert-document-buffer))

  (comment
    ;; Okular as the default pdf viewer
    ;; https://www.emacswiki.org/emacs/AUCTeX#h5o-27
    (comment (setq TeX-view-program-selection '((output-pdf "Okular"))))
    (setcar (cdr (assoc 'output-pdf TeX-view-program-selection)) "Okular"))

  (progn
    ;; enable forward search
    ;; https://lists.gnu.org/archive/html/auctex/2021-07/msg00018.html
    (setq TeX-source-correlate-mode t
          TeX-source-correlate-start-server t)

    ;; [key bindings for forward/inverse search]
    ;; https://www.gnu.org/software/auctex/manual/auctex/I_002fO-Correlation.html
    ;; forward search -> TeX-view (C-c C-v)
    ;; inverse search -> C-down-mouse-1 for pdf-tools (or "shift + left-mouse" for Okular)
    )
  )

(progn
  (make-local-variable 'bibliography-file-name-as-source-of-reference)
  (setq-default bibliography-file-name-as-source-of-reference "some-bibliography.bib")

  (defun find-reference-in-bibliography-file ()
    (interactive)
    (let (start-pos end-pos)
      (save-excursion
        ;; find identifiers separated by curly bracket, comma, white-space
        (when (re-search-backward "\[{,[:blank:]\]" nil t)
          (setq start-pos (1+ (point))))
        (when (re-search-forward "\[},[:blank:]\]" nil t)
          (setq end-pos (1- (point)))))
      (let ((ref-id-str nil)
            (ref-id-str-valid-p nil))
        (when (and start-pos end-pos)
          (setq ref-id-str (trim-string (buffer-substring-no-properties start-pos end-pos)))
          (setq ref-id-str-valid-p (not (or (string-match-p "[[:blank:]]" ref-id-str)
                                            (string-empty-p ref-id-str))))
          (when ref-id-str-valid-p
            (let ((ref-pos nil))
              (find-file-other-window bibliography-file-name-as-source-of-reference)
              (save-excursion
                (beginning-of-buffer)
                (re-search-forward ref-id-str)
                (setq ref-pos (point)))
              (when ref-pos
                (goto-char ref-pos)
                (recenter-top-bottom)))))

        (unless ref-id-str-valid-p
          (comment (xref-find-definitions (xref-backend-identifier-at-point (xref-find-backend))))
          (call-interactively 'xref-find-definitions))))))
