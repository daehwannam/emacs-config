
(add-hook 'LaTeX-mode-hook 'linum-mode)

;;; preview pane mode
;; https://www.emacswiki.org/emacs/LaTeXPreviewPane
;;
;; https://stackoverflow.com/questions/757564/in-emacs-lisp-how-do-i-check-if-a-variable-is-defined
(if (boundp 'latex-preview-pane-enable)
    (latex-preview-pane-enable))

;; https://tex.stackexchange.com/a/967
(add-hook 'LaTeX-mode-hook #'turn-on-flyspell)

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

  (progn
    ;; Using pdf-tools as the default view for AUCTeX
    ;;
    ;; https://emacs.stackexchange.com/a/19475
    ;; https://emacs.stackexchange.com/a/19475

    ;; to use pdfview with auctex
    (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
          ;; TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
          ;; TeX-source-correlate-start-server t ; not sure if last line is neccessary
          )

    ;; to have the buffer refresh after compilation
    (add-hook 'TeX-after-compilation-finished-functions
              #'TeX-revert-document-buffer))

  (progn
    (setq quelpa-update-melpa-p nil)
    (quelpa '(pdf-continuous-scroll-mode
              :fetcher github
              :repo "dalanicolai/pdf-continuous-scroll-mode.el"))
   (add-hook 'pdf-view-mode-hook 'pdf-continuous-scroll-mode)))

(progn
  ;; enable forward search
  ;; https://lists.gnu.org/archive/html/auctex/2021-07/msg00018.html
  (setq TeX-source-correlate-mode t
        TeX-source-correlate-start-server t))
