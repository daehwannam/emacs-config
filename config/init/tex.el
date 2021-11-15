
(add-hook 'LaTeX-mode-hook 'linum-mode)

;;; preview pane mode
;; https://www.emacswiki.org/emacs/LaTeXPreviewPane
;;
;; https://stackoverflow.com/questions/757564/in-emacs-lisp-how-do-i-check-if-a-variable-is-defined
(if (boundp 'latex-preview-pane-enable)
    (latex-preview-pane-enable))

;; https://tex.stackexchange.com/a/967
(add-hook 'LaTeX-mode-hook #'turn-on-flyspell)

;; https://tex.stackexchange.com/a/161303
(setq LaTeX-command-style '(("" "%(PDF)%(latex) -shell-escape %S%(PDFout)")))

(when (package-installed-p 'pdf-tools)
  (progn
    ;; install pdf-tools if didin't yet
    (pdf-tools-install))

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
              #'TeX-revert-document-buffer)))
