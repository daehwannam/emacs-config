
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
