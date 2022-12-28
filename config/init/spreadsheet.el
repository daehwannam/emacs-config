(progn
  ;; https://www.emacswiki.org/emacs/CsvMode
  ;; https://elpa.gnu.org/packages/csv-mode.html
  (when (package-installed-p 'csv-mode)
    (add-hook 'csv-mode-hook #'csv-align-mode)
    (progn
      (require 'csv-mode)
      (define-key csv-mode-map [(control ?c) (control ?l)] 'csv-align-mode))))

(provide 'dhnam-spreadsheet)
