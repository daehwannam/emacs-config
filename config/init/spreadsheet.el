
(require 'dhnam-spreadsheet)

(progn
  ;; https://www.emacswiki.org/emacs/CsvMode
  ;; https://elpa.gnu.org/packages/csv-mode.html
  (when (package-installed-p 'csv-mode)
    (add-hook 'csv-mode-hook #'csv-align-mode)
    (progn
      (require 'csv-mode)
      (comment (define-key csv-mode-map [(control ?c) (control ?l)] 'csv-align-mode))
      (setq dhnam/csv-toggle-key (kbd "C-c C-l"))
      (define-key csv-mode-map dhnam/csv-toggle-key 'dhnam/toggle-csv)
      (define-key tsv-mode-map dhnam/csv-toggle-key 'dhnam/toggle-tsv))))

(provide 'init-spreadsheet)
