

;; https://stackoverflow.com/questions/26478594/how-to-delete-empty-lines-in-a-file-by-emacs/26492924#26492924
(defun remove-empty-lines ()
  (interactive)
  (flush-lines "^[[:space:]]*$"))

;; (defun remove-trailing-white-space ()
;;   (interactive)
;;   (replace-regexp "[[:space:]]*$" ""))
;;
;; USE "delete-trailing-whitespace" instead
;; https://www.emacswiki.org/emacs/DeletingWhitespace#toc3

(defun shrink-empty-lines ()
  (interactive)
  (replace-regexp "^[[:space:]]*\n\\([[:space:]]*\n\\)+" "\n"))
(shrink-empty-lines)


;; https://stackoverflow.com/a/25886353
(defun toggle-camelcase-underscores ()
  "Toggle between camelcase and underscore notation for the symbol at point."
  (interactive)
  (save-excursion
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (start (car bounds))
           (end (cdr bounds))
           (currently-using-underscores-p (progn (goto-char start)
                                                 (re-search-forward "_" end t))))
      (if currently-using-underscores-p
          (progn
            (upcase-initials-region start end)
            (replace-string "_" "" nil start end)
            (downcase-region start (1+ start)))
        (replace-regexp "\\([A-Z]\\)" "_\\1" nil (1+ start) end)
        (downcase-region start (cdr (bounds-of-thing-at-point 'symbol)))))))

(global-set-key (kbd "M-_") 'toggle-camelcase-underscores)
