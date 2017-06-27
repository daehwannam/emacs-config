

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
