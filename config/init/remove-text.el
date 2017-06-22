

;; https://stackoverflow.com/questions/26478594/how-to-delete-empty-lines-in-a-file-by-emacs/26492924#26492924
(defun remove-empty-lines ()
  (interactive)
  (flush-lines "^[[:space:]]*$"))

(defun remove-tailing-white-space ()
  (interactive)
  (replace-regexp "[[:space:]]*$" ""))

