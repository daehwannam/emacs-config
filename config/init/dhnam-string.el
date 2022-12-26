
;; https://stackoverflow.com/questions/23636226/how-to-round-all-the-numbers-in-a-region
(defun dhnam/round-number (start end &optional fp)
  "round the numbers of region."
  (interactive "r")  ; region is necessary
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Text-from-Minibuffer.html
  (let ((fp (or fp (read-string "Enter floating point format: ")))) ; (ex. 0.3)
    (save-restriction
      (narrow-to-region start end)
      (goto-char 1)
      (let ((case-fold-search nil))
	(while (search-forward-regexp "\\([0-9]+\\.[0-9]+\\)" nil t)
	  (replace-match (format (concat "%" fp "f") (string-to-number (match-string 1)))
			 ))))))


;; round by query-replace-regexp
;; https://stackoverflow.com/a/23644015
;;
;; C-M-% -> [0-9]+\.[0-9]+ -> RET -> \,(format "%0.2f" \#&) -> RET
