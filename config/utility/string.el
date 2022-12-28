

(defun dhnam/string-trim (string)
  ;; http://ergoemacs.org/emacs/modernization_elisp_lib_problem.html
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string))
  )


(defun dhnam/string-starts-with (s begins)
  ;; https://www.emacswiki.org/emacs/ElispCookbook
  "Return non-nil if string S starts with BEGINS."
  (cond ((>= (length s) (length begins))
         (string-equal (substring s 0 (length begins)) begins))
        (t nil)))


(defun dhnam-key-swap/string-to-char (s)
  (let ((k (kbd s)))
    (cond ((stringp k) (string-to-char k))
          ((vectorp k) (elt k 0)))))
