
;; save and load a list
;; https://stackoverflow.com/a/44834833
(defun print-to-file (data filename)
  (with-temp-file filename
    (prin1 data (current-buffer))))

(defun read-from-file (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (cl-assert (eq (point) (point-min)))
    (read (current-buffer))))

;; (print-to-file '(a f c (q e g)) "test.el")
;; (read-from-file "test.el")

(defun joindirs (root &rest dirs)
  "Joins a series of directories together, like Python's os.path.join,
  (dotemacs-joindirs \"/tmp\" \"a\" \"b\" \"c\") => /tmp/a/b/c"

  ;; https://stackoverflow.com/a/13473856

  (if (not dirs)
      root
    (apply 'joindirs
           (expand-file-name (car dirs) root)
           (cdr dirs))))
