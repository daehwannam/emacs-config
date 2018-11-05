
;; read file contents
;; http://ergoemacs.org/emacs/elisp_read_file_content.html
(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))


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
