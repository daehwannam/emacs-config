(defun dhnam/animate-something ()
  (interactive)
  (let ((buffer (buffer-name)))
    (switch-to-buffer (get-buffer-create "*Animation*"))
    (erase-buffer)
    (sit-for 0)
    (animate-string "Amazing physics going on..."
		    (/ (window-height) 2) (- (/ (window-width) 2) 12))
    (switch-to-buffer buffer)))
