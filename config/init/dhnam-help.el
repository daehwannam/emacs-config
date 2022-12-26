
(defun dhnam/describe-key-map (keymap)
  (interactive
   ;; This `interactive' is copied from `describe-variable'
   (let ((v (variable-at-point))
	     (enable-recursive-minibuffers t)
         (orig-buffer (current-buffer))
	     val)
     (setq val (completing-read
                (if (symbolp v)
                    (format
                     "Type keymap (default %s): " v)
                  "Type keymap: ")
                #'help--symbol-completion-table
                (lambda (vv)
                  ;; In case the variable only exists in the buffer
                  ;; the command we switch back to that buffer before
                  ;; we examine the variable.
                  (with-current-buffer orig-buffer
                    (or (get vv 'variable-documentation)
                        (and (boundp vv) (not (keywordp vv))))))
                t nil nil
                (if (symbolp v) (symbol-name v))))
     (list (if (equal val "")
	           v (intern val)))))

  (let ((buf (get-buffer-create "*Key Map Help*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert
       (symbol-name keymap)
       "\n\n"
       (progn
         ;; get the information of keymap
         ;; this code snippet is used by `describe-mode'
         ;; https://stackoverflow.com/a/3480727
         (substitute-command-keys (format "\\{%s}" (symbol-name keymap))))))

    (switch-to-buffer-other-window buf)))
