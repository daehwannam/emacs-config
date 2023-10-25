
(progn
  ;; Display *Help* in the current window
  ;; https://emacs.stackexchange.com/a/22502

  (add-to-list 'display-buffer-alist
               '("*Help*" display-buffer-same-window)))

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


(require 'dhnam-miscellaneous)

(define-key help-mode-map (kbd "o") 'dhnam/push-button-same-window)
(define-key xref--xref-buffer-mode-map (kbd "o") 'dhnam/xref-goto-xref-same-window)


(provide 'init-help)
