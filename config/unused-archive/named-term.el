(provide 'named-term)

;; (defun named-term ()
;;   (interactive)
;;   (let (buffer)
;;     (if current-prefix-arg
;;         (setq buffer (read-buffer "Term buffer: ")))
;;     (term "/bin/bash")
;;     (if buffer
;; 	(rename-buffer buffer))))


(defun named-term (&optional program &optional buffer)
  "Start a terminal-emulator in a new buffer.
The buffer is in Term mode; see `term-mode' for the
commands to use in that buffer.
\\<term-raw-map>Type \\[switch-to-buffer] to switch to another buffer."
  (interactive)
  (list
   (if (not program)
       (setq program (read-from-minibuffer "Run program: "
					   (or explicit-shell-file-name
					       (getenv "ESHELL")
					       (getenv "SHELL")
					       "/bin/sh"))))
   (if (not buffer)
       (setq buffer (read-buffer "Term buffer: "
				 ;; If the current buffer is an inactive
				 ;; term buffer, use it as the default.
				 (if (and (eq major-mode 'term-mode)
					  (null (get-buffer-process (current-buffer))))
				     (buffer-name)
				   (generate-new-buffer-name "*term*")))))
   )
  (if (get-buffer buffer)
      (progn
	(switch-to-buffer buffer))
    (progn
      (set-buffer (make-term buffer program))
      (term-mode)
      (term-char-mode)
      (switch-to-buffer (concat "*" buffer "*"))
      (rename-buffer buffer))))


(defalias 'nterm 'named-term)

