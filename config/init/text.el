

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

;; https://stackoverflow.com/a/25886353
(defun toggle-camelcase-underscores ()
  "Toggle between camelcase and underscore notation for the symbol at point."
  (interactive)
  (save-excursion
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (start (car bounds))
           (end (cdr bounds))
           (currently-using-underscores-p (progn (goto-char start)
                                                 (re-search-forward "_" end t))))
      (if currently-using-underscores-p
          (progn
            (upcase-initials-region start end)
            (replace-string "_" "" nil start end)
            (downcase-region start (1+ start)))
        (replace-regexp "\\([A-Z]\\)" "_\\1" nil (1+ start) end)
        (downcase-region start (cdr (bounds-of-thing-at-point 'symbol)))))))

(global-set-key (kbd "M-_") 'toggle-camelcase-underscores)

(defun kill-ring-save-at-point ()
  "Copy the symbol at point."
  (interactive)
  (save-excursion
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (start (car bounds))
           (end (cdr bounds))
           (currently-using-underscores-p (progn (goto-char start)
                                                 (re-search-forward "_" end t))))
      (kill-ring-save start end)
      )))

(comment (global-set-key (kbd "C-x M-w") 'kill-ring-save-at-point))
(comment (global-set-key (kbd "M-W") 'kill-ring-save-at-point))
(key-chord-define-global "w." 'kill-ring-save-at-point)

(defun just-one-space-in-region (beg end)
  "replace all whitespace in the region with single spaces"
  ;; https://stackoverflow.com/a/8674989

  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward "\n+" nil t)
	    (replace-match " ")))
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward "\\s-+" nil t)  ; \\s-+
	    (replace-match " ")))))

(defun just-one-space-conditionally (&optional n)
  (interactive "*p")
  (if (use-region-p)
      (just-one-space-in-region (region-beginning) (region-end))
    (just-one-space n)))

;; (global-set-key (kbd "M-<SPC>") 'just-one-space-conditionally)
(cl-flet ((bound-key-p (k) (when (let ((func (lookup-key (current-global-map) k)))
				   (or (eq func 'just-one-space)
				       (eq func 'just-one-space-conditionally)))
			     k)))
  (let ((k (or (bound-key-p (kbd "M-<SPC>"))
	       (bound-key-p (kbd "M-'"))
	       (t (error "No key for 'just-one-space is found")))))
    (global-set-key k 'just-one-space-conditionally)))

(progn
  ;; https://www.emacswiki.org/emacs/CopyingWholeLines
  (defun copy-and-next-line (arg)
    "Copy lines (as many as prefix argument) in the kill ring.
      Ease of use features:
      - Move to start of next line.
      - Appends the copy on sequential calls.
      - Use newline as last char even on the last line of the buffer.
      - If region is active, copy its lines."
    (interactive "p")
    (let ((beg (point))
          (end (line-end-position arg)))
      (when mark-active
        (if (> (point) (mark))
            (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
          (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
      (if (eq last-command 'copy-and-next-line)
          (kill-append (buffer-substring beg end) (< end beg))
        (kill-ring-save beg end)))
    (kill-append "\n" nil)
    (beginning-of-line (or (and arg (1+ arg)) 2))
    (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

  (key-chord-define (current-global-map) "kk" (make-repeatable-command #'copy-and-next-line)))

(progn
  ;; add new keybindings for kitty terminal
  (key-chord-define-global "q\\" 'indent-region))

(when (package-installed-p 'expand-region)
  (require 'expand-region)
  (comment (global-set-key (kbd "C-=") 'er/expand-region))
  (global-set-key (kbd "C-c .") (make-repeatable-command #'er/expand-region)))

(progn
  ;; enable `narrow-to-region'
  (put 'narrow-to-region 'disabled nil))
