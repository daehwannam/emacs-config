
(unless (fboundp 'comment)
  (defmacro comment (&rest args)
    `nil))

(defun tct/rows-to-org-table (headers rows)
  (let ((unaligned-table
         (concat
          (format "| %s |\n" (mapconcat #'identity headers " | "))
          "|-\n"
          (mapconcat
           (lambda (row) (format "| %s |" (mapconcat #'identity row " | ")))
           rows
           "\n"))))
    (with-temp-buffer
      (insert unaligned-table)
      (org-table-align)
      (buffer-string))))

(defun tct/get-current-time-stamp ()
  (format-time-string "[%Y-%m-%d %H:%M]" (org-current-time)))

(defvar tct/time-stamp-regex "\\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}\\]")

(defun tct/insert-time-stamp ()
  (interactive)

  (insert (tct/get-current-time-stamp)))

(defvar tct/todo-done-pairs
  '(("TODO" "DONE")
    ("FIXME" "FIXED")
    ("BUG" "DEBUGGED")))

(defvar tct/non-paired-keywords nil)

(defun tct/get-non-paired-keywords (keywords)
  (let ((keywords-in-pairs (apply #'append tct/todo-done-pairs)))
    (seq-remove (lambda (keyword) (member keyword keywords-in-pairs)) keywords)))


(defvar tct/keyword-with-date-re-search-template "\\_<%s\\_> *\\(%s\\) *:? *\\(.*\\)$")
(defvar tct/keyword-with-period-re-search-template "\\_<%s\\_> *\\(%s\\)--\\(%s\\) *:? *\\(.*\\)$")
(defvar tct/keyword-fallback-re-search-template "\\_<%s\\_> *:? *\\(.*\\)$")
(defvar tct/keyword-fallback-re-search-template-no-keyword "%s *:? *\\(.*\\)$")

(defun tct/toggle (todo-keyword done-keyword &optional using-comment-dwim)
  "Insert a keyword or replace it as a counterpart.

When TODO-KEYWORD is \"TODO\" and DONE-KEYWORD is \"DONE\",
an example TODO comment is 'TODO [2025-11-18 16:40]: Some comment here.', and
an example DONE comment is 'DONE [2025-11-18 16:40]--[2025-11-18 17:25]: Some comment here.'.
"

  (or
   (let ((pattern (format tct/keyword-with-date-re-search-template todo-keyword tct/time-stamp-regex)))
     (save-excursion
       (beginning-of-line)
       (let ((found (re-search-forward pattern (line-end-position) t)))
         (when found
           (replace-match (format "%s %s--%s: %s"
                                  done-keyword
                                  (match-string 1)
                                  (tct/get-current-time-stamp)
                                  (match-string 2)))
           t))))
   (let ((pattern (format tct/keyword-with-period-re-search-template
                          done-keyword
                          tct/time-stamp-regex
                          tct/time-stamp-regex)))
     (save-excursion
       (beginning-of-line)
       (let ((found (re-search-forward pattern (line-end-position) t)))
         (when found
           (replace-match (format "%s %s: %s"
                                  todo-keyword
                                  (match-string 1)
                                  (match-string 3)))
           t))))
   (progn
     (when using-comment-dwim
       (comment-dwim nil)
       (when (not (eq (char-after (1- (point))) ?\ ))
         (insert " ")))
     (insert (format "%s %s: "
                     todo-keyword
                     (tct/get-current-time-stamp)))
     t)))

(defun tct/toggle-todo (&optional using-comment-dwim)
  "Insert or toggle a TODO or DONE comment."

  (interactive)

  (tct/toggle "TODO" "DONE" using-comment-dwim))

(defun tct/toggle-in-program (todo-keyword done-keyword)
  (tct/toggle
   todo-keyword
   done-keyword
   (and (derived-mode-p 'prog-mode)
        (not (dhnam/in-string-p))
        (not (dhnam/in-comment-p)))))

(defun tct/toggle-todo-in-program ()
  (interactive)

  (tct/toggle-in-program "TODO" "DONE"))


(defvar tct/file-path-display-length 30)
(defvar tct/file-path-omission "···")

(defun tct/get-displayed-file-path (file-path)
  (if (> (length file-path) tct/file-path-display-length)
      (concat
       tct/file-path-omission
       (substring-no-properties
        file-path
        (+ (- (length file-path) tct/file-path-display-length)
           (length tct/file-path-omission))))
    (progn
      (comment (concat (make-string (- tct/file-path-display-length (length file-path)) ? ) file-path))
      file-path)))


(defvar tct/grep-code-block-template
  "Command:
#+begin_src sh
%s
#+end_src

"
  )

(defun tct/run-command (command)
  (let ((buf (get-buffer-create (format "* %s *" command)))
        ;; (output-start nil)
        )
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "# -*- mode: org; todo-comment-table-mode: t; truncate-lines: t; eval: (read-only-mode 1) -*-\n\n")
        (insert (format tct/grep-code-block-template command))
        ;; (setq output-start (point))
        (comment
          ;; read-only UI mode; it also activates `read-only-mode'
          (special-mode))
        (read-only-mode 1)
        ;; (org-mode)
        ))

    ;; Create the process
    (make-process
     :name command
     :buffer buf
     :command (list shell-file-name shell-command-switch command)
     :filter (lambda (proc output)
               ;; When the output of the process is passed
               (with-current-buffer (process-buffer proc)
                 (let ((inhibit-read-only t))
                   (goto-char (point-max))
                   (insert output))))
     :sentinel (lambda (proc event)
                 ;; When the process is ended
                 (when (memq (process-status proc) '(exit signal))
                   (with-current-buffer (process-buffer proc)
                     (let ((tuples nil))
                       (let ((inhibit-read-only t)
                             (remaining t))
                         (beginning-of-buffer)
                         (re-search-forward (format (regexp-quote tct/grep-code-block-template) "[^\n]+"))
                         (while (and remaining (re-search-forward ":" (line-end-position) t))
                           (let ((path (buffer-substring (line-beginning-position) (match-beginning 0)))
                                 (file-line (let ((file-line-start (point)))
                                              (re-search-forward ":" (line-end-position) t)
                                              (buffer-substring file-line-start (match-beginning 0))))
                                 (text (buffer-substring-no-properties (point) (line-end-position))))
                             (push (list path file-line text) tuples))
                           (progn
                             (end-of-line)
                             (let ((cur-point (point)))
                               (ignore-errors (forward-char))
                               (when (equal cur-point (point))
                                 (setq remaining nil))))))

                       (beginning-of-buffer)
                       (re-search-forward (format (regexp-quote tct/grep-code-block-template) "[^\n]+"))
                       (let ((inhibit-read-only t))
                         (delete-region (point) (point-max)))

                       (let ((rows nil)
                             (row-number 0))
                         (dolist (tuple tuples)
                           (cl-incf row-number 1)
                           (seq-let (path file-line text) tuple
                             (let ((location (format "[[%s::%s][%s]] " path file-line (tct/get-displayed-file-path path))))
                               (seq-let
                                   (date content)
                                   (cond
                                    ((save-excursion
                                       (string-match (format tct/keyword-with-date-re-search-template
                                                             "TODO" tct/time-stamp-regex)
                                                     text))
                                     (list (match-string 1 text) (string-trim (match-string 2 text))))
                                    ((save-excursion
                                       (string-match (format tct/keyword-with-period-re-search-template
                                                             "DONE" tct/time-stamp-regex tct/time-stamp-regex)
                                                     text))
                                     (list (match-string 1 text) (string-trim (match-string 2 text))))
                                    ((save-excursion
                                       (string-match (format tct/keyword-fallback-re-search-template
                                                             "TODO")
                                                     text))
                                     (list nil (string-trim (match-string 1 text))))
                                    ((save-excursion
                                       (string-match (format tct/keyword-fallback-re-search-template-no-keyword
                                                             "TODO")
                                                     text))
                                     (list nil (string-trim (match-string 1 text))))
                                    (t
                                     (error "No valid match")))
                                 (push (list (number-to-string row-number) location date content) rows)))))
                         (org-mode)
                         (comment (setq-local org-startup-truncated t))
                         (setq-local truncate-lines t)
                         (todo-comment-table-mode 1)
                         (let ((inhibit-read-only t))
                           (insert (tct/rows-to-org-table '("No." "Location" "Date" "Comment") (reverse rows)))
                           (progn
                             ;; Calling `org-table-align' is necessary,
                             ;; although `tct/rows-to-org-table' already uses `org-table-align'.
                             (org-table-align))
                           (comment
                             (when (fboundp 'valign-mode)
                               (previous-line)
                               (valign-table))))))))))
    (comment (display-buffer buf))
    (display-buffer buf '(display-buffer-same-window))))

(defvar tct/grep-command-template
  "grep -I -n -H -r -o -e '%s' --include='%s' %s"

  ;; The grep options:
  ;; -I : ignore binary files
  ;; -n : print line numbers
  ;; -H : print file paths
  ;; -r : search recursively
  ;; -o : print mached parts only
  ;; -e <pattern> : specify the  pattern to find
  ;; --include=<extension> : specify the file extension to search
  )

(defvar tct/todo-grep-regexp-template "\\(%s\\).*")

(defun tct/get-todo-grep-regexp (keywords)
  (format tct/todo-grep-regexp-template
          (string-join
           (mapcar (lambda (keyword) (format "\\<%s\\>" keyword)) keywords)
           "\\|")))

(defun tct/summarize-todo (&optional regexp files dir)
  "Find TODO items and summarize them."

  (interactive
   (progn
     (grep-compute-defaults)
     (let* ((regexp (tct/get-todo-grep-regexp '("TODO")))
            (files (grep-read-files regexp))
		    (dir (read-directory-name "Base directory: "
					                  nil default-directory t)))
	   (list regexp files dir))))
  (let* ((window (tct/run-command (format tct/grep-command-template regexp files dir)))
         (buffer (window-buffer window)))
    buffer))

(defun tct/summarize-todo-again ()
  "Find TODO items and summarize them again."

  (interactive)

  (let ((command (save-excursion
                   (beginning-of-buffer)
                   (re-search-forward (format (regexp-quote tct/grep-code-block-template) "\\([^\n]+\\)"))
                   (match-string 1))))
    (tct/run-command command)))


(comment
  (unless (fboundp 'tct/after-otc-summarize-todo)
    (defun tct/after-otc-summarize-todo ()
      (comment
        (local-set-key (kbd "C-c D") #'tct/summarize-todo-again))))

  (defun tct/summarize-todo-advice (original &rest args)
    (let ((buffer (apply original args)))
      (set-buffer buffer)
      (tct/after-otc-summarize-todo)
      buffer))

  (advice-add 'tct/summarize-todo
              :around #'tct/summarize-todo-advice))


(defun tct/sort-by-date-column (&optional reversed)
  (interactive "P")

  (save-excursion
   (let ((inhibit-read-only t))
     (beginning-of-buffer)
     (re-search-forward "| \\(Date\\)[^\n|]*|")
     (goto-char (match-beginning 1))
     (next-line 2)
     (org-table-sort-lines nil (if reversed ?T ?t)))))


(defun tct/sort-by-date-column-reversely ()
  (interactive)
  (tct/sort-by-date-column t))


(defun tct/sort-by-number-column (&optional reversed)
  (interactive "P")

  (save-excursion
   (let ((inhibit-read-only t))
     (beginning-of-buffer)
     (re-search-forward "| \\(No.\\)[^\n|]*|")
     (goto-char (match-beginning 1))
     (next-line 2)
     (org-table-sort-lines nil (if reversed ?N ?n)))))


(defun tct/sort-by-number-column-reversely ()
  (interactive)
  (tct/sort-by-number-column t))


(require 'cl-lib)


(defun tct/sort-by-location-date-columns (reversed)
  (interactive "P")
  
  (save-excursion
    (let ((inhibit-read-only t))
      (beginning-of-buffer)
      (re-search-forward "| \\(Location\\)[^\n|]*|")
      (goto-char (match-beginning 1))

      (assert (org-at-table-p))
      (let ((table (org-table-to-lisp))
            (compare
             (if reversed
                 (lambda (row1 row2)
                   (if (not (string= (nth 0 row1) (nth 0 row2)))
                       (progn
                         ;; Currently, the line numbers of links are not considered
                         ;; e.g. [[path::line][repr]]
                         (string> (nth 0 row1) (nth 0 row2)))
                     (not (time-less-p (org-time-string-to-time (nth 1 row1))
                                       (org-time-string-to-time (nth 1 row2))))))
               (lambda (row1 row2)
                 (if (not (string= (nth 0 row1) (nth 0 row2)))
                     (string< (nth 0 row1) (nth 0 row2))
                   (time-less-p (org-time-string-to-time (nth 1 row1))
                                (org-time-string-to-time (nth 1 row2))))))))
        (let ((headers (car table))
              (rows (cddr table)))
          (let ((sorted (cl-sort (cl-copy-seq rows) compare)))
            ;; insert the sorted table
            (org-table-goto-line 0)
            (beginning-of-line)
            (delete-region (point) (org-table-end))
            (insert (tct/rows-to-org-table headers sorted))))))))

(defun tct/undo ()
  (interactive)
  (let ((inhibit-read-only t))
    (undo)))

(comment
  (defface tct/pulse-highlight-face
    '((((class color) (background dark))
       (:background "#AAAA33"))
      (((class color) (background light))
       (:background "#FFFFAA")))
    "Face used during a pulse for display."
    :group 'pulse))

(defun tct/flash-region (beg end &optional face duration)
  "Highlight a region."
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'face (or face 'region))
    (run-with-timer (or duration 0.3) nil
                    (lambda (o) (delete-overlay o)) ov)))

(defvar tct/highlight-pulse-delay 0.5)
(defun tct/pulse-highlight-current-word (&optional face)
  "Highlight the current word briefly."
  (interactive)
  (comment
   (let ((pulse-delay tct/highlight-pulse-delay))
     (let* ((bounds (bounds-of-thing-at-point 'word))
            (beg (car bounds))
            (end (cdr bounds))
            (face (or face 'region)))
       (when bounds
         (pulse-momentary-highlight-region beg end face)))))
  (let* ((bounds (bounds-of-thing-at-point 'word))
            (beg (car bounds))
            (end (cdr bounds))
            (face (or face 'region)))
    (tct/flash-region beg end face tct/highlight-pulse-delay)))

(defun tct/open-at-point-and-goto-keyword (keyword-regex)
  (org-open-at-point)
  (re-search-forward keyword-regex)
  (goto-char (match-beginning 1))
  (comment (pulse-momentary-highlight-one-line (point) 'tct/pulse-highlight-face))
  (comment (pulse-momentary-highlight-one-line (point) 'region))
  (tct/pulse-highlight-current-word 'region))

(defun tct/goto-location ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (re-search-forward "\\[\\[" (line-end-position) t)
    (tct/open-at-point-and-goto-keyword "\\(TODO\\)")))

(defun tct/goto-location-no-visit ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (re-search-forward "\\[\\[" (line-end-position) t)
    (let ((win (selected-window)))
      (tct/open-at-point-and-goto-keyword "\\(TODO\\)")
      (select-window win))))

;; (defun tct/org-open-at-point-no-focus ()
;;   "Open link at point but keep cursor in current window."

;;   (interactive)
;;   (let ((win (selected-window)))
;;     (org-open-at-point)
;;     (select-window win)))

(defun tct/goto-previous-line-location-no-visit ()
  (interactive)
  (let ((link-location
         (save-excursion
           (previous-line)
           (end-of-line)
           (when (re-search-backward "\\[\\[" nil t)
             (point)))))
    (if link-location
        (progn
          (goto-char link-location)
          (let ((win (selected-window)))
            (tct/open-at-point-and-goto-keyword "\\(TODO\\)")
            (select-window win)))
      (error "There is no previous table line."))))

(defun tct/goto-next-line-location-no-visit ()
  (interactive)
  (let ((link-location
         (save-excursion
           (next-line)
           (beginning-of-line)
           (when (re-search-forward "\\[\\[" nil t)
             (point)))))
    (if link-location
        (progn
          (goto-char link-location)
          (let ((win (selected-window)))
            (tct/open-at-point-and-goto-keyword "\\(TODO\\)")
            (select-window win)))
      (error "There is no next table line."))))

(defvar todo-comment-table-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "d") 'tct/sort-by-date-column)
    (define-key map (kbd "D") 'tct/sort-by-date-column-reversely)
    (define-key map (kbd "m") 'tct/sort-by-number-column)
    (define-key map (kbd "M") 'tct/sort-by-number-column-reversely)
    (comment (define-key map (kbd "p") 'previous-line))
    (comment (define-key map (kbd "n") 'next-line))
    (define-key map (kbd "p") 'tct/goto-previous-line-location-no-visit)
    (define-key map (kbd "n") 'tct/goto-next-line-location-no-visit)
    (define-key map (kbd "C-/")'tct/undo)
    (define-key map (kbd "g") 'tct/summarize-todo-again)
    (define-key map (kbd "RET") 'tct/goto-location)
    (define-key map (kbd "M-RET") 'tct/goto-location-no-visit)
    map))

(define-minor-mode todo-comment-table-mode
  "TODO Comment Table Mode."

  nil
  :gloal nil
  :lighter " tct"
  :keymap todo-comment-table-mode-map
  (if todo-comment-table-mode
      (progn
        ;; When enabled
        )
    (progn
      ;; When disabled
      )))


(provide 'todo-comment-table)
