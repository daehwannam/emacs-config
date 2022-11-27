
(comment
  (defcustom python-shell-prompt-pdb-regexp "[(<]*[Ii]?[Pp]db[>)]+ "
    "Regular expression matching pdb input prompt of Python shell.
It should not contain a caret (^) at the beginning."
    :type 'string)

  (defcustom python-pdbtrack-stacktrace-info-regexp
    "> \\([^\"(]+\\)(\\([0-9]+\\))\\([?a-zA-Z0-9_<>]+\\)()"
    "Regular expression matching stacktrace information.
Used to extract the current line and module being inspected.

Must match lines with real filename, like
 > /path/to/file.py(42)<module>()->None
and lines in which filename starts with `<', e.g.
 > <stdin>(1)<module>()->None

In the first case /path/to/file.py file will be visited and overlay icon
will be placed in line 42.
In the second case pdbtracking session will be considered over because
the top stack frame has been reached.

Filename is expected in the first parenthesized expression.
Line number is expected in the second parenthesized expression."
    :type 'string
    :version "27.1"
    :safe 'stringp))

(unless (fboundp 'comment)
  (defmacro comment (&rest args)
    `nil))

(defun pdb-tracking/go-to-current-line ()
  (interactive)
  (let (file-name line-number)
    (save-excursion
      (comment (goto-char (point-max)))
      (comment
        (progn
          ;; Use the below code instead of `move-beginning-of-line',
          ;; whose action is different in `shell-mode'
          (skip-chars-backward "^\n")))
      (goto-char (line-beginning-position))
      (when (and (looking-at python-shell-prompt-pdb-regexp)
                 (re-search-backward python-pdbtrack-stacktrace-info-regexp (comment (- (point) 10000)) t))

        (setq file-name (match-string-no-properties 1))
        (setq line-number (string-to-number (match-string-no-properties 2)))))

    (when (and file-name line-number (not (string-match "<.*>" file-name)))
      (let* ((original-window (selected-window))
            (tracked-buffer (find-file-other-window file-name))
            (tracked-buffer-window (get-buffer-window tracked-buffer))
            (tracked-buffer-line-pos nil))
        (with-current-buffer tracked-buffer
          (set (make-local-variable 'overlay-arrow-position) (make-marker))
          (setq tracked-buffer-line-pos (progn
                                          (goto-char (point-min))
                                          (forward-line (1- line-number))
                                          (recenter nil t)
                                          (point-marker)))
          (when tracked-buffer-window
            (set-window-point
             tracked-buffer-window tracked-buffer-line-pos))
          (set-marker overlay-arrow-position tracked-buffer-line-pos))
        (pop-to-buffer tracked-buffer)
        (select-window original-window)))))

(defun pdb-tracking/trigger ()
  (when (equal this-command (key-binding (kbd "RET")))
    (pdb-tracking/go-to-current-line)))

(progn
  (defvar automatic-tracking-p nil)

  (defun pdb-tracking/toggle-automatic-tracking ()
    (interactive)
    (if automatic-tracking-p
        (progn
          (remove-hook 'post-command-hook #'pdb-tracking/trigger)
          (message "Automatic pdb tracking is disabled"))
      (progn
        (add-hook 'post-command-hook #'pdb-tracking/trigger)
        (message "Automatic pdb tracking is enabled")))
    (setq automatic-tracking-p (not automatic-tracking-p))))

(defvar pdb-tracking-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c-o") 'pdb-tracking/go-to-current-line)
    map)
  "Keymap for `pdb-tracking-mode'.")

(define-minor-mode pdb-tracking-mode
  "Tracking code for PDB "
  nil   ; Initial value, nil for disabled
  :global nil
  :lighter " pdb-tracking"
  :keymap pdb-tracking-mode-map

  (comment
    (if pdb-tracking-mode
        (add-hook 'post-command-hook #'pdb-tracking/trigger)
      (remove-hook 'post-command-hook #'pdb-tracking/trigger))))

(provide 'pdb-tracking)
