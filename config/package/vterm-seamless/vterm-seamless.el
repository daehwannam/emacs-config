;; vterm-seamless-mode

(unless (fboundp 'comment)
  (defmacro comment (&rest args)
    `nil))

(progn
  ;; Functions
  (defun vtsl/copy-mode-then (cmd)
    (let ((func-symbol (intern (concat "vtsl/copy-mode-then-" (symbol-name cmd)))))
      (fset func-symbol
            `(lambda
               ;; ,(help-function-arglist cmd) ;; arg list
               (&rest args)
               ,(format "Switch to `vterm-copy-mode' then call `%s'." (symbol-name cmd)) ;; doc string
               ,(interactive-form cmd) ;; interactive form
               (vterm-copy-mode 1)
               (apply #',cmd args)))
      func-symbol))

  (defun vtsl/copy-mode-exit-then (cmd)
    (let ((func-symbol (intern (concat "vtsl/copy-mode-exit-then-" (symbol-name cmd)))))
      (fset func-symbol
            `(lambda
               ;; ,(help-function-arglist cmd) ;; arg list
               (&rest args)
               ,(format "Exit `vterm-copy-mode' then call `%s'." (symbol-name cmd)) ;; doc string
               ,(interactive-form cmd) ;; interactive form
               (vtsl/copy-mode-exit)
               (apply #',cmd args)))
      func-symbol))

  (defvar vtls/prompt-regexp-list
    (list "\\$"                                ; normal prompt
          "[(<]*[Ii]?[Pp]db\\(\\+\\+\\)?[>)]+" ; Pdb, Pdb++, ipdb
          ))

  (defvar vtls/prompt-separator-regex " \\|\n")

  (defun vtsl/get-prompt-regexp (&optional other-regexp-list)
    (string-join (mapcar (lambda (s) (format "\\(%s\\)" s))
                         (append vtls/prompt-regexp-list other-regexp-list))
                 "\\|"))

  (defun vtsl/trigger-copy-mode-exit ()
    (when (= (line-number-at-pos (point)) vtsl/last-cursor-line-num)
      (vterm-copy-mode 0))))

(progn
  ;; Interactive functions
  (defun vtsl/copy-mode-exit ()
    (interactive)
    "Exit `vterm-copy-mode'"
    (vterm-copy-mode -1))

  (defun vtsl/kill-line ()
    (interactive)
    ;; https://www.emacswiki.org/emacs/CopyingWholeLines
    (let ((beg (point))
          (end (line-end-position)))
      (kill-ring-save beg end))
    (vterm--self-insert))

  (defun vterm-send-meta-d ()
    "Send `M-d' to the libvterm."
    (interactive)
    (vterm-send-key "d" nil t))

  (defun vterm-send-ctrl-backspace ()
    "Send `C-<backspace>' to the libvterm."
    (interactive)
    (vterm-send-key "<backspace>" nil nil t))

  (defun vterm-send-ctrl-a-and-delete ()
    (interactive)
    (vterm-send-key "a" nil nil t)
    (vterm-send-delete))

  (defun vterm-send-ctrl-a-and-meta-d ()
    (interactive)
    (vterm-send-key "a" nil nil t)
    (vterm-send-meta-d))

  (defun vtsl/previous-prompt (&optional forward)
    (interactive)
    (let* ((unified-prompt-regexp (vtsl/get-prompt-regexp))
           (original-point (point))
           (new-point
            (save-excursion
              (unless forward
                (move-beginning-of-line 1))
              (if (funcall (if forward 're-search-forward 're-search-backward)
                           (vtsl/get-prompt-regexp)
                           nil t)
                  (progn
                    (re-search-forward vtls/prompt-separator-regex nil t)
                    (when (equal (match-string 0) "\n")
                      (backward-char))
                    (point))
                original-point))))
      (goto-char new-point)))

  (defun vtsl/next-prompt ()
    (interactive)
    (vtsl/previous-prompt t)))

(defvar vterm-seamless-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-k")     #'vtsl/kill-line)
    (define-key map (kbd "C-_")     #'vterm-undo)
    (define-key map (kbd "M-p")     #'vterm-send-up)
    (define-key map (kbd "M-n")     #'vterm-send-down)
    (define-key map (kbd "C-p")     (vtsl/copy-mode-then 'previous-line))
    (define-key map (kbd "C-n")     (vtsl/copy-mode-then 'next-line))
    ;; (define-key map (kbd "C-r")     (vtsl/copy-mode-then 'isearch-backward))
    ;; (define-key map (kbd "C-s")     (vtsl/copy-mode-then 'isearch-forward))
    (define-key map (kbd "C-c C-p") (vtsl/copy-mode-then 'vtsl/previous-prompt))
    (define-key map (kbd "C-c C-n") (vtsl/copy-mode-then 'vtsl/next-prompt))
    (define-key map (kbd "M-<")     (vtsl/copy-mode-then 'beginning-of-buffer))
    (define-key map (kbd "C-l")     #'recenter-top-bottom)
    (define-key map (kbd "M-r")     #'move-to-window-line-top-bottom)

    map)
  "Keymap for `vterm-seamless-mode'.")

(define-minor-mode vterm-seamless-mode
  "vterm-mode extension"
  nil                          ; Initial value, nil for disabled
  :global nil
  :lighter " vtsl"
  :keymap vterm-seamless-mode-map)

(defvar vterm-seamless-copy-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map (kbd "C-c C-k") #'vterm-copy-mode-done)
    (define-key map (kbd "C-c C-k")             #'vtsl/copy-mode-exit)
    (define-key map (kbd "RET")                 #'vtsl/copy-mode-exit)
    (define-key map (kbd "<return>")            #'vtsl/copy-mode-exit)
    (define-key map (kbd "M-j")                 #'vtsl/copy-mode-exit)
    (define-key map (kbd "C-c C-p")             #'vtsl/previous-prompt)
    (define-key map (kbd "C-c C-n")             #'vtsl/next-prompt)
    (define-key map (kbd "M->")                 #'vterm-reset-cursor-point)
    (define-key map [remap self-insert-command] (vtsl/copy-mode-exit-then 'vterm--self-insert))
    (define-key map (kbd "C-y")                 (vtsl/copy-mode-exit-then 'vterm-yank))
    (define-key map (kbd "DEL")                 (vtsl/copy-mode-exit-then 'vterm-send-backspace))
    (define-key map (kbd "<backspace>")         (vtsl/copy-mode-exit-then 'vterm-send-backspace))
    (define-key map (kbd "M-DEL")               (vtsl/copy-mode-exit-then 'vterm-send-meta-backspace))
    (define-key map (kbd "<M-backspace>")       (vtsl/copy-mode-exit-then 'vterm-send-meta-backspace))
    (define-key map (kbd "<C-backspace>")       (vtsl/copy-mode-exit-then 'vterm-send-meta-backspace))
    (define-key map (kbd "C-d")                 (vtsl/copy-mode-exit-then 'vterm-send-ctrl-a-and-delete))
    (define-key map (kbd "M-d")                 (vtsl/copy-mode-exit-then 'vterm-send-ctrl-a-and-meta-d))

    ;; key-chords
    (key-chord-define vterm-mode-map "fj" (vtsl/copy-mode-then 'ctrlf-backward-default))

    map))

(defvar vtsl/last-cursor-point nil)
(defvar vtsl/last-cursor-line-num nil)

(define-minor-mode vterm-seamless-copy-mode
  "vterm-mode extension"
  nil                                ; Initial value, nil for disabled
  :global nil
  :lighter " vtsl-copy"
  :keymap vterm-seamless-copy-mode-map

  (make-local-variable 'vtsl/last-cursor-point)

  (if vterm-seamless-copy-mode
      (progn
        (setq-local vtsl/last-cursor-point
                    (save-excursion
                      (vterm-reset-cursor-point)
                      (point)))
        (setq-local vtsl/last-cursor-line-num
                    (line-number-at-pos vtsl/last-cursor-point))

        (add-hook 'post-command-hook 'vtsl/trigger-copy-mode-exit))

    (progn
      (setq-local vtsl/last-cursor-point nil)
      (setq-local vtsl/last-cursor-line-num nil)
      (remove-hook 'post-command-hook 'vtsl/trigger-copy-mode-exit ))))

(progn
  (defun vtsl/mode-switch ()
    ;; https://emacs.stackexchange.com/a/47092
    (if vterm-copy-mode
        (progn
          (vterm-seamless-mode 0)
          (vterm-seamless-copy-mode 1))
      (progn
        (vterm-seamless-mode 1)
        (vterm-seamless-copy-mode 0))))

  (defun vtsl/activate ()
    (interactive)
    (when (eq major-mode 'vterm-mode)
      (vtsl/mode-switch)
      (add-hook 'vterm-copy-mode-hook 'vtsl/mode-switch 0 t)))

  (defun vtsl/deactivate ()
    (interactive)
    (when (eq major-mode 'vterm-mode)
      (vterm-seamless-mode 0)
      (vterm-seamless-copy-mode 0)
      (remove-hook 'vterm-copy-mode-hook 'vtsl/mode-switch t))))

(provide 'vterm-seamless)
