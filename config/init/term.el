
(require 'multi-term)
(require 'named-term)

(defalias 'mterm 'multi-term)

(defun tmux-term-1 ()
  "Create new term buffer, then do tmux new session.
It's modified code of 'multi-term.
Will prompt you shell name when you type `C-u' before this command."
  (interactive)
  (let (term-buffer)
    ;; Set buffer.
    (setq term-buffer (multi-term-get-buffer current-prefix-arg))
    (setq multi-term-buffer-list (nconc multi-term-buffer-list (list term-buffer)))
    (set-buffer term-buffer)
    ;; Internal handle for `multi-term' buffer.
    (multi-term-internal)
    ;; Switch buffer
    (switch-to-buffer term-buffer))
  (term-send-raw-string "tmux new \C-m"))

(cl-defun multi-term-tmux-sessions (&optional (user+host nil))
  "multi-term-tmux code is used: https://github.com/beyondmetis/multi-term-tmux"
(interactive)
    (if user+host
	(let* ((sessionlist (shell-command-to-string (concat "ssh " user+host " -q -t -t tmux list-sessions"))) (sessionlist (split-string sessionlist "\n" t)) (sesslist nil)) 
	(dolist (elt sessionlist)
	    (setq sessname (split-string elt ":" t))
	    (setq sessname (nth 0 sessname))
	    (setq sesslist (append sesslist (list sessname))))
	sesslist)

	(let* ((sessionlist (shell-command-to-string "tmux list-sessions")) (sessionlist (split-string sessionlist "\n" t)) (sesslist nil)) 
	(dolist (elt sessionlist)
	    (setq sessname (split-string elt ":" t))
	    (setq sessname (nth 0 sessname))
	    (setq sesslist (append sesslist (list sessname))))
	sesslist)))

(defun multi-term-get-tmux-buffer (session-name &optional special-shell dedicated-window)
  "Get term buffer.
If option SPECIAL-SHELL is `non-nil', will use shell from user input.
If option DEDICATED-WINDOW is `non-nil' will create dedicated `multi-term' window ."
  (with-temp-buffer
    (let ((shell-name (or multi-term-program ;shell name
                          (getenv "SHELL")
                          (getenv "ESHELL")
                          "/bin/sh"))
          (index 1)                     ;setup new term index
          term-name)                    ;term name
      (if dedicated-window
          (setq term-name multi-term-dedicated-buffer-name)
        ;; Compute index.
        (while (buffer-live-p (get-buffer (format "*%s<%s>*" multi-term-buffer-name index)))
          (setq index (1+ index)))
        ;; switch to current local directory,
        ;; if in-existence, switch to `multi-term-default-dir'.
        (cd (or default-directory (expand-file-name multi-term-default-dir)))
        ;; adjust value N when max index of term buffer is less than length of term list
        (setq term-name (format "%s<%s>" multi-term-buffer-name index)))
      ;; Try get other shell name if `special-shell' is non-nil.
      (if special-shell
          (setq shell-name (read-from-minibuffer "Run program: " shell-name)))
      ;; Make term, details to see function `make-term' in `term.el'.
      (if session-name
	  (if (member session-name (multi-term-tmux-sessions))
	      (make-term term-name "tmux" nil "-L" "emacs-term" "a" "-t" session-name)
	    (make-term term-name "tmux" nil "-L" "emacs-term" "new" "-s" session-name))
	(make-term term-name "tmux" nil "-L" "emacs-term" "new"))
      ;; (if multi-term-program-switches
      ;;     (make-term term-name shell-name nil multi-term-program-switches)
      ;;   (make-term term-name shell-name))
      )))

(defun tmux-term (&optional session-name)
  "Create new term buffer, then do tmux new session.
It's modified code of 'multi-term.
Will prompt you shell name when you type `C-u' before this command."
  (interactive (list (read-from-minibuffer "Session name: ")))
  (when (string= session-name "")
    (setq session-name nil))
  (let (term-buffer)
    ;; Set buffer.
    (setq term-buffer (multi-term-get-tmux-buffer session-name current-prefix-arg))
    (setq multi-term-buffer-list (nconc multi-term-buffer-list (list term-buffer)))
    (set-buffer term-buffer)
    ;; Internal handle for `multi-term' buffer.
    (multi-term-internal)
    ;; Switch buffer
    (switch-to-buffer term-buffer)))

(defalias 'tterm 'tmux-term)
;; (require 'multi-term-tmux)
;; (defalias 'tterm 'multi-term-tmux-open)

;; tmux command
;;
;; string join: https://stackoverflow.com/questions/12999530/is-there-a-function-that-joins-a-string-into-a-delimited-string
(defun tmux-kill-numbered-sessions ()
  (interactive)
  (let ((session-names (split-string (trim-string (shell-command-to-string
						   "tmux ls | grep '^[0-9][0-9]*: ' | awk '{print substr($1, 0, length($1))}' | xargs echo")))))
    (dolist (session-name session-names)
      (shell-command (concat "tmux kill-session -t " session-name)))
    (message (concat "killed sessions: " (mapconcat 'identity session-names ", ")))))

(defun tmux-kill-all-emacs-term-sessions ()
  (interactive)
  (shell-command "tmux -L emacs-term kill-server"))
