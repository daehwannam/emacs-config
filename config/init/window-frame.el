;;; other-window
;; http://stackoverflow.com/questions/5046597/changing-window-faster-in-emacs-or-repeating-last-shortcut-with-a-single-strike
;(when (fboundp 'windmove-default-keybindings)
;  (windmove-default-keybindings))

;; https://www.emacswiki.org/emacs/WindMove
;(global-set-key (kbd "C-c <left>")  'windmove-left)
;(global-set-key (kbd "C-c <right>") 'windmove-right)
;(global-set-key (kbd "C-c <up>")    'windmove-up)
;(global-set-key (kbd "C-c <down>")  'windmove-down)

;;; other window with repeatition
;; http://stackoverflow.com/questions/91071/emacs-switch-to-previous-window

(defun other-window-backwards () (interactive) (other-window -1))
(defun other-frame-backwards () (interactive) (other-frame -1))

;(load "make-repeatable-command") ;(load "~/.emacs.d/package/make-repeatable-command.el")
(require 'make-repeatable-command)
(global-set-key (kbd "C-x o") (make-repeatable-command 'other-window))
(global-set-key (kbd "C-x O") (make-repeatable-command 'other-window-backwards))
(global-set-key (kbd "C-x 5 o") (make-repeatable-command 'other-frame))
(global-set-key (kbd "C-x 5 O") (make-repeatable-command 'other-frame-backwards))

(global-set-key (kbd "C-x ㅐ") (make-repeatable-command 'other-window))

;; other-frame key setting
(global-set-key (kbd "C-c o") (make-repeatable-command 'other-frame))
(global-set-key (kbd "C-c O") (make-repeatable-command 'other-frame-backwards))

(global-set-key (kbd "C-c ㅐ") (make-repeatable-command 'other-frame))

;; make-frame-command & delete-frame key setting
(global-set-key (kbd "C-c 0") 'delete-frame)
(global-set-key (kbd "C-c 1") 'delete-other-frames)
(global-set-key (kbd "C-c 2") 'make-frame-command)

;; window size adjust
(global-set-key (kbd "C-x ^") (make-repeatable-command 'enlarge-window))
(global-set-key (kbd "C-x %") (make-repeatable-command 'shrink-window))
(global-set-key (kbd "C-x }") (make-repeatable-command 'enlarge-window-horizontally))
(global-set-key (kbd "C-x {") (make-repeatable-command 'shrink-window-horizontally))


;; kill buffer and delete window
(defun kill-buffer-and-delete-window ()
  (interactive)
  (kill-buffer)
  (delete-window))

(global-set-key (kbd "C-x K") 'kill-buffer-and-delete-window)

;; kill buffer and delete frame
(defun kill-buffer-and-delete-frame ()
  (interactive)
  (kill-buffer)
  (delete-frame))

;(global-set-key (kbd "C-c K") 'kill-buffer-and-delete-frame)

;; split windows below or right
(defun split-window-below-or-right-and-find-file (filename &optional wildcards)
  (interactive
   (find-file-read-args "Find file in other window: "
                        (confirm-nonexistent-file-or-buffer)))
  (if (< (* (window-body-height) 2) (window-body-width))
      (split-window-right)
    (split-window-below))
  (other-window 1)
  (find-file filename))

(global-set-key (kbd "C-x F") 'split-window-below-or-right-and-find-file)


(defun split-window-below-or-right-and-execute-extended-command (prefixarg &optional command-name)
  ;; Based on Fexecute_extended_command in keyboard.c of Emacs.
  ;; Aaron S. Hawley <aaron.s.hawley(at)gmail.com> 2009-08-24
  "Read function name, then read its arguments and call it.

To pass a numeric argument to the command you are invoking, specify
the numeric argument to this command.

Noninteractively, the argument PREFIXARG is the prefix argument to
give to the command you invoke, if it asks for an argument."
  (interactive (list current-prefix-arg (read-extended-command)))
  ;; Emacs<24 calling-convention was with a single `prefixarg' argument.

  ;; split windows below or right
  (if (< (* (window-body-height) 2) (window-body-width))
      (split-window-right)
    (split-window-below))
  (other-window 1)

  (if (null command-name)
      (setq command-name (let ((current-prefix-arg prefixarg)) ; for prompt
                           (read-extended-command))))
  (let* ((function (and (stringp command-name) (intern-soft command-name)))
         (binding (and suggest-key-bindings
		       (not executing-kbd-macro)
		       (where-is-internal function overriding-local-map t))))
    (unless (commandp function)
      (error "`%s' is not a valid command name" command-name))
    (setq this-command function)
    ;; Normally `real-this-command' should never be changed, but here we really
    ;; want to pretend that M-x <cmd> RET is nothing more than a "key
    ;; binding" for <cmd>, so the command the user really wanted to run is
    ;; `function' and not `execute-extended-command'.  The difference is
    ;; visible in cases such as M-x <cmd> RET and then C-x z (bug#11506).
    (setq real-this-command function)
    (let ((prefix-arg prefixarg))
      (command-execute function 'record))
    ;; If enabled, show which key runs this command.
    (when binding
      ;; But first wait, and skip the message if there is input.
      (let* ((waited
              ;; If this command displayed something in the echo area;
              ;; wait a few seconds, then display our suggestion message.
              (sit-for (cond
                        ((zerop (length (current-message))) 0)
                        ((numberp suggest-key-bindings) suggest-key-bindings)
                        (t 2)))))
        (when (and waited (not (consp unread-command-events)))
          (with-temp-message
              (format "You can run the command `%s' with %s"
                      function (key-description binding))
            (sit-for (if (numberp suggest-key-bindings)
                         suggest-key-bindings
                       2))))))))
(global-set-key (kbd "M-X") 'split-window-below-or-right-and-execute-extended-command)


(defun split-window-below-or-right-and-switch-to-buffer (buffer-or-name &optional norecord force-same-window)
  "Display buffer BUFFER-OR-NAME in the selected window.

WARNING: This is NOT the way to work on another buffer temporarily
within a Lisp program!  Use `set-buffer' instead.  That avoids
messing with the window-buffer correspondences.

If the selected window cannot display the specified
buffer (e.g. if it is a minibuffer window or strongly dedicated
to another buffer), call `pop-to-buffer' to select the buffer in
another window.

If called interactively, read the buffer name using the
minibuffer.  The variable `confirm-nonexistent-file-or-buffer'
determines whether to request confirmation before creating a new
buffer.

BUFFER-OR-NAME may be a buffer, a string (a buffer name), or nil.
If BUFFER-OR-NAME is a string that does not identify an existing
buffer, create a buffer with that name.  If BUFFER-OR-NAME is
nil, switch to the buffer returned by `other-buffer'.

If optional argument NORECORD is non-nil, do not put the buffer
at the front of the buffer list, and do not make the window
displaying it the most recently selected one.

If optional argument FORCE-SAME-WINDOW is non-nil, the buffer
must be displayed in the selected window; if that is impossible,
signal an error rather than calling `pop-to-buffer'.

The option `switch-to-buffer-preserve-window-point' can be used
to make the buffer appear at its last position in the selected
window.

Return the buffer switched to."
  (interactive
   (list (read-buffer-to-switch "Switch to buffer: ") nil 'force-same-window))

  ;; split windows below or right
  (if (< (* (window-body-height) 2) (window-body-width))
      (split-window-right)
    (split-window-below))
  (other-window 1)

  (let ((buffer (window-normalize-buffer-to-switch-to buffer-or-name)))
    (cond
     ;; Don't call set-window-buffer if it's not needed since it
     ;; might signal an error (e.g. if the window is dedicated).
     ((eq buffer (window-buffer)))
     ((window-minibuffer-p)
      (if force-same-window
          (user-error "Cannot switch buffers in minibuffer window")
        (pop-to-buffer buffer norecord)))
     ((eq (window-dedicated-p) t)
      (if force-same-window
          (user-error "Cannot switch buffers in a dedicated window")
        (pop-to-buffer buffer norecord)))
     (t
      (let* ((entry (assq buffer (window-prev-buffers)))
	     (displayed (and (eq switch-to-buffer-preserve-window-point
				 'already-displayed)
			     (get-buffer-window buffer 0))))
	(set-window-buffer nil buffer)
	(when (and entry
		   (or (eq switch-to-buffer-preserve-window-point t)
		       displayed))
	  ;; Try to restore start and point of buffer in the selected
	  ;; window (Bug#4041).
	  (set-window-start (selected-window) (nth 1 entry) t)
	  (set-window-point nil (nth 2 entry))))))

    (unless norecord
      (select-window (selected-window)))
    (set-buffer buffer)))

(global-set-key (kbd "C-x B") 'split-window-below-or-right-and-switch-to-buffer)
