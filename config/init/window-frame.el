
(progn
  ;; http://stackoverflow.com/questions/5046597/changing-window-faster-in-emacs-or-repeating-last-shortcut-with-a-single-strike
  ;;(when (fboundp 'windmove-default-keybindings)
  ;;  (windmove-default-keybindings))

  ;; https://www.emacswiki.org/emacs/WindMove
  ;;(global-set-key (kbd "C-c <left>")  'windmove-left)
  ;;(global-set-key (kbd "C-c <right>") 'windmove-right)
  ;;(global-set-key (kbd "C-c <up>")    'windmove-up)
  ;;(global-set-key (kbd "C-c <down>")  'windmove-down)

;;; other window with repeatition
  ;; http://stackoverflow.com/questions/91071/emacs-switch-to-previous-window

  (defun dhnam/other-window-backwards () (interactive) (other-window -1))
  (defun dhnam/other-frame-backwards () (interactive) (other-frame -1))

  (progn
    (defun dhnam/split-move-window-below (&optional size)
      (interactive "P")
      (split-window-below size)
      (windmove-down))

    (defun dhnam/split-move-window-right (&optional size)
      (interactive "P")
      (split-window-right size)
      (windmove-right)))

  ;; (load "make-repeatable-command") ;(load "~/.emacs.d/package/make-repeatable-command.el")
  (require 'dhnam-make-repeatable-command)
  (progn
    (comment
      (global-set-key (kbd "C-x o") (make-repeatable-command 'other-window))
      (global-set-key (kbd "C-x O") (make-repeatable-command 'dhnam/other-window-backwards))
      (global-set-key (kbd "C-x 5 o") (make-repeatable-command 'other-frame))
      (global-set-key (kbd "C-x 5 O") (make-repeatable-command 'dhnam/other-frame-backwards))

      (global-set-key (kbd "C-x ㅐ") (make-repeatable-command 'other-window)))

    (progn
      (global-set-key (kbd "C-x 9") 'delete-other-windows))

    (progn
      (defhydra dhnam/window-navigation ()
        "Window navigation"

        ("o" other-window)
        ("i" dhnam/other-window-backwards)
        ;; ("RET" nil "quit")
        ;; ("q" nil "quit")
        ;; ("SPC" nil "quit")
        )

      (hydra-set-property 'dhnam/window-navigation :verbosity 0)

      (global-set-key (kbd "C-x o") 'dhnam/window-navigation/other-window)
      ;; "C-x i" is originally mapped to `insert-file'
      (global-set-key (kbd "C-x i") 'dhnam/window-navigation/dhnam/other-window-backwards)
      (global-set-key (kbd "C-x I") 'insert-file)

      (global-set-key (kbd "C-x ㅐ") 'dhnam/window-navigation/other-window)
      (global-set-key (kbd "C-x ㅑ") 'dhnam/window-navigation/dhnam/other-window-backwards))

    (comment
      (unless (and (boundp 'exwm-split-defined) exwm-split-defined)
        (global-set-key (kbd "C-x 2") 'dhnam/split-move-window-below)
        (global-set-key (kbd "C-x 3") 'dhnam/split-move-window-right)

        (global-set-key (kbd "C-x 8") 'dhnam/split-move-window-below)
        (global-set-key (kbd "C-x 7") 'dhnam/split-move-window-right)))

    (progn
      (global-set-key (kbd "C-x 8") 'split-window-below)
      (global-set-key (kbd "C-x 7") 'split-window-right))

    (comment
      ;; other-frame key setting
      (global-set-key (kbd "C-c o") (make-repeatable-command 'other-frame))
      (global-set-key (kbd "C-c O") (make-repeatable-command 'dhnam/other-frame-backwards))

      (global-set-key (kbd "C-c ㅐ") (make-repeatable-command 'other-frame)))

    (progn
      (defhydra dhnam/frame-navigation ()
        "Frame navigation"

        ("o" other-frame)
        ("i" dhnam/other-frame-backwards)
        ;; ("RET" nil "quit")
        ;; ("q" nil "quit")
        ;; ("SPC" nil "quit")
        )

      (hydra-set-property 'dhnam/frame-navigation :verbosity 0)

      (global-set-key (kbd "C-c o") 'dhnam/frame-navigation/other-frame)
      (global-set-key (kbd "C-c i") 'dhnam/frame-navigation/dhnam/other-frame-backwards)

      (global-set-key (kbd "C-c ㅐ") 'dhnam/frame-navigation/other-frame)
      (global-set-key (kbd "C-c ㅑ") 'dhnam/frame-navigation/dhnam/other-frame-backwards))

    (progn
      ;; make-frame-command & delete-frame key setting
      (global-set-key (kbd "C-c 0") 'delete-frame)
      (global-set-key (kbd "C-c 1") 'delete-other-frames)
      (global-set-key (kbd "C-c 2") 'make-frame-command)

      (global-set-key (kbd "C-c 9") 'delete-other-frames)
      (global-set-key (kbd "C-c 8") 'make-frame-command)))

  (progn
    (comment
      (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "o") (make-repeatable-command 'other-window))
	    (define-key map (kbd "O") (make-repeatable-command 'dhnam/other-window-backwards))
	    (define-key map (kbd "5 o") (make-repeatable-command 'other-frame))
	    (define-key map (kbd "5 O") (make-repeatable-command 'dhnam/other-frame-backwards))

	    (define-key map (kbd "ㅐ") (make-repeatable-command 'other-window))

	    (define-key map (kbd "9") 'delete-other-windows)
	    (define-key map (kbd "8") 'split-window-below)
	    (define-key map (kbd "7") 'split-window-right)

	    (define-key map (kbd "0") 'delete-window)

	    (defvar window-modification-prefix-map map
	      "Keymap for window related commands."))
      (fset 'window-modification-prefix-map window-modification-prefix-map)
      (key-chord-define-global "ff" 'window-modification-prefix-map))

    (comment
      (let ((map (make-sparse-keymap)))
	    ;; other-frame key setting
	    (define-key map (kbd "o") (make-repeatable-command 'other-frame))
	    (define-key map (kbd "O") (make-repeatable-command 'dhnam/other-frame-backwards))

	    (define-key map (kbd "ㅐ") (make-repeatable-command 'other-frame))

	    ;; make-frame-command & delete-frame key setting
	    (define-key map (kbd "0") 'delete-frame)
	    (define-key map (kbd "1") 'delete-other-frames)
	    (define-key map (kbd "2") 'make-frame-command)

	    (define-key map (kbd "9") 'delete-other-frames)
	    (define-key map (kbd "8") 'make-frame-command)

	    (defvar frame-modification-prefix-map map
	      "Keymap for frame related commands."))
      (fset 'frame-modification-prefix-map frame-modification-prefix-map)
      (key-chord-define-global "rr" 'frame-modification-prefix-map)))

  (comment
    (when (and (boundp 'dhnam/exwm-cmd-line-arg-passed) dhnam/exwm-cmd-line-arg-passed)
      (global-set-key (kbd "C-c o") (make-repeatable-command 'dhnam/exwm-other-workspace-in-group))
      (global-set-key (kbd "C-c O") (make-repeatable-command 'dhnam/exwm-other-workspace-in-group-backwards))
      (global-set-key (kbd "C-c ㅐ") (make-repeatable-command 'dhnam/exwm-other-workspace-in-group))))

  (progn
    ;; window size adjust
    (global-set-key (kbd "C-x ^") (make-repeatable-command 'enlarge-window))
    (global-set-key (kbd "C-x %") (make-repeatable-command 'shrink-window))
    (global-set-key (kbd "C-x }") (make-repeatable-command 'enlarge-window-horizontally))
    (global-set-key (kbd "C-x {") (make-repeatable-command 'shrink-window-horizontally))))

(comment
  (progn
    (global-unset-key (kbd "C-x o"))
    (global-unset-key (kbd "C-x 0"))
    (global-unset-key (kbd "C-x 1"))
    
    (global-unset-key (kbd "C-x 2"))
    (global-unset-key (kbd "C-x 3"))

    (global-unset-key (kbd "C-c o"))
    (global-unset-key (kbd "C-c 0"))
    (global-unset-key (kbd "C-c 1"))
    (global-unset-key (kbd "C-c 2")))

  (key-chord-define-global "j4" 'delete-window)
  (key-chord-define-global "j1" 'delete-other-windows)
  (key-chord-define-global "j2" 'split-window-below)
  (key-chord-define-global "j3" 'split-window-right)
  (progn
    (key-chord-define-global "jx" 'other-window)
    (key-chord-define-global "jc" 'dhnam/other-window-backwards))
  (comment
    (key-chord-define-global "jf" 'other-window)
    (key-chord-define-global "jd" 'dhnam/other-window-backwards))

  (key-chord-define-global "k4" 'delete-frame)
  (key-chord-define-global "k1" 'delete-other-frames)
  (key-chord-define-global "k2" 'make-frame-command)
  (progn
    (key-chord-define-global "kx" 'other-frame)
    (key-chord-define-global "kc" 'dhnam/other-frame-backwards))
  (comment
    (key-chord-define-global "kf" 'other-frame)
    (key-chord-define-global "kd" 'dhnam/other-frame-backwards)))

(comment
  (global-unset-key (kbd "C-x 1"))
  (global-unset-key (kbd "C-x 2"))
  (global-unset-key (kbd "C-x 3"))

  (global-unset-key (kbd "C-c 1"))
  (global-unset-key (kbd "C-c 2")))

;; kill buffer and delete window
(defun dhnam/kill-buffer-and-delete-window ()
  (interactive)
  (kill-buffer)
  (delete-window))

;; (global-set-key (kbd "C-x K") 'dhnam/kill-buffer-and-delete-window)

;; kill buffer and delete frame
(defun dhnam/kill-buffer-and-delete-frame ()
  (interactive)
  (kill-buffer)
  (delete-frame))

                                        ;(global-set-key (kbd "C-c K") 'dhnam/kill-buffer-and-delete-frame)

;; kill other window buffer
(defun dhnam/kill-other-window-buffer ()
  (interactive)
  (let ((curr-window (selected-window)))
    (other-window 1)
    (kill-buffer)
    (unless (equal (selected-window) curr-window)
      (other-window -1))))

(global-set-key (kbd "C-x K") 'dhnam/kill-other-window-buffer)

;; kill other window
(defun dhnam/delete-other-window ()
  (interactive)
  (other-window 1)
  (delete-window))

(comment
  (global-set-key (kbd "C-x 9") 'dhnam/delete-other-window))

;; split windows below or right
(defun dhnam/split-window-below-or-right-and-find-file (filename &optional wildcards)
  (interactive
   (find-file-read-args "Find file in other window: "
                        (confirm-nonexistent-file-or-buffer)))
  (if (< (* (window-body-height) 2) (window-body-width))
      (split-window-right)
    (split-window-below))
  (other-window 1)
  (find-file filename))

(comment (global-set-key (kbd "C-x F") 'dhnam/split-window-below-or-right-and-find-file))


(defun dhnam/split-window-below-or-right-and-execute-extended-command (prefixarg &optional command-name)
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
(comment (global-set-key (kbd "M-X") 'dhnam/split-window-below-or-right-and-execute-extended-command))


(defun dhnam/split-window-below-or-right-and-switch-to-buffer (buffer-or-name &optional norecord force-same-window)
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

(comment (global-set-key (kbd "C-x B") 'dhnam/split-window-below-or-right-and-switch-to-buffer))


(comment
  (when (fboundp 'god-mode)
    ;; (global-set-key (kbd "C-x C-o") (make-repeatable-command 'other-window))
    ;; (global-set-key (kbd "C-x C-5 C-o") (make-repeatable-command 'other-frame))

    ;; (global-set-key (kbd "C-x C-ㅐ") (make-repeatable-command 'other-window))

    ;; ;; other-frame key setting
    ;; (global-set-key (kbd "C-c C-o") (make-repeatable-command 'other-frame))

    ;; (global-set-key (kbd "C-c C-ㅐ") (make-repeatable-command 'other-frame))

    ;; ;; make-frame-command & delete-frame key setting
    ;; (global-set-key (kbd "C-c C-0") 'delete-frame)
    ;; (global-set-key (kbd "C-c C-1") 'delete-other-frames)
    ;; (global-set-key (kbd "C-c C-2") 'make-frame-command)

    ;; window size adjust (NOT WORK)
    (global-set-key (kbd "C-x C-^") (make-repeatable-command 'enlarge-window))
    (global-set-key (kbd "C-x C-%") (make-repeatable-command 'shrink-window))
    (global-set-key (kbd "C-x C-}") (make-repeatable-command 'enlarge-window-horizontally))
    (global-set-key (kbd "C-x C-{") (make-repeatable-command 'shrink-window-horizontally))))

(progn
  ;; Disable `pop-to-buffe' to resize widnows
  ;; For example, `pop-to-buffe' is used by `find-file-other-window'
  ;;
  ;; Https://stackoverflow.com/a/11458625

  (setq even-window-heights nil))

(comment
  (progn
    ;; Horizontal split
    ;; https://emacs.stackexchange.com/a/40517

    (defun split-window-sensibly-prefer-horizontal (&optional window)
      "Based on split-window-sensibly, but designed to prefer a horizontal split,
i.e. windows tiled side-by-side."
      (let ((window (or window (selected-window))))
        (or (and (window-splittable-p window t)
                 ;; Split window horizontally
                 (with-selected-window window
                   (split-window-right)))
            (and (window-splittable-p window)
                 ;; Split window vertically
                 (with-selected-window window
                   (split-window-below)))
            (and
             ;; If WINDOW is the only usable window on its frame (it is
             ;; the only one or, not being the only one, all the other
             ;; ones are dedicated) and is not the minibuffer window, try
             ;; to split it horizontally disregarding the value of
             ;; `split-height-threshold'.
             (let ((frame (window-frame window)))
               (or
                (eq window (frame-root-window frame))
                (catch 'done
                  (walk-window-tree (lambda (w)
                                      (unless (or (eq w window)
                                                  (window-dedicated-p w))
                                        (throw 'done nil)))
                                    frame)
                  t)))
             (not (window-minibuffer-p window))
             (let ((split-width-threshold 0))
               (when (window-splittable-p window t)
                 (with-selected-window window
                   (split-window-right))))))))

    (defun split-window-really-sensibly (&optional window)
      (let ((window (or window (selected-window))))
        (if (> (window-total-width window) (* 2 (window-total-height window)))
            (with-selected-window window (split-window-sensibly-prefer-horizontal window))
          (with-selected-window window (split-window-sensibly window)))))

    (comment (setq split-height-threshold 4))
    (comment (setq split-width-threshold 40))
    (setq split-window-preferred-function 'split-window-really-sensibly)))

(comment
  (progn
    ;; Always vertical split
    ;; https://stackoverflow.com/a/43972522

    (setq split-width-threshold 0)
    (setq split-height-threshold nil)))


(provide 'init-window-frame)
