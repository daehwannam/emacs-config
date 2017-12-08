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
