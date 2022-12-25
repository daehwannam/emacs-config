
(when (fboundp 'persp-mode)
  (require 'dhnam-make-repeatable-command)

  ;;; change prefix-key
  ;; (setq persp-mode-prefix-key (kbd "C-z"))

  ;;; change default perspective name
  (setq persp-initial-frame-name "p0")	;default = main

  (defvar persp-count 1
    "Counter for the created perspectives")

  (defvar default-persp-name "p")

  (defun next-persp-name ()
    (let* ((name (concat default-persp-name (number-to-string persp-count)))
	   (persp (gethash name perspectives-hash)))
      (while (not (null persp))
	(setq persp-count (1+ persp-count))
	(setq name (concat default-persp-name (number-to-string persp-count)))
	(setq persp (gethash name perspectives-hash)))
      name))

    ;; ;;; rename the first perspective
    ;; (persp-rename (next-persp-name))
    ;; (setq persp-count (1+ persp-count))

  ;;; enable persp-mode
  (persp-mode)

  ;;; open new perspective (modified version of persp-switch
  (defun persp-open (name)
    "Create a new perspective"
    (interactive (list
		  (read-string (format "New perspective name (%s): " (next-persp-name))
			       nil nil (next-persp-name))))
    (persp-switch name))

  ;;; customized keys
  (define-key perspective-map (kbd "2") 'persp-open)

  (define-key perspective-map (kbd "n") (make-repeatable-command 'persp-next))
  (define-key perspective-map (kbd "p") (make-repeatable-command 'persp-prev))
  (define-key perspective-map (kbd "o") 'persp-open)

  (define-key perspective-map (kbd "C-o") 'persp-open)

  ;; (define-key perspective-map (kbd "o") (make-repeatable-command'persp-next))
  ;; (define-key perspective-map (kbd "O") (make-repeatable-command'persp-prev))

  ;; (define-key perspective-map (kbd "ㅐ") (make-repeatable-command'persp-next))
  ;; (define-key perspective-map (kbd "ㅒ") (make-repeatable-command'persp-prev))

  ;; (global-set-key (kbd "C-c 2") 'persp-open)

  ;; (global-set-key (kbd "C-c o") (make-repeatable-command'persp-next))
  ;; (global-set-key (kbd "C-c O") (make-repeatable-command'persp-prev))

  ;; (global-set-key (kbd "C-c ㅐ") (make-repeatable-command'persp-next))
  ;; (global-set-key (kbd "C-c ㅒ") (make-repeatable-command'persp-prev))
  )


;;;; [default Key bindings]
;;;; s -- persp-switch: Query a perspective to switch or create
;;;; k -- persp-remove-buffer: Query a buffer to remove from current perspective
;;;; c -- persp-kill : Query a perspective to kill
;;;; r -- persp-rename: Rename current perspective
;;;; a -- persp-add-buffer: Query an open buffer to add to current perspective
;;;; A -- persp-set-buffer: Add buffer to current perspective and remove it from all others
;;;; i -- persp-import: Import a given perspective from another frame.
;;;; n, <right> -- persp-next : Switch to next perspective
;;;; p, <left> -- persp-prev: Switch to previous perspective
