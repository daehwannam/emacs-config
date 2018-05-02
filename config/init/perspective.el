
(require 'make-repeatable-command)

(persp-mode)

(defvar persp-count 0
  "Counter for the created perspectives")

(defvar default-persp-name "p")

(defun next-persp-name ()
  (concat default-persp-name (number-to-string persp-count)))

;;; rename the first perspective
(persp-rename (next-persp-name))
(setq persp-count (1+ persp-count))

;;; open new perspective (modified version of persp-switch
(defun persp-open (name)
  "Create a new perspective"
  (interactive (list
                (read-string (format "New perspective name (%s): " (next-persp-name))
                             nil nil (next-persp-name))))
  (persp-switch name)
  (setq persp-count (1+ persp-count)))

;;; customized keys
(define-key perspective-map (kbd "2") 'persp-open)

(define-key perspective-map (kbd "n") (make-repeatable-command'persp-next))
(define-key perspective-map (kbd "p") (make-repeatable-command'persp-prev))

(define-key perspective-map (kbd "o") (make-repeatable-command'persp-next))
(define-key perspective-map (kbd "O") (make-repeatable-command'persp-prev))

(define-key perspective-map (kbd "ㅐ") (make-repeatable-command'persp-next))
(define-key perspective-map (kbd "ㅒ") (make-repeatable-command'persp-prev))

;; (global-set-key (kbd "C-c 2") 'persp-open)

;; (global-set-key (kbd "C-c o") (make-repeatable-command'persp-next))
;; (global-set-key (kbd "C-c O") (make-repeatable-command'persp-prev))

;; (global-set-key (kbd "C-c ㅐ") (make-repeatable-command'persp-next))
;; (global-set-key (kbd "C-c ㅒ") (make-repeatable-command'persp-prev))

