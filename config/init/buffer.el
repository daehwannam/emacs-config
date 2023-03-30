
(progn
  (comment
   ;; buffer-menu
   ;; http://stackoverflow.com/questions/1231188/emacs-list-buffers-behavior
   (global-set-key "\C-x\C-b" 'buffer-menu))
  (progn
    ;; ibuffer
    ;; http://emacs.stackexchange.com/questions/202/close-all-dired-buffers
    (global-set-key "\C-x\C-b" 'ibuffer)))

(require 'dhnam-make-repeatable-command)

(comment
 ;; other window with repeatition
 ;; http://stackoverflow.com/questions/91071/emacs-switch-to-previous-window
 ;;(load "make-repeatable-command") ;(load "~/.emacs.d/package/make-repeatable-command.el")
  
 ;; switch buffers
 ;; http://emacs.stackexchange.com/questions/728/how-do-i-switch-buffers-quickly
 ;;(global-set-key (kbd "C-c p") 'previous-buffer)
 ;;(global-set-key (kbd "C-c n") 'next-buffer)

 ;;(global-set-key (kbd "C-c C-p") (make-repeatable-command 'previous-buffer))
 ;;(global-set-key (kbd "C-c C-n") (make-repeatable-command 'next-buffer))

 ;;(global-set-key (kbd "C-M-p") (make-repeatable-command 'previous-buffer))
 ;;(global-set-key (kbd "C-M-n") (make-repeatable-command 'next-buffer))

 ;;(global-set-key (kbd "C-c C-b") (make-repeatable-command 'previous-buffer))
 ;;(global-set-key (kbd "C-c C-f") (make-repeatable-command 'next-buffer))

 ;; (global-set-key (kbd "ESC <f7>") (make-repeatable-command 'previous-buffer)) ; M-<f7>
 ;; (global-set-key (kbd "ESC <f8>") (make-repeatable-command 'next-buffer)) ; M-<f8>

 ;; (global-set-key (kbd "<f7>") (make-repeatable-command 'previous-buffer)) ; M-<f7>
 ;; (global-set-key (kbd "<f8>") (make-repeatable-command 'next-buffer)) ; M-<f8>

 ;; (global-set-key (kbd "C-x <left>") (make-repeatable-command 'previous-buffer)) ; M-<f7>
 ;; (global-set-key (kbd "C-x <right>") (make-repeatable-command 'next-buffer)) ; M-<f8>
 )

(progn
  (defun dhnam/copy-whole-buffer ()
    (interactive)
    (kill-ring-save (point-min) (point-max)))

  (global-set-key (kbd "C-x H") 'dhnam/copy-whole-buffer))


(if (and nil (package-installed-p 'hydra))
    (progn
      (defhydra hydra-prev-next-buffer ()
	    "prev-next-buffer"
	    ("<left>" previous-buffer)
	    ("<right>" next-buffer))
      (global-set-key (kbd "C-x <left>") #'hydra-prev-next-buffer/previous-buffer)
      (global-set-key (kbd "C-x <right>") #'hydra-prev-next-buffer/next-buffer))
  (progn
    (global-set-key (kbd "C-x <left>") (make-repeatable-command 'previous-buffer))
    (global-set-key (kbd "C-x <right>") (make-repeatable-command 'next-buffer))

    (progn
      (define-key (current-global-map) (kbd "C-x M-p") (make-repeatable-command 'previous-buffer))
      (define-key (current-global-map) (kbd "C-x M-n") (make-repeatable-command 'next-buffer)))

    (progn
      (define-key (current-global-map) (kbd "M-9") 'previous-buffer)
      (define-key (current-global-map) (kbd "M-0") 'next-buffer))

    (comment
      (key-chord-define (current-global-map) "pp" (make-repeatable-command 'previous-buffer))
      (key-chord-define (current-global-map) "nn" (make-repeatable-command 'next-buffer)))

    (comment
      (defhydra hydra-prev-next-buffer ()
	    "prev-next-buffer"
	    ("M-p" previous-buffer)
	    ("M-n" next-buffer))
      (global-set-key (kbd "C-x M-p") #'hydra-prev-next-buffer/previous-buffer)
      (global-set-key (kbd "C-x M-n") #'hydra-prev-next-buffer/next-buffer))))

(progn
  (require 'buffer-move)
  (comment
   (defhydra hydra-buffer-move ()
     "buffer-move"
     ("h" buf-move-left)
     ("l" buf-move-right)
     ("k" buf-move-up)
     ("j" buf-move-down)
     ("q" nil "quit"))
   )

  (defhydra hydra-buffer-move (global-map "C-c m")
    "buffer-move"
    ("q" nil "quit")
    ("j" buf-move-left)
    ("l" buf-move-right)
    ("i" buf-move-up)
    ("k" buf-move-down)))

(progn
  ;; dhnam/buf-shift-* functions are defined for application buffers of EXWM

  (comment (require 'buffer-move))

  (defun dhnam/buf-shift-up ()
    "It's modified from `buf-move-up'"
    (interactive)
    (let* ((other-win (windmove-find-other-window 'up))
	       (buf-this-buf (window-buffer (selected-window))))
      (if (null other-win)
          (error "No window above this one")
        ;; swap top with this one
        (comment (set-window-buffer (selected-window) (window-buffer other-win)))
        ;; move this one to top
        (set-window-buffer other-win buf-this-buf)
        (select-window other-win))))

  (defun dhnam/buf-shift-down ()
    "It's modified from `buf-move-down'"
    (interactive)
    (let* ((other-win (windmove-find-other-window 'down))
	       (buf-this-buf (window-buffer (selected-window))))
      (if (or (null other-win) 
              (string-match "^ \\*Minibuf" (buffer-name (window-buffer other-win))))
          (error "No window under this one")
        ;; swap top with this one
        (comment (set-window-buffer (selected-window) (window-buffer other-win)))
        ;; move this one to top
        (set-window-buffer other-win buf-this-buf)
        (select-window other-win))))

  (defun dhnam/buf-shift-left ()
    "It's modified from `buf-move-left'"
    (interactive)
    (let* ((other-win (windmove-find-other-window 'left))
	       (buf-this-buf (window-buffer (selected-window))))
      (if (null other-win)
          (error "No left split")
        ;; swap top with this one
        (comment (set-window-buffer (selected-window) (window-buffer other-win)))
        ;; move this one to top
        (set-window-buffer other-win buf-this-buf)
        (select-window other-win))))

  (defun dhnam/buf-shift-right ()
    "It's modified from `buf-move-right'"
    (interactive)
    (let* ((other-win (windmove-find-other-window 'right))
	       (buf-this-buf (window-buffer (selected-window))))
      (if (null other-win)
          (error "No right split")
        ;; swap top with this one
        (comment (set-window-buffer (selected-window) (window-buffer other-win)))
        ;; move this one to top
        (set-window-buffer other-win buf-this-buf)
        (select-window other-win)))))

(progn
  ;; https://stackoverflow.com/a/384346

  (defun dhnam/rename-file-and-buffer (new-name)
    "Renames both current buffer and file it's visiting to NEW-NAME."
    (interactive "sNew name: ")
    (let ((name (buffer-name))
          (filename (buffer-file-name)))
      (if (not filename)
          (message "Buffer '%s' is not visiting a file!" name)
	(if (get-buffer new-name)
            (message "A buffer named '%s' already exists!" new-name)
          (progn
            (rename-file filename new-name 1)
            (rename-buffer new-name)
            (set-visited-file-name new-name)
            (set-buffer-modified-p nil)))))))

(comment
  (key-chord-define-global "kk" 'kill-buffer)
  (key-chord-define-global "ff" 'find-file)
  )

(defun dhnam/find-actual-file (filename &optional wildcards)
  "Open an actual file indicated by a symlink"
  ;; https://emacs.stackexchange.com/a/41292

  (interactive
   (find-file-read-args "Find an actual file: "
                        (confirm-nonexistent-file-or-buffer)))
  (let ((find-file-visit-truename t))
    (find-file filename wildcards)))

(progn
  (defun dhnam/switch-to-scratch-buffer ()
    (interactive)
    (switch-to-buffer "*scratch*"))

  (key-chord-define-global "s;" 'dhnam/switch-to-scratch-buffer))

(provide 'init-buffer)
