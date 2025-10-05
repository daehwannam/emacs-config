
(require 'dhnam-buffer)

(progn
  (comment
    ;; buffer-menu
    ;; http://stackoverflow.com/questions/1231188/emacs-list-buffers-behavior
    (global-set-key "\C-x\C-b" 'buffer-menu))
  (progn
    ;; ibuffer
    ;; http://emacs.stackexchange.com/questions/202/close-all-dired-buffers
    (global-set-key "\C-x\C-b" 'ibuffer)
    (with-eval-after-load 'ibuffer
      (define-key ibuffer-mode-map (kbd "M-o") nil))))

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

(global-set-key (kbd "C-x H") 'dhnam/copy-whole-buffer)


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
  (require 'buffer-move nil t)
  (comment
    (defhydra hydra-buffer-move ()
      "buffer-move"
      ("h" buf-move-left)
      ("l" buf-move-right)
      ("k" buf-move-up)
      ("j" buf-move-down)
      ("q" nil "quit"))
    )

  (progn
    (comment
      (defhydra hydra-buffer-move (global-map "C-c m")
        "buffer-move"
        ("q" nil "quit")
        ("j" buf-move-left)
        ("l" buf-move-right)
        ("i" buf-move-up)
        ("k" buf-move-down)))

    (defhydra hydra-buffer-move (global-map "C-c m")
      "buffer-move"
      ("q" nil "quit")
      ("k" buf-move-left)
      ("l" buf-move-right)
      ("i" buf-move-up)
      ("o" buf-move-down))

    (progn
      ;; Disable any hint message
      (hydra-set-property 'hydra-buffer-move :verbosity 0))))

(comment
  (key-chord-define-global "kk" 'kill-buffer)
  (key-chord-define-global "ff" 'find-file))

(key-chord-define-global "s;" 'dhnam/switch-to-org-scratch-buffer)
(key-chord-define-global "e;" 'dhnam/switch-to-scratch-buffer)

(provide 'init-buffer)
