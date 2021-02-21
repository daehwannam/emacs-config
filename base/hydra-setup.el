
(progn
  ;; https://github.com/abo-abo/hydra/wiki/Emacs

  (defmacro keycall (key-string &rest args)
    `(funcall (key-binding (kbd ,key-string)) ,@args))

  (progn
    (defhydra hydra-yank-pop ()
      "yank"
      ("C-y" yank nil)
      ("M-y" yank-pop nil)
      ("M-Y" (yank-pop 1) nil)

      ("y" (yank-pop 1) "next")
      ("Y" (yank-pop -1) "prev")
      ("l" counsel-yank-pop "list" :color blue)) ; or browse-kill-ring
    (global-set-key (kbd "C-y") #'hydra-yank-pop/yank))

  (comment
   (defhydra hydra-move ()
     ;; this could cause segmentation fault error
     "move"
     ("i" nil)  ; stop hydra
     ("g" (keycall "C-g"))
     ("n" (keycall "C-n"))
     ("p" (keycall "C-p"))
     ;; ("f" (keycall "C-f"))
     ;; ("b" (keycall "C-b"))
     ("f" forward-char)
     ("b" backward-char)
     ("M-f" forward-word)
     ("M-b" backward-word)
     ("M-N" (keycall "C-M-N"))
     ("M-P" (keycall "C-M-P"))
     ("M-F" (keycall "C-M-f"))
     ("M-B" (keycall "C-M-b"))
     ("C-v" (keycall "C-v"))
     ("M-v" (keycall "M-v"))
     ("C-w" (keycall "C-w"))
     ("M-w" (keycall "M-w"))
     ("a" (keycall "C-a" 1))
     ("e" (keycall "C-e" 1))
     ("l" (keycall "C-l"))
     ("L" (keycall "M-L"))
     ("M-r" (keycall "M-r"))
     ("M-R" (keycall "M-R"))
     ("s" (keycall "C-s"))
     ("r" (keycall "C-r"))
     ("M->" (keycall "M->"))
     ("M-<" (keycall "M-<"))
     ("<SPC>" set-mark-command)
     ("x x" exchange-point-and-mark)
     ("j" swiper)))

  (comment
    (let ((hydra-def (defhydra hydra-move ()
		       ;; this could be safe from segmentation fault error
		       "move"
		       ("i" nil)  ; stop hydra
		       ("g" keyboard-quit)
		       ("n" next-line)
		       ("p" previous-line)
		       ("f" forward-char)
		       ("b" backward-char)
		       ("M-f" forward-word)
		       ("M-b" backward-word)
		       ("M-N" forward-list)
		       ("M-P" backward-list)
		       ("M-F" forward-sexp)
		       ("M-B" backward-sexp)
		       ("C-v" scroll-up-small)
		       ("M-v" scroll-down-small)
		       ("a" move-beginning-of-line)
		       ;; remove keycall
		       ("e" move-end-of-line)
		       ("l" recenter-top-bottom)
		       ("L" reverse-recenter-top-bottom)
		       ("M-r" move-to-window-line-top-bottom)
		       ("M-R" reverse-move-to-window-line-top-bottom)
		       ("s" isearch-forward)
		       ("r" isearch-backward)
		       ("M->" end-of-buffer)
		       ("M-<" beginning-of-buffer)
		       ("<SPC>" set-mark-command)
		       ("x x" exchange-point-and-mark)
		       ("j" swiper)
		       )))
      ;; (global-set-key (kbd "C-f") #'hydra-move/forward-char)
      ;; (global-set-key (kbd "C-b") #'hydra-move/backward-char)
      (comment (global-set-key (kbd "M-f") #'hydra-move/forward-word))
      (comment (global-set-key (kbd "M-b") #'hydra-move/backward-word))
      (global-set-key (kbd "M-'") hydra-def)))

  (progn
    (defun other-window-backwards () (interactive) (other-window -1))
    (defhydra hydra-window ()
      "window"
      ("o" other-window)
      ("O" other-window-backwards)

      ("0" delete-window)

      ("1" delete-other-windows)
      ("2" split-window-below)
      ("3" split-window-right)

      ("9" delete-other-windows)
      ("8" split-window-below)
      ("7" split-window-right)

      ("^" enlarge-window)
      ("%" shrink-window)
      ("}" enlarge-window-horizontally)
      ("{" shrink-window-horizontally)
      ("+" balance-windows))

    (global-set-key (kbd "C-x o") #'hydra-window/other-window)
    (global-set-key (kbd "C-x O") #'hydra-window/other-window-backwards)
    (global-set-key (kbd "C-x 0") #'hydra-window/delete-window)

    (global-set-key (kbd "C-x 1") #'hydra-window/delete-other-windows)
    (global-set-key (kbd "C-x 2") #'hydra-window/split-window-below)
    (global-set-key (kbd "C-x 3") #'hydra-window/split-window-right)

    (global-set-key (kbd "C-x 9") #'hydra-window/delete-other-windows)
    (global-set-key (kbd "C-x 8") #'hydra-window/split-window-below)
    (global-set-key (kbd "C-x 7") #'hydra-window/split-window-right)

    (global-set-key (kbd "C-x ^") #'hydra-window/enlarge-window)
    (global-set-key (kbd "C-x %") #'hydra-window/shrink-window)
    (global-set-key (kbd "C-x }") #'hydra-window/enlarge-window-horizontally)
    (global-set-key (kbd "C-x {") #'hydra-window/shrink-window-horizontally)
    (global-set-key (kbd "C-x +") #'hydra-window/balance-windows))

  (progn
    (defun other-frame-backwards () (interactive) (other-frame -1))
    (defhydra hydra-frame ()
      "frame"
      ("o" other-frame)
      ("O" other-frame-backwards)

      ("0" delete-frame)

      ("1" delete-other-frames)
      ("2" make-frame-command)

      ("9" delete-other-frames)
      ("8" make-frame-command))

    (global-set-key (kbd "C-c o") #'hydra-frame/other-frame)
    (global-set-key (kbd "C-c O") #'hydra-frame/other-frame-backwards)
    (global-set-key (kbd "C-c 0") #'hydra-frame/delete-frame)

    (global-set-key (kbd "C-c 1") #'hydra-frame/delete-other-frames)
    (global-set-key (kbd "C-c 2") #'hydra-frame/make-frame-command)

    (global-set-key (kbd "C-c 9") #'hydra-frame/delete-other-frames)
    (global-set-key (kbd "C-c 8") #'hydra-frame/make-frame-command)))

