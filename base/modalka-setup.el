
(dhnam/install-package-unless-installed 'modalka)

(use-existing-pkg modalka
  :init
  (progn
    (assert (fboundp 'modalka-mode))
    ;; https://github.com/mrkkrp/modalka

    (defmacro modalka-define-kbd-for-function (key-string function)
      `(define-key modalka-mode-map (kbd ,key-string) ,function))

    (defmacro modalka-define-key-chord (key-string function)
      `(key-chord-define modalka-mode-map key-string ,function))

    (progn
      (defvar modalka-mode-configured t)
      (modalka-mode t)
      ;; (modalka-mode nil)
      (setcar (cdr (assq 'modalka-mode minor-mode-alist)) "↑↑↑"))

    (defun modalka-mode-on ()
      (interactive)
      (if modalka-global-mode
	  (message "Already enabled")
	(modalka-global-mode t)))

    (defun modalka-mode-off ()
      (interactive)
      (if (not modalka-global-mode)
	  (message "Already disabled")
	(modalka-global-mode)))

    (progn
      (comment
       ;; original binding of "M-'" was 'abbrev-prefix-mark
       (global-set-key (kbd "M-'") 'modalka-mode-on)
       (modalka-define-kbd-for-function "'" 'modalka-global-mode)
       (comment (define-key modalka-mode-map (kbd "i") 'modalka-global-mode)))
      (progn
	(global-set-key (kbd "M-<SPC>") 'modalka-mode-on)
	(key-chord-define-global "gl" 'goto-line)
	(progn
	  ;; keyboard-quit
	  ;; https://emacs.stackexchange.com/a/5972
	  (global-set-key (kbd "M-g") (kbd "C-g")))
	(define-key modalka-mode-map (kbd "i") 'modalka-global-mode)
	(global-set-key (kbd "M-'") 'just-one-space)))

    
    (progn
      (defun update-modalka-line-format ()
	(progn
	  ;; (force-mode-line-update)
	  (setq mode-line-format
		'("%e"
		  mode-line-front-space
		  mode-line-mule-info
		  mode-line-client
		  mode-line-modified
		  mode-line-remote
		  mode-line-frame-identification
		  mode-line-buffer-identification
		  "   "
		  mode-line-position
		  ("" (:eval (if (bound-and-true-p modalka-mode) "<M>" "<E>")) "  ")
		  (vc-mode
		   vc-mode)
		  mode-line-modes
		  mode-line-misc-info
		  mode-line-end-spaces)))
	(comment
	 (setq mode-line-format
	       (dhnam/get-list-inserted-after
		mode-line-format
		(cl-position 'mode-line-position mode-line-format)
		'("" (:eval (if (bound-and-true-p modalka-mode) "<M>" "<E>")) "  ")))))

      (defun modalka-mode-color-indicator ()
	(cond (modalka-mode
	       (progn
		 (set-face-background 'mode-line "blue4")
		 (set-face-foreground 'mode-line "gray")
		 (set-face-background 'mode-line-inactive "gray45")
		 (set-face-foreground 'mode-line-inactive "blue")))
	      (t
	       (progn
		 (set-face-background 'mode-line-inactive "gray30")
		 (set-face-foreground 'mode-line-inactive "gray80")
		 (set-face-background 'mode-line "gray75")
		 (set-face-foreground 'mode-line "black")))))

      (defun modalka-mode-indicator ()
	(update-modalka-line-format)
	(modalka-mode-color-indicator))

      (add-hook 'modalka-mode-hook 'modalka-mode-indicator))

    ;; binding examples
    ;; https://github.com/mrkkrp/dot-emacs/blob/master/mk/mk-packages.el
    ;; https://github.com/mrkkrp/dot-emacs/blob/master/mk/mk-global.el
    ;; https://github.com/mrkkrp/dot-emacs/tree/master/mk

    (progn
      ;; bindings
      (progn
	(modalka-define-kbd-for-function "a" 'move-beginning-of-line)
	(modalka-define-kbd-for-function "b" 'backward-char)
	(modalka-define-kbd-for-function "e" 'move-end-of-line)
	(modalka-define-kbd-for-function "f" 'forward-char)
	(modalka-define-kbd-for-function "n" 'next-line)
	(modalka-define-kbd-for-function "p" 'previous-line)
	(modalka-define-kbd "w" "C-w")
	(modalka-define-kbd "y" "C-y")
	(modalka-define-kbd "k" "C-k")
	(modalka-define-kbd "d" "C-d")
	(modalka-define-kbd "s" "C-s")
	(modalka-define-kbd "r" "C-r")
	(comment (modalka-define-kbd-for-function "u" 'universal-argument))
	(modalka-define-kbd "l" "C-l")
	(modalka-define-kbd "j" "C-j")
	(modalka-define-kbd "SPC" "C-SPC")
	(modalka-define-kbd "/" "C-/")
	(modalka-define-kbd-for-function "." 'repeat))

      (comment
       (modalka-define-kbd "a" "C-a")
       (modalka-define-kbd-for-function "b" 'backward-char)
       ;; (modalka-define-kbd "b" "C-b")
       (modalka-define-kbd "e" "C-e")
       (modalka-define-kbd "f" "C-f")
       (comment (modalka-define-kbd "g" "C-g"))
       (modalka-define-kbd "n" "C-n")
       (modalka-define-kbd "p" "C-p")
       (modalka-define-kbd "w" "C-w")
       (modalka-define-kbd "y" "C-y")
       (modalka-define-kbd "k" "C-k")
       (modalka-define-kbd "d" "C-d")
       (modalka-define-kbd "s" "C-s")
       (modalka-define-kbd "r" "C-r")
       (modalka-define-kbd "u" "C-u")
       (modalka-define-kbd "l" "C-l")
       (modalka-define-kbd "j" "C-j")
       (modalka-define-kbd "SPC" "C-SPC")
       (modalka-define-kbd "/" "C-/"))

      (comment
       (modalka-define-kbd "W" "M-w")
       (modalka-define-kbd "Y" "M-y"))

      (comment
       ;; (modalka-define-kbd "l" "C-l")
       ;; (modalka-define-kbd "L" "M-L")
       ;; (define-key modalka-mode-map (kbd "L") 'dhnam/reverse-recenter-top-bottom)
       (modalka-define-kbd-for-function "L" 'dhnam/reverse-recenter-top-bottom))
      
      (progn
	;; (modalka-define-kbd "x s" "C-x s")
	(modalka-define-kbd "v" "C-v")
	(comment (modalka-define-kbd "V" "M-v")))

      (progn
	(modalka-define-kbd "K" "C-x k")
	(modalka-define-kbd "F" "C-x C-f")
	(modalka-define-kbd "S" "C-x C-s")
	(modalka-define-kbd "B" "C-x b"))

      (comment
	(modalka-define-kbd "+" "C-x +")
	(modalka-define-kbd "^" "C-x ^")
	(modalka-define-kbd "%" "C-x %")
	(modalka-define-kbd "}" "C-x }")
	(modalka-define-kbd "{" "C-x {"))

      (progn
	(modalka-define-kbd "E" "C-x C-e")
	(comment (modalka-define-kbd "x e" "C-x C-e"))
	(comment (modalka-define-kbd "x s" "C-x C-s"))
	(modalka-define-kbd "x x" "C-x C-x")
	(modalka-define-kbd "x c" "C-x C-c")

	(modalka-define-kbd "c e" "C-c C-e")
	(modalka-define-kbd "c r" "C-c C-r")
	(comment
	 (modalka-define-kbd "x w" "C-x M-w")))

      (progn
	(modalka-define-kbd "M-B" "C-M-b")
	(modalka-define-kbd "M-F" "C-M-f")
	(modalka-define-kbd "M-N" "C-M-n")
	(modalka-define-kbd "M-P" "C-M-p")
	(modalka-define-kbd "M-|" "C-M-\\"))

      (progn
	(comment
	 (modalka-define-kbd "x o" "C-x o")
	 (modalka-define-kbd "x 0" "C-x 0")
	 (modalka-define-kbd "x 1" "C-x 1")
	 (modalka-define-kbd "x 2" "C-x 2")
	 (modalka-define-kbd "x 3" "C-x 3"))

	(progn
	  (modalka-define-kbd "o" "C-x o")
	  (modalka-define-kbd "O" "C-x O")
	  (modalka-define-kbd "0" "C-x 0")
	  (modalka-define-kbd "1" "C-x 1")
	  (modalka-define-kbd "2" "C-x 2")
	  (modalka-define-kbd "3" "C-x 3"))

	(progn
	  (modalka-define-kbd "c o" "C-c o")
	  (modalka-define-kbd "c O" "C-c O")
	  (modalka-define-kbd "c 0" "C-c 0")
	  (modalka-define-kbd "c 1" "C-c 1")
	  (modalka-define-kbd "c 2" "C-c 2")

	  (modalka-define-kbd "z o" "C-z o")
	  (modalka-define-kbd "z O" "C-z O")
	  (modalka-define-kbd "z 0" "C-z 0")
	  (modalka-define-kbd "z 1" "C-z 1")
	  (modalka-define-kbd "z 2" "C-z 2")))

      (progn
	(comment
	 (modalka-define-kbd "x o" "c-x o")
	 (modalka-define-kbd "x 0" "c-x 0")
	 (modalka-define-kbd "x 9" "c-x 9")
	 (modalka-define-kbd "x 8" "c-x 8")
	 (modalka-define-kbd "x 7" "c-x 7"))

	(progn
	  (modalka-define-kbd "1" "C-x 9")
	  (modalka-define-kbd "2" "C-x 8")
	  (modalka-define-kbd "3" "C-x 7")

	  (modalka-define-kbd "o" "C-x o")
	  (modalka-define-kbd "0" "C-x 0")
	  (modalka-define-kbd "9" "C-x 9")
	  (modalka-define-kbd "8" "C-x 8")
	  (modalka-define-kbd "7" "C-x 7"))

	(progn
	  (modalka-define-kbd "c o" "C-c o")
	  (modalka-define-kbd "c 0" "C-c 0")
	  (modalka-define-kbd "c 9" "C-c 9")
	  (modalka-define-kbd "c 8" "C-c 8"))

	(progn
	  (modalka-define-kbd "z o" "C-z o")
	  (modalka-define-kbd "z 0" "C-z 0")
	  (modalka-define-kbd "z 9" "C-z 9")
	  (modalka-define-kbd "z 8" "C-z 8")))

      (progn
	(modalka-define-kbd "x +" "C-x +"))

      (comment
       (modalka-define-kbd "c s" "C-c s")
       (modalka-define-kbd "c x p" "C-c C-x C-p")
       (modalka-define-kbd "c x f" "C-c C-x C-f"))

      (progn
	(modalka-define-kbd "<left>" "C-x <left>")
	(modalka-define-kbd "<right>" "C-x <right>"))

      (progn
	(modalka-define-kbd "c <left>" "C-c <left>")
	(modalka-define-kbd "c <right>" "C-c <right>"))

      (progn
	(modalka-define-kbd "c R" "C-c M-r"))

      ;; turn on modalka-mode
      ;; (require 'evil)
      (modalka-global-mode t))

    (cl-position 'mode-line-position mode-line-format))

  :after (key-chord))
