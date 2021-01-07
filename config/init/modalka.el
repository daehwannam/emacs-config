
(comment
 (when (fboundp 'modalka-mode)
   ;; https://github.com/mrkkrp/modalka

   (defmacro modalka-define-kbd-for-function (key-string function)
     `(define-key modalka-mode-map (kbd ,key-string) ,function))

   (global-set-key (kbd "M-'") 'modalka-global-mode) ; original binding was 'abbrev-prefix-mark

   (progn
     (modalka-mode t)
     (modalka-mode nil)
     (setcar (cdr (assq 'modalka-mode minor-mode-alist)) "↑↑↑"))

   (progn
     (defun modalka-mode-indicator ()
       (cond (modalka-mode
	      (progn
		(set-face-background 'mode-line "blue4")
		(set-face-foreground 'mode-line "gray")
		(set-face-background 'mode-line-inactive "gray30")
		(set-face-foreground 'mode-line-inactive "blue")))
	     (t
	      (progn
		(set-face-background 'mode-line-inactive "gray30")
		(set-face-foreground 'mode-line-inactive "gray80")
		(set-face-background 'mode-line "gray75")
		(set-face-foreground 'mode-line "black")))))
     (add-hook 'modalka-mode-hook 'modalka-mode-indicator))

   ;; binding examples
   ;; https://github.com/mrkkrp/dot-emacs/blob/master/mk/mk-packages.el
   ;; https://github.com/mrkkrp/dot-emacs/blob/master/mk/mk-global.el
   ;; https://github.com/mrkkrp/dot-emacs/tree/master/mk

   (progn
     ;; bindings
     (progn
       (modalka-define-kbd-for-function "'" 'modalka-global-mode))

     (progn
       (modalka-define-kbd "a" "C-a")
       (modalka-define-kbd-for-function "b" 'backward-char)
       ;; (modalka-define-kbd "b" "C-b")
       (modalka-define-kbd "e" "C-e")
       (modalka-define-kbd "f" "C-f")
       (modalka-define-kbd "g" "C-g")
       (modalka-define-kbd "n" "C-n")
       (modalka-define-kbd "p" "C-p")
       (modalka-define-kbd "w" "C-w")
       (modalka-define-kbd "y" "C-y")
       (modalka-define-kbd "k" "C-k")
       (modalka-define-kbd "d" "C-d")
       (modalka-define-kbd "s" "C-s")
       (modalka-define-kbd "r" "C-r")
       (modalka-define-kbd "SPC" "C-SPC")
       (modalka-define-kbd "/" "C-/"))

     (progn
       (modalka-define-kbd "W" "M-w")
       (modalka-define-kbd "Y" "M-y"))

     (progn
       (modalka-define-kbd "l" "C-l")
       ;; (modalka-define-kbd "L" "M-L")
       ;; (define-key modalka-mode-map (kbd "L") 'reverse-recenter-top-bottom)
       (modalka-define-kbd-for-function "L" 'reverse-recenter-top-bottom))
     
     (progn
       ;; (modalka-define-kbd "x s" "C-x s")
       (modalka-define-kbd "v" "C-v")
       (modalka-define-kbd "V" "M-v"))

     (progn
       (modalka-define-kbd "K" "C-x k")
       (modalka-define-kbd "F" "C-x C-f")
       (modalka-define-kbd "B" "C-x b"))

     (progn
       (modalka-define-kbd "x e" "C-x C-e")
       (modalka-define-kbd "x s" "C-x C-s")
       (modalka-define-kbd "x w" "C-x M-w"))

     (progn
       (modalka-define-kbd "o" "C-x o")
       (modalka-define-kbd "0" "C-x 0")
       (modalka-define-kbd "1" "C-x 1")
       (modalka-define-kbd "2" "C-x 2")
       (modalka-define-kbd "3" "C-x 3"))

     (progn
       (modalka-define-kbd "c o" "C-c o")
       (modalka-define-kbd "c 0" "C-c 0")
       (modalka-define-kbd "c 1" "C-c 1")
       (modalka-define-kbd "c 2" "C-c 2"))

     (progn
       (modalka-define-kbd "c s" "C-c s")
       (modalka-define-kbd "c x p" "C-c C-x C-p")
       (modalka-define-kbd "c x f" "C-c C-x C-f"))

     (progn
       (modalka-define-kbd "<left>" "C-x <left>")
       (modalka-define-kbd "<right>" "C-x <right>"))

     (modalka-global-mode t))
   ))
