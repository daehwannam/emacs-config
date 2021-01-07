
(when (fboundp 'ryo-modal-mode)
  ;; https://github.com/Kungsgeten/ryo-modal

  (comment
   (define-globalized-minor-mode ryo-modal-global-mode
     ryo-modal-mode
     (lambda () (ryo-modal-mode 1))))

  (setcar (cdr (assq 'ryo-modal-mode minor-mode-alist)) "↑↑↑")

  ;; (global-set-key (kbd "M-'") 'ryo-modal-global-mode)
  (global-set-key (kbd "M-'") 'ryo-modal-mode)

  (ryo-modal-keys
   ("," ryo-modal-repeat))

  (ryo-modal-keys  ; https://github.com/mrkkrp/modalka#example-of-use
   ("W" "M-w")
   ("Y" "M-y")
   ("a" "C-a")
   ("b" "C-b")
   ("e" "C-e")
   ("f" "C-f")
   ("g" "C-g")
   ("n" "C-n")
   ("p" "C-p")
   ("w" "C-w")
   ("y" "C-y")
   ("SPC" "C-SPC"))

  (ryo-modal-keys
   ("s" swiper))
  )
