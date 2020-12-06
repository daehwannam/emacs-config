(when (and (fboundp 'ivy-mode) (fboundp 'counsel-mode))
  ;; http://oremacs.com/swiper/

  ;; Initialization
  ;; (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")

  ;; Ivy-based interface to standard commands
  (global-set-key (kbd "C-c s") 'swiper)
  ;; (global-set-key (kbd "C-c C-s") 'swiper) ; conflict with 'elpy-rgrep-symbol
  ;; (global-set-key (kbd "C-r") 'swiper)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  ;; (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)

  ;; Ivy-based interface to shell and system tools
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)

  ;; Ivy-resume and other commands
  ;; ivy-resume resumes the last Ivy-based completion.
  (global-set-key (kbd "C-c C-r") 'ivy-resume)

  (defun swiper-with-text-in-region (start end)
    (interactive "r")
    (deactivate-mark)
    (swiper (buffer-substring start end)))

  (global-set-key (kbd "C-c C-s") 'swiper-with-text-in-region)
  )

(when (fboundp 'counsel-projectile-mode)
  (counsel-projectile-mode))
