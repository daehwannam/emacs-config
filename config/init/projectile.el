
;; http://batsov.com/projectile/

(use-existing-pkg projectile
  :init
  (projectile-global-mode +1)

  ;; (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

  (when (and (require 'persp-projectile nil t) (fboundp 'persp-mode))
    "do something here and replace this text"
    (define-key projectile-mode-map (kbd "C-c p ;") 'projectile-persp-switch-project)
    ))
