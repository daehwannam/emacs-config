
;; http://batsov.com/projectile/

(when (fboundp 'projectile-global-mode)
  (projectile-global-mode)

  (when (and (require 'persp-projectile nil t) (fboundp 'persp-mode))
    "do something here and replace this text"
    (define-key projectile-mode-map (kbd "C-c p ;") 'projectile-persp-switch-project)
    ))
