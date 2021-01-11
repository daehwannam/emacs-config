
;; http://batsov.com/projectile/

(use-existing-pkg projectile
  :init
  (progn
    (progn
      (projectile-global-mode +1)

      ;; (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
      (comment (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))
      (progn
	;; keymap with key-chord
	;; https://stackoverflow.com/a/25475574
	(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
	(key-chord-define-global "cp" projectile-command-map))

      (setq projectile-completion-system 'ivy))

    (when (and (require 'persp-projectile nil t) (fboundp 'persp-mode))
      "do something here and replace this text"
      (comment (define-key projectile-mode-map (kbd "C-c p ;") 'projectile-persp-switch-project))))

  :after (key-chord))

(use-existing-pkg counsel-projectile
  :init
  (counsel-projectile-mode +1)
  :after (projectile))

(comment
 ;; project list is saved into "~/.emacs.d/projectile-bookmarks.eld"
 )
