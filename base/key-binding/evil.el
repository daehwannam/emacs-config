
(comment
 ;; Set up package.el to work with MELPA
 (require 'package)
 (add-to-list 'package-archives
              '("melpa" . "https://melpa.org/packages/"))
 (package-initialize)
 (package-refresh-contents))


(progn
  (dhnam/install-package-unless-installed 'evil-collection)

  (setq evil-want-keybinding nil)

  ;; Register the bindings, either all at once with
  (evil-collection-init))

(progn
  ;; Download Evil
  (dhnam/install-package-unless-installed 'evil)

  ;; Enable Evil
  (progn
    (comment
     ;; https://stackoverflow.com/a/18851955
     (setq evil-want-C-u-scroll t))
    (progn
      ;; https://stackoverflow.com/a/23715631
      (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
      (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
      (define-key evil-insert-state-map (kbd "C-u")
	(lambda ()
	  (interactive)
	  (evil-delete (point-at-bol) (point))))))
  (require 'evil)

  (progn
    (define-key evil-normal-state-map   (kbd "M-'") #'dhnam/just-one-space-conditionally)
    (define-key evil-motion-state-map   (kbd "M-'") #'dhnam/just-one-space-conditionally)
    (define-key evil-insert-state-map   (kbd "M-'") #'dhnam/just-one-space-conditionally)
    (define-key evil-window-map         (kbd "M-'") #'dhnam/just-one-space-conditionally)
    (define-key evil-operator-state-map (kbd "M-'") #'dhnam/just-one-space-conditionally)

    (define-key evil-normal-state-map   (kbd "M-<SPC>") #'evil-force-normal-state)
    (define-key evil-motion-state-map   (kbd "M-<SPC>") #'evil-force-normal-state)
    (define-key evil-insert-state-map   (kbd "M-<SPC>") #'evil-force-normal-state)
    (define-key evil-window-map         (kbd "M-<SPC>") #'evil-force-normal-state)
    (define-key evil-operator-state-map (kbd "M-<SPC>") #'evil-force-normal-state))

  (evil-mode 1))
