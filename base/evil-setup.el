
(comment
 ;; Set up package.el to work with MELPA
 (require 'package)
 (add-to-list 'package-archives
              '("melpa" . "https://melpa.org/packages/"))
 (package-initialize)
 (package-refresh-contents))


(progn
  (unless (package-installed-p 'evil-collection)
    (package-install 'evil-collection))

  (setq evil-want-keybinding nil)

  ;; Register the bindings, either all at once with
  (evil-collection-init))

(progn
  ;; Download Evil
  (unless (package-installed-p 'evil)
    (package-install 'evil))

  ;; Enable Evil
  (setq evil-want-C-u-scroll t) ; https://stackoverflow.com/a/18851955
  (require 'evil)

  (progn
    (define-key evil-normal-state-map   (kbd "M-'") #'just-one-space-conditionally)
    (define-key evil-motion-state-map   (kbd "M-'") #'just-one-space-conditionally)
    (define-key evil-insert-state-map   (kbd "M-'") #'just-one-space-conditionally)
    (define-key evil-window-map         (kbd "M-'") #'just-one-space-conditionally)
    (define-key evil-operator-state-map (kbd "M-'") #'just-one-space-conditionally)

    (define-key evil-normal-state-map   (kbd "M-<SPC>") #'evil-force-normal-state)
    (define-key evil-motion-state-map   (kbd "M-<SPC>") #'evil-force-normal-state)
    (define-key evil-insert-state-map   (kbd "M-<SPC>") #'evil-force-normal-state)
    (define-key evil-window-map         (kbd "M-<SPC>") #'evil-force-normal-state)
    (define-key evil-operator-state-map (kbd "M-<SPC>") #'evil-force-normal-state))

  (evil-mode 1))