
(comment
 ;; Set up package.el to work with MELPA
 (require 'package)
 (add-to-list 'package-archives
              '("melpa" . "https://melpa.org/packages/"))
 (package-initialize)
 (package-refresh-contents))


(progn
  (dhnam/install-package-unless-installed 'evil-collection)

  (setq evil-want-minibuffer t)

  (progn
    ;; Evil collection
    ;;
    ;; https://github.com/emacs-evil/evil-collection

    (progn
      ;; evil-want-keybinding should be disabled before loading `evil-collection'
      (setq evil-want-keybinding nil))

    (require 'evil-collection)

    (setq evil-collection-calendar-want-org-bindings t)
    (setq evil-collection-outline-bind-tab-p t)
    (setq evil-collection-setup-minibuffer t)

    ;; Register the bindings, either all at once with
    (evil-collection-init))

  (progn
    (dhnam/install-package-unless-installed 'evil-terminal-cursor-changer)

    (unless (display-graphic-p)
      ;; https://github.com/7696122/evil-terminal-cursor-changer
      (require 'evil-terminal-cursor-changer)
      (evil-terminal-cursor-changer-activate) ; or (etcc-on)
      )

    (progn
      (setq evil-motion-state-cursor 'box)  ; █
      (setq evil-visual-state-cursor 'box)  ; █
      (setq evil-normal-state-cursor 'box)  ; █
      (setq evil-insert-state-cursor 'bar)  ; ⎸
      (setq evil-emacs-state-cursor  'hbar) ; _)
      )))

(comment
  ;; TODO
  ;; https://github.com/cofi/evil-leader
  ;;
  ;; <space> as a leader key
  )

(progn
  ;; Download Evil
  (dhnam/install-package-unless-installed 'evil)

  ;; Enable Evil
  (progn
    (comment
      ;; https://stackoverflow.com/a/18851955
      (setq evil-want-C-u-scroll t))
    (setq evil-move-beyond-eol t)
    (progn
      ;; https://stackoverflow.com/a/23715631
      (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
      (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
      (define-key evil-insert-state-map (kbd "C-u")
	    (lambda ()
	      (interactive)
	      (evil-delete (point-at-bol) (point))))))
  (require 'evil)

  (comment
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
