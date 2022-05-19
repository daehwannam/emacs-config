
;; (if (or (string-equal machine-domain "engels")
;; 	(string-equal machine-domain "vbox"))
;;     (progn
;;       (global-set-key (kbd "C-x g") 'magit-status)
;;       (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)))

(when (package-installed-p 'magit)  ; (member 'magit installable-domain-specific-packages)
  (progn
    (global-set-key (kbd "C-x g") 'magit-status)
    (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)))


;; Hide visibility
;; 
;; https://irreal.org/blog/?p=8877#:~:text=The%20magit%2Dsection%2Dinitial%2D,shown%20on%20the%20status%20page.
;; 
(setq  magit-section-initial-visibility-alist
       '((stashes . hide) (unpushed . hide)))
;; (untracked . hide)
