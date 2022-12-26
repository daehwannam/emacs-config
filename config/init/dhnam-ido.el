
(comment
 (when (package-installed-p 'ido)
   (require 'ido)
   (comment                       ; not working
    ;; disable ido for specific commands
    ;; https://www.emacswiki.org/emacs/InteractivelyDoThings#h5o-34
    (ido-mode)

    (define-key (cdr ido-minor-mode-map-entry) [remap write-file] nil)
    (define-key (cdr ido-minor-mode-map-entry) [remap find-file] nil))

   (progn
     ;; https://www.masteringemacs.org/article/introduction-to-ido-mode
     (setq ido-enable-flex-matching t)
     (setq ido-everywhere t)
     (setq ido-use-filename-at-point 'guess))


   (ido-mode t)                         ;  (comment (ido-mode nil))
   ))
