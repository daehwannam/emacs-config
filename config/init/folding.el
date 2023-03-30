;; HideShow mode
(add-hook 'prog-mode-hook #'hs-minor-mode)
(global-set-key (kbd "C-c i h") #'hs-hide-all)
(global-set-key (kbd "C-c i u") #'hs-show-all)
(global-set-key (kbd "C-c i i") #'hs-toggle-hiding)
(global-set-key (kbd "C-c i j") #'hs-hide-level)


;; (hs-minor-mode 1) ;; enable current buffer only...
;; (hs-minor-mode 1) ;; disable the mode

;; vimish-fold
;; (when (fboundp 'vimish-fold-mode)
;;   (require 'vimish-fold)
;;   (vimish-fold-global-mode 1)		; https://emacs.stackexchange.com/a/17168

;;   (global-set-key (kbd "C-c i f") #'vimish-fold)
;;   (global-set-key (kbd "C-c i d") #'vimish-fold-delete)

;;   (global-set-key (kbd "C-c i u") #'vimish-fold-unfold)
;;   (global-set-key (kbd "C-c i U") #'vimish-fold-unfold-all)
;;   (global-set-key (kbd "C-c i r") #'vimish-fold-refold)
;;   (global-set-key (kbd "C-c i R") #'vimish-fold-refold-all)
;;   (global-set-key (kbd "C-c i D") #'vimish-fold-delete-all)
;;   (global-set-key (kbd "C-c i t") #'vimish-fold-toggle)
;;   (global-set-key (kbd "C-c i T") #'vimish-fold-toggle-all)

;;   (global-set-key (kbd "C-c i n") #'vimish-fold-next-fold)
;;   (global-set-key (kbd "C-c i p") #'vimish-fold-previous-fold)
;;   )

(provide 'init-folding)
