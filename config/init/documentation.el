
;;; poporg mode
;;; https://github.com/pinard/poporg

(when (fboundp 'poporg-mode)
  ;; (use-existing-pkg poporg
  ;;   :bind (("C-c / /" . poporg-dwim)))

  (global-set-key (kbd "C-c / /") 'poporg-dwim)

  (progn
    (defun pop-rst-dwim ()
      (interactive)
      (poporg-dwim)
      (rst-mode)
      (poporg-mode +1))

    (global-set-key (kbd "C-c / r") 'pop-rst-dwim)))
