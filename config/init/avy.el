
(use-existing-pkg avy
  :commands (avy-goto-char)
  :init
  (comment
    (comment (key-chord-define-global "jj" 'avy-goto-char))
    (global-set-key (kbd "M-'") 'avy-goto-char-2)
    (comment
      (comment (setq avy-timeout-seconds 0.5))
      (global-set-key (kbd "M-'") 'avy-goto-char-timer))
    (comment (global-set-key (kbd "M-'") 'swiper-avy)))

  (progn
    ;; <FFrancSign> = ₣
    (global-set-key (kbd "₣") 'avy-goto-char-timer)
    (comment (global-set-key (kbd "<FFrancSign>") 'avy-goto-char)))

  :config
  (progn
    (defun dhnam/avy-handler-default-advice (orig-fun &rest args)
      (assert (= (length args) 1))
      (let ((char (car args)))
        (when (equal char ?\₫)
          (setq char ?\C-g))
        (funcall orig-fun char)))

    (advice-add 'avy-handler-default :around #'dhnam/avy-handler-default-advice)))

(provide 'init-avy)
