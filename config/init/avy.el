
(use-existing-pkg avy
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
    (global-set-key (kbd "₣") 'avy-goto-char)
    (comment (global-set-key (kbd "<FFrancSign>") 'avy-goto-char))))

(provide 'dhnam-avy)
