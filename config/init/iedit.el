
(when (fboundp 'iedit-mode)
  (global-set-key (kbd "C-c i ;") 'iedit-mode)) ; default key C-; is not work in Putty.

(provide 'init-iedit)
