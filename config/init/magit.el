
(setq domain-name (trim-string (get-string-from-file "~/.emacs.d/config/domain.txt")))
(if (or (string-equal domain-name "engels")
	(string-equal domain-name "vbox"))
    (progn
      (global-set-key (kbd "C-x g") 'magit-status)
      (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)))
