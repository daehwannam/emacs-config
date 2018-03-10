
(if (or (string-equal machine-domain "engels")
	(string-equal machine-domain "vbox"))
    (progn
      (global-set-key (kbd "C-x g") 'magit-status)
      (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)))
