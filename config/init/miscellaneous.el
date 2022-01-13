

;; highlight setting
;; (require 'highlight nil t)


;; yes or no question
;;
;; https://www.emacswiki.org/emacs/YesOrNoP
(defalias 'yes-or-no-p 'y-or-n-p)


(progn
  (comment (setq old-save-buffers-kill-function (key-binding (kbd "C-x C-c"))))
  (comment (setq old-save-buffers-kill-function #'save-buffers-kill-emacs))
  (setq old-save-buffers-kill-function #'save-buffers-kill-terminal)

  (defun save-buffers-kill-emacs-with-asking ()
    "Close only if y was pressed."
    (interactive)
    (if (y-or-n-p (format "Are you sure you want to close this frame? "))
	(progn
	  ;; (save-buffers-kill-emacs)
	  ;; (save-buffers-kill-terminal)  
	  (funcall old-save-buffers-kill-function))
      (message "Canceled frame close")))

  (when (or t (daemonp))
    (global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs-with-asking)
    (global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs-with-asking)
    (key-chord-define-global "xc" 'save-buffers-kill-emacs-with-asking)))

(progn
  ;; proced config
  (progn
    ;; open progn current window
    ;; https://emacs.stackexchange.com/a/44846
    (add-to-list 'display-buffer-alist '("proced" . (display-buffer-same-window))))

  (comment
   (defun proced-all (&optional arg)
     (interactive "P")
     (let ((proced-filter 'all))
       (proced arg))))

  (progn
    (defun proced-filter-interactive-all () (interactive) (proced-filter-interactive 'all))
    (defun proced-filter-interactive-user () (interactive) (proced-filter-interactive 'user))

    (add-hook 'proced-mode-hook (lambda () (key-chord-define-local "fl" 'proced-filter-interactive-all)))
    (add-hook 'proced-mode-hook (lambda () (key-chord-define-local "fu" 'proced-filter-interactive-user)))
    (comment (key-chord-define proced-mode-map "fl" 'proced-filter-interactive-all))
    (comment (key-chord-define proced-mode-map "fu" 'proced-filter-interactive-user))))
