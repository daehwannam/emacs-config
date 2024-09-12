
(require 'dhnam-miscellaneous)

;; highlight setting
;; (require 'highlight nil t)


;; yes or no question
;;
;; https://www.emacswiki.org/emacs/YesOrNoP
(defalias 'yes-or-no-p 'y-or-n-p)


(progn
  (comment (setq old-save-buffers-kill-function (key-binding (kbd "C-x C-c"))))
  (comment (setq old-save-buffers-kill-function #'save-buffers-kill-emacs))
  (comment (setq old-save-buffers-kill-function #'save-buffers-kill-terminal))
  (setq old-save-buffers-kill-function #'save-buffers-kill-emacs)

  (defun dhnam/save-buffers-kill-emacs-with-asking ()
    "Close only if y was pressed."
    (interactive)
    (if (y-or-n-p (format "Are you sure you want to close this frame? "))
	    (progn
	      ;; (save-buffers-kill-emacs)
	      ;; (save-buffers-kill-terminal)
	      (funcall old-save-buffers-kill-function))
      (message "Canceled frame close")))

  (when (or t (daemonp))
    (global-set-key (kbd "C-x C-c") 'dhnam/save-buffers-kill-emacs-with-asking)
    (global-set-key (kbd "C-x C-c") 'dhnam/save-buffers-kill-emacs-with-asking)
    (key-chord-define-global "xc" 'dhnam/save-buffers-kill-emacs-with-asking)))

(progn
  ;; proced config
  (progn
    ;; open progn current window
    ;; https://emacs.stackexchange.com/a/44846
    (add-to-list 'display-buffer-alist '("proced" . (display-buffer-same-window))))

  (comment
   (defun dhnam/proced-all (&optional arg)
     (interactive "P")
     (let ((proced-filter 'all))
       (proced arg))))

  (progn
    (defun dhnam/proced-filter-interactive-all () (interactive) (proced-filter-interactive 'all))
    (defun dhnam/proced-filter-interactive-user () (interactive) (proced-filter-interactive 'user))

    (add-hook 'proced-mode-hook (lambda () (key-chord-define-local "fl" 'dhnam/proced-filter-interactive-all)))
    (add-hook 'proced-mode-hook (lambda () (key-chord-define-local "fu" 'dhnam/proced-filter-interactive-user)))
    (comment (key-chord-define proced-mode-map "fl" 'dhnam/proced-filter-interactive-all))
    (comment (key-chord-define proced-mode-map "fu" 'dhnam/proced-filter-interactive-user))))

(defun dhnam/kill-gc ()
  (interactive)
  (let ((path-file "~/gc-path.txt"))
    (kill-new (dhnam/string-trim (dhnam/get-string-from-file (dhnam/string-trim (dhnam/get-string-from-file path-file)))))))

(defun dhnam/insert-gc ()
  (interactive)
  (let ((path-file "~/gc-path.txt"))
    (insert (dhnam/string-trim (dhnam/get-string-from-file (dhnam/string-trim (dhnam/get-string-from-file path-file)))))))

(progn
  (define-key occur-mode-map "o" 'dhnam/occur-mode-goto-occurrence-current-window))

(with-eval-after-load 'diff-mode
  ;; M-o is originally mapped to `diff-goto-source'
  (define-key diff-mode-map (kbd "M-i") nil)
  (define-key diff-mode-map (kbd "M-o") nil)

  (define-key diff-mode-shared-map (kbd "RET") 'dhnam/diff-goto-conditionally)
  (define-key diff-mode-shared-map (kbd "o") 'dhnam/diff-goto-conditionally-in-same-window))

(provide 'init-miscellaneous)
