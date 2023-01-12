

(comment
 ;; edit server doesn't work with overleaf!

 (progn
   ;; Chrome edit server

   (use-existing-pkg edit-server
     :ensure t
     :commands edit-server-start
     :init (if after-init-time
               (edit-server-start)
             (add-hook 'after-init-hook
                       #'(lambda() (edit-server-start))))
     :config (setq edit-server-new-frame-alist
                   `((name . "Edit with Emacs FRAME")
                     (top . 200)
                     (left . 200)
                     (width . 80)
                     (height . 25)
                     (minibuffer . t)
                     (menu-bar-lines . t)
                     (window-system . ,(or window-system x)))))


   (comment
    ;; default edit-server map
    (defvar dhnam/edit-server-edit-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-x C-s") 'edit-server-save)
        (define-key map (kbd "C-x #")   'edit-server-done)
        (define-key map (kbd "C-c C-c") 'edit-server-done)
        (define-key map (kbd "C-x C-c") 'edit-server-abort)
        map)))))


(use-existing-pkg atomic-chrome
  :init (atomic-chrome-start-server)
  :config
  (setq atomic-chrome-url-major-mode-alist
	'(("overleaf\\.com" . latex-mode))))

(progn
  (defun dhnam/quit-window-and-other-window-backwards ()
    (interactive)
    (quit-window)
    (dhnam/other-window-backwards))

  (add-hook 'eww-mode-hook (lambda () (local-set-key (kbd "Q") #'dhnam/quit-window-and-other-window-backwards)))

  (progn
    ;; disable making new tab when `eww-open-in-new-buffer' is called
    (setq eww-browse-url-new-window-is-tab nil)))

(progn
  (defun dhnam/eww-new (url &optional arg)
    "Make new eww buffer"
    (interactive
     (let* ((uris (eww-suggested-uris))
	        (prompt (concat "Enter URL or keywords"
			                (if uris (format " (default %s)" (car uris)) "")
			                ": ")))
       (list (read-string prompt nil 'eww-prompt-history uris))))

    (let ((default-prefix-arg 4))
      (eww url default-prefix-arg)))

  (key-chord-define-global "i1" 'dhnam/eww-new))

(provide 'dhnam-web-browser)
