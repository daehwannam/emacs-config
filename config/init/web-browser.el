

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

(use-existing-pkg eww
  :commands (dhnam/eww-new)
  :config
  (require 'dhnam-web-browser)

  :init
  (key-chord-define-global "i1" 'dhnam/eww-new))

(provide 'init-web-browser)
