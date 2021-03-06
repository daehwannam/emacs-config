;; ;;; it's modified version of *revive-mode-config.el*

;; (autoload 'restore-window-configuration "revive")
;; (autoload 'current-window-configuration-printable "revive")

;; (unless (file-exists-p "~/.emacs.d/layout/")
;;   ;; https://stackoverflow.com/questions/17376706/in-emacs-lisp-how-can-i-append-a-string-to-a-file-that-i-dont-like-to-open
;;   ;; https://stackoverflow.com/questions/14071991/how-to-create-an-empty-file-by-elisp/14072295
;;   (make-directory "~/.emacs.d/layout/"))

;; (defun revive-layout-read-args (prompt mustmatch)
;;   (list (read-file-name prompt "~/.emacs.d/layout/" nil mustmatch)))

;; (defun emacs-layout-test (out-name)
;;   "save the frame and window layout to ~/.layout. Requires revive.el."
;;   (interactive
;;    (revive-layout-read-args "Save the layout into: "
;; 			    (confirm-nonexistent-file-or-buffer))))

;; ;;;###autoload
;; (defun emacs-save-layout (out-name)
;;   "save the frame and window layout to ~/.layout. Requires revive.el."
;;   (interactive
;;    (revive-layout-read-args "Save the layout into: "
;; 			    (confirm-nonexistent-file-or-buffer)))
;;   (let ((frames (frame-list))
;;         (configs nil)
;;         (buffs (buffer-list))
;;         (filtered-buffs nil)
;;         (s-buffs nil))
;;     (dolist (b buffs)
;;       (let ((file-name (buffer-file-name b)))
;;         (when (and file-name
;;                    (> (length file-name) 0))
;;           (setq filtered-buffs (cons file-name filtered-buffs)))))
;;     (when filtered-buffs (setq filtered-buffs (reverse filtered-buffs)))
;;     (dolist (frame frames)
;;       (select-frame frame)
;;       (setq configs (cons (current-window-configuration-printable) configs)))
;;     (setq configs (cons filtered-buffs configs))
;;     (write-region (with-output-to-string (prin1 configs)) nil out-name)))


;; ;;;###autoload
;; (defun emacs-load-layout (in-name)
;;   "Load the layout saved by emacs-save-layout. Requires revive.el."
;;   (interactive
;;    (revive-layout-read-args "Load the layout from: "
;; 			    (confirm-nonexistent-file-or-buffer)))
;;   (let* ((config-count 0)
;;          (frames (frame-list))
;;          (configs nil)
;;          (frame-count (length frames))
;;          (buffs nil))
;;     (with-temp-buffer
;;       (insert-file-contents-literally in-name)
;;       (setq buffs (read (current-buffer)))
;;       (setq configs (rest buffs))
;;       (setq buffs (first buffs)))
;;     (dolist (b buffs)
;;       (find-file-noselect b)
;;       (message "Loading buffer %s" b))
;;     (setq config-count (length configs))
;;     (message "Config count is %s" config-count)
;;     (unless (>= frame-count config-count)
;;       (dotimes (i (- config-count frame-count))
;;         (make-frame))
;;       (setq frames (frame-list))
;;       (setq frame-count (length frames))
;;       (message "frame-count is %s" frame-count))
;;     (defun it (lconfigs lframes)
;;       (when (and lconfigs lframes)
;;         (select-frame (first lframes))
;;         (restore-window-configuration (first lconfigs))
;;         (it (rest lconfigs) (rest lframes))))
;;     (it configs frames)))


;; ;; (require 'revive-mode-config)

;; (global-set-key (kbd "C-c S") 'emacs-save-layout)
;; (global-set-key (kbd "C-c F") 'emacs-load-layout)
;; ;; (define-key ctl-x-map "S" 'emacs-save-layout)
;; ;; (define-key ctl-x-map "F" 'emacs-load-layout)
;; ;; (add-hook 'kill-emacs-hook 'emacs-save-layout)

;; ;; (emacs-save-layout "~/.emacs.d/layout/test")












(autoload 'save-current-configuration "revive" "Save status" t)
(autoload 'resume "revive" "Resume Emacs" t)
(autoload 'wipe "revive" "Wipe Emacs" t)

(global-set-key (kbd "C-c S") 'save-current-configuration)
(global-set-key (kbd "C-c F") 'resume)

