
(use-existing-pkg counsel
  :init
  (progn
    (defun swiper-with-text-in-region (start end)
      (interactive "r")
      (deactivate-mark)
      (swiper (buffer-substring start end)))

    (progn
      ;; https://github.com/abo-abo/swiper/issues/1206#issuecomment-345455888

      (defun ivy-ignore-buffers-with-different-major-mode (str)
        "Return non-nil if STR names a buffer of a different major mode.
This function is intended for use with `ivy-ignore-buffers'."
        (let ((current-major-mode (buffer-local-value 'major-mode (current-buffer)))
              (buf (get-buffer str)))
          (or (not buf)
              (not (eq (buffer-local-value 'major-mode buf)
                       current-major-mode)))))

      (defun counsel-switch-buffer-within-same-major-mode ()
        "Switch to another buffer within the same major mode.
Display a preview of the selected ivy completion candidate buffer
in the current window."
        (interactive)
        (let ((ivy-update-fns-alist
               '((ivy-switch-buffer . counsel--switch-buffer-update-fn)))
              (ivy-unwind-fns-alist
               '((ivy-switch-buffer . counsel--switch-buffer-unwind)))

              (ivy-ignore-buffers (cons #'ivy-ignore-buffers-with-different-major-mode
                                        ivy-ignore-buffers)))
          (ivy-read "Switch to buffer: " #'internal-complete-buffer
                    :keymap ivy-switch-buffer-map
                    :preselect (buffer-name (other-buffer (current-buffer)))
                    :action #'ivy--switch-buffer-action
                    :matcher #'ivy--switch-buffer-matcher
                    :caller 'ivy-switch-buffer)))

      (key-chord-define-global "qm" 'counsel-switch-buffer-within-same-major-mode))

    (progn
      ;; swiper within highlited strings
      (require 'cl-lib)

      (defun swiper-over-highlights ()
        (interactive)
        (let ((original-swiper--candidates (symbol-function 'swiper--candidates)))
          (cl-letf (((symbol-function 'swiper--candidates)
                     (lambda ()
                       (let ((pattern (mapconcat #'car hi-lock-interactive-patterns "\\|")))
                         (cl-remove-if-not (lambda (x) (string-match-p pattern x))
                                           (funcall original-swiper--candidates))))))
            (swiper)))))

    (progn
      ;; ivy mode

      ;; http://oremacs.com/swiper/
      ;; Initialization
      ;; (ivy-mode 1)
      (setq ivy-use-virtual-buffers t)
      (setq ivy-count-format "(%d/%d) ")

      (progn
	;; swiper
	;; Ivy-based interface to standard commands
	;; (global-set-key (kbd "C-s") 'isearch-forward)
	;; (global-set-key (kbd "C-c s") 'swiper)
	(key-chord-define-global "js" 'swiper)
	(comment (space-chord-define-global "s" 'swiper))
	;; (global-set-key (kbd "C-c C-s") 'swiper) ; conflict with 'elpy-rgrep-symbol
	;; (global-set-key (kbd "C-r") 'swiper)
	)

      ;; Ivy-resume and other commands
      ;; ivy-resume resumes the last Ivy-based completion.
      (global-set-key (kbd "C-c C-r") 'ivy-resume))

    (key-chord-define-global "qb" 'counsel-switch-buffer)
    (comment
     (unless (and (boundp 'exwm-cmd-arg-passed) exwm-cmd-arg-passed)
       (global-set-key (kbd "C-x b") 'counsel-switch-buffer)
       (global-set-key (kbd "C-x B") 'switch-to-buffer)))

    (progn
      ;; counsel-mode
      (global-set-key (kbd "M-x") 'counsel-M-x)
      ;; (global-set-key (kbd "C-x C-f") 'counsel-find-file)
      (global-set-key (kbd "<f1> f") 'counsel-describe-function)
      (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
      (global-set-key (kbd "<f1> l") 'counsel-find-library)
      (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
      (global-set-key (kbd "<f2> u") 'counsel-unicode-char)

      ;; Ivy-based interface to shell and system tools
      (global-set-key (kbd "C-c g") 'counsel-git)
      (global-set-key (kbd "C-c j") 'counsel-git-grep)
      (global-set-key (kbd "C-c k") 'counsel-ag)
      (global-set-key (kbd "C-x l") 'counsel-locate)
      (global-set-key (kbd "C-S-o") 'counsel-rhythmbox))

    ;; (global-set-key (kbd "C-c C-s") 'swiper-with-text-in-region)
    )

  :after (key-chord))

(use-existing-pkg counsel-projectile-mode
  :init
  (counsel-projectile-mode))
