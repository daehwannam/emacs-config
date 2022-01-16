
(use-existing-pkg counsel
  :init
  (progn
    (defun swiper--recenter-p ()
      ;; fix for not applying scroll on exit in terminal emacs
      ;; https://github.com/abo-abo/swiper/issues/2159#issuecomment-514967202
      t)

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

      (comment
        (defun swiper-over-highlights-simple ()
          (interactive)
          (let ((original-swiper--candidates (symbol-function 'swiper--candidates)))
            (cl-letf (((symbol-function 'swiper--candidates)
                       (lambda ()
                         (let ((pattern (mapconcat #'car hi-lock-interactive-patterns "\\|")))
                           (cl-remove-if-not (lambda (x) (string-match-p pattern x))
                                             (funcall original-swiper--candidates))))))
              (swiper)))))


      (defun swiper-over-highlights (&optional initial-input)
        (interactive)
        (let ((original-swiper--candidates (symbol-function 'swiper--candidates))
              (pattern (mapconcat #'car hi-lock-interactive-patterns "\\|")))
          (cl-letf (((symbol-function 'swiper--candidates)
                     (lambda ()
                       (cl-remove-if-not (lambda (x) (string-match-p pattern x))
                                         (funcall original-swiper--candidates)))))
            (let ((candidates (swiper--candidates)))
              (swiper--init)
              (setq swiper-invocation-face
                    (plist-get (text-properties-at (point)) 'face))
              (let ((preselect
                     (save-excursion
                       (search-forward-regexp pattern nil t)
                       (let* ((current-line-value (current-line))
                              (candidate-line-numbers (mapcar (lambda (x) (cadr (text-properties-at 0 x)))
                                                              candidates))
                              (preselect-line-num (cl-find-if (lambda (x) (<= current-line-value x))
                                                              candidate-line-numbers)))
                         (- (length candidate-line-numbers)
                            (length (member preselect-line-num candidate-line-numbers))))))
                    (minibuffer-allow-text-properties t)
                    res)
                (unwind-protect
                     (and
                      (setq res
                            (ivy-read
                             "Swiper: "
                             candidates
                             :initial-input initial-input
                             :keymap swiper-map
                             :preselect preselect
                             :require-match t
                             :action #'swiper--action
                             :re-builder #'swiper--re-builder
                             :history 'swiper-history
                             :extra-props (list :fname (buffer-file-name))
                             :caller 'swiper))
                      (point))
                  (unless (or res swiper-stay-on-quit)
                    (goto-char swiper--opoint))
                  (isearch-clean-overlays)
                  (unless (or res (string= ivy-text ""))
                    (cl-pushnew ivy-text swiper-history))
                  (setq swiper--current-window-start nil)
                  (when swiper--reveal-mode
                    (reveal-mode 1)))))))))

    (defun swiper-within-region (&optional initial-input)
      "`isearch-forward' with an overview.
When non-nil, INITIAL-INPUT is the initial search pattern."
      (interactive)
      (if (use-region-p)
          (save-restriction
            (deactivate-mark t)
            (narrow-to-region (region-beginning) (region-end))
            (swiper initial-input))
        (swiper initial-input)))

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
	(comment (key-chord-define-global "js" 'swiper))
	(key-chord-define-global "js" 'swiper-within-region)
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
      ;; (key-chord-define-global ";f" 'counsel-find-file)
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

  :after (key-chord)

  :config
  (progn
    (progn
      ;; replace `ivy-partial-or-done'
      (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-partial)

      ;; disable magic-tilde
      (setq ivy-magic-tilde nil))))

(use-existing-pkg counsel-projectile-mode
  :init
  (counsel-projectile-mode))
