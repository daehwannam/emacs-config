;; use C-j as newline-and-indent rather than Enter.
(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))


(progn
  (comment
    (defun dhnam/indent-rigidly-pop-undo ()
      (interactive)
      ;; (keyboard-quit)
      ;; (undo)
      (pop buffer-undo-list)
      ))
  

  (setq indent-rigidly-map
        (let ((map (make-sparse-keymap)))
          (define-key map [left]  'indent-rigidly-left)
          (define-key map [right] 'indent-rigidly-right)
          (define-key map [S-left]  'indent-rigidly-left-to-tab-stop)
          (define-key map [S-right] 'indent-rigidly-right-to-tab-stop)
          (define-key map (kbd "j")  'indent-rigidly-left)
          (define-key map (kbd "l") 'indent-rigidly-right)
          (define-key map (kbd "J")  'indent-rigidly-left-to-tab-stop)
          (define-key map (kbd "L") 'indent-rigidly-right-to-tab-stop)
          (define-key map (kbd "q") 'undo)
          (define-key map (kbd "RET") 'keyboard-quit)
          map)))


(progn
  ;; https://emacs.stackexchange.com/a/12217
  ;; https://stackoverflow.com/a/37108216
  ;; https://stackoverflow.com/a/5528393
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)

  (defun dhnam/enable-indent-tabs-mode ()
    (setq indent-tabs-mode t))

  (add-hook 'makefile-mode-hook 'dhnam/enable-indent-tabs-mode))

