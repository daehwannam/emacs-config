(progn
  (require 'comint)
  (comment
    (defhydra dhnam/hydra-comint-previous-next-matching-input-from-input ()
      "comint previous/next matching input"
      ("p" comint-previous-matching-input-from-input)
      ("n" comint-next-matching-input-from-input))
    (progn
      (key-chord-define comint-mode-map "fp" 'dhnam/hydra-comint-previous-next-matching-input-from-input/comint-previous-matching-input-from-input)
      (key-chord-define comint-mode-map "fn" 'dhnam/hydra-comint-previous-next-matching-input-from-input/comint-next-matching-input-from-input)))

  (progn
    (define-key comint-mode-map (kbd "M-P") 'dhnam/comint-previous-matching-input-from-input-or-backward-list)
    (define-key comint-mode-map (kbd "M-N") 'dhnam/comint-next-matching-input-from-input-or-forward-list))

  (if (and nil (package-installed-p 'hydra))
      (progn
	    (defhydra dhnam/hydra-comint-previous-next-prompt ()
	      "comint previous/next promp"
	      ("p" comint-previous-prompt)
	      ("n" comint-next-prompt))
	    (define-key comint-mode-map (kbd "C-c C-p")
	      #'dhnam/hydra-comint-previous-next-prompt/comint-previous-prompt)
	    (define-key comint-mode-map (kbd "C-c C-n")
	      #'dhnam/hydra-comint-previous-next-prompt/comint-next-prompt))
    (progn
      (define-key comint-mode-map (kbd "C-c C-p") (make-repeatable-command 'comint-previous-prompt))
      (define-key comint-mode-map (kbd "C-c C-n") (make-repeatable-command 'comint-next-prompt)))))

(with-eval-after-load 'compile
  (comment
    (if (and nil (package-installed-p 'hydra))
        (progn
	      (defhydra dhnam/hydra-compilation-previous-next-error ()
	        "compilation previous/next error"
	        ("p" compilation-previous-error)
	        ("n" compilation-next-error))
	      (define-key compilation-shell-minor-mode-map
	        (kbd "C-c M-p") #'dhnam/hydra-compilation-previous-next-error/compilation-previous-error)
	      (define-key compilation-shell-minor-mode-map
	        (kbd "C-c M-n") #'dhnam/hydra-compilation-previous-next-error/compilation-next-error))
      (progn
        (define-key compilation-shell-minor-mode-map (kbd "C-c M-p")
	      (make-repeatable-command 'compilation-previous-error))
        (define-key compilation-shell-minor-mode-map (kbd "C-c M-n")
	      (make-repeatable-command 'compilation-next-error)))))

  (define-key compilation-shell-minor-mode-map (kbd "C-c ]")
    (make-repeatable-command 'compilation-next-error))
  (define-key compilation-shell-minor-mode-map (kbd "C-c [")
    (make-repeatable-command 'compilation-previous-error))
  (define-key compilation-shell-minor-mode-map (kbd "C-M-n") 'forward-list)
  (define-key compilation-shell-minor-mode-map (kbd "C-M-p") 'backward-list))

(comment
  (defun dhnam/comint-previous-input-or-compilation-previous-error (arg)
    "Cycle backwards through input history, saving input, or move
point to the previous error in the compilation buffer"
    (interactive "*p")
    (if (comint-after-pmark-p)
        (comint-previous-input arg)
      (compilation-previous-error arg)))

  (defun dhnam/comint-next-input-or-compilation-next-error (arg)
    "Cycle forwards through input history, saving input, or move
point to the next error in the compilation buffer"
    (interactive "*p")
    (if (comint-after-pmark-p)
        (comint-next-input arg)
      (compilation-next-error arg)))

  (defun dhnam/comint-previous-matching-input-from-input-or-compilation-previous-error (arg)
    "Cycle backwards through input history, saving input, or move
point to the previous error in the compilation buffer"
    (interactive "*p")
    (if (comint-after-pmark-p)
        (comint-previous-matching-input-from-input arg)
      (compilation-previous-error arg)))

  (defun dhnam/comint-next-matching-input-from-input-or-compilation-next-error (arg)
    "Cycle forwards through input history, saving input, or move
point to the next error in the compilation buffer"
    (interactive "*p")
    (if (comint-after-pmark-p)
        (comint-next-matching-input-from-input arg)
      (compilation-next-error arg)))

  (add-hook 'compilation-shell-minor-mode-hook
	        (lambda () (local-set-key (kbd "M-p") 'comint-previous-matching-input-from-input-or-compilation-previous-error)))

  (add-hook 'compilation-shell-minor-mode-hook
	        (lambda () (local-set-key (kbd "M-n") 'comint-next-matching-input-from-input-or-compilation-next-error))))

(progn
  (defun dhnam/comint-previous-matching-input-from-input-or-backward-list (arg)
    "Search backwards through input history for match for current input, or move
backward across one balanced group of parentheses."
    (interactive "^p")
    (if (comint-after-pmark-p)
        (comint-previous-matching-input-from-input arg)
      (backward-list arg)))

  (defun dhnam/comint-next-matching-input-from-input-or-forward-list (arg)
    "Search forwards through input history for match for current input, or move
forward across one balanced group of parentheses."
    (interactive "^p")
    (if (comint-after-pmark-p)
        (comint-next-matching-input-from-input arg)
      (forward-list arg))))

(provide 'dhnam-comint)
