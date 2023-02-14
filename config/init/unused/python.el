(progn
  ;; use C-j as newline-and-indent
  (defun dhnam/non-electric-indent-mode ()
    (electric-indent-mode -1))
  (comment
    ;; https://github.com/jorgenschaefer/elpy/issues/195
    (add-hook 'python-mode-hook 'dhnam/non-electric-indent-mode)))

;;; python-mode linum-mode setting
(comment (add-hook 'python-mode-hook 'linum-mode))

;;---;;; python-mode
;;---;; https://www.emacswiki.org/emacs/ProgrammingWithPythonModeDotEl
;;---;; https://gitlab.com/python-mode-devs/python-mode
;;---(autoload 'python-mode "python-mode" "Python Mode." t)
;;---(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;;---add-to-list 'interpreter-mode-alist '("python" . python-mode))

;;; python 3 setting
;; https://askubuntu.com/questions/460668/how-to-use-python3-in-emacs-on-ubuntu-14-04
;; (setq python-shell-interpreter "python3")  ;; it occurs error for virtual environment of python2


;;; ipython
;; https://stackoverflow.com/a/17817119
;; (defun dhnam/run-ipython ()
;;   (interactive)
;;   (term "ipython")
;;   (rename-buffer "*IPython*"))
(comment
  (defun dhnam/run-ipython ()
    (interactive)
;;; count windows: https://emacs.stackexchange.com/questions/3494/how-to-count-all-of-the-windows-in-a-frame
    (if (< (length (mapcar #'window-buffer (window-list))) 2)
        (split-window-right))
    (other-window 1)
    (named-term "ipython" "*IPython*")))

;; (when (executable-find "ipython")
;;   (setq python-shell-interpreter "ipython"))

;; (cond
;;  ((string-equal machine-domain "vbox") ; Microsoft Windows
;;   (progn
;;     (setenv "WORKON_HOME" "~/bin/anaconda2/envs/")
;;     (pyvenv-mode 1)))
;;  ((string-equal machine-domain "engels") ; vbox linux
;;   (progn
;;     (setenv "WORKON_HOME" "~/bin/anaconda3/envs/")
;;     (pyvenv-mode 1)))
;; )


(defun dhnam/how-many-str (regexp str)
  (loop with start = 0
        for count from 0
        while (string-match regexp str start)
        do (setq start (match-end 0))
        finally return count))

(comment
  ;; not-used
  (defun dhnam/insert-py-code-to-hy-output-frame ()
    (interactive)
    (setq back-move-len (length "\"))))"))
    (insert pdb-hy-input-frame-left-side)
    (insert pdb-hy-input-frame-right-side)
    (backward-char back-move-len))

  (add-hook 'pdb-mode-hook
	        (lambda () (local-set-key (kbd "C-c C-j") #'dhnam/insert-py-code-to-hy-output-frame))))

(comment
  (defun dhnam/hy-pdb-send-input ()
    (interactive)
    (move-beginning-of-line 1)
    (insert pdb-hy-input-frame-left-side)
    (move-end-of-line 1)
    (insert pdb-hy-input-frame-right-side)
    (comint-send-input))

  (add-hook 'pdb-mode-hook
	        (lambda () (local-set-key (kbd "M-j") #'dhnam/hy-pdb-send-input))))
