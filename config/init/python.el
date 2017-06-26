
;---;;; python-mode
;---;; https://www.emacswiki.org/emacs/ProgrammingWithPythonModeDotEl
;---;; https://gitlab.com/python-mode-devs/python-mode
;---(autoload 'python-mode "python-mode" "Python Mode." t)
;---(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;---add-to-list 'interpreter-mode-alist '("python" . python-mode))

;;; python-mode linum-mode setting
(add-hook 'python-mode-hook 'linum-mode)

;;; python 3 setting
;; https://askubuntu.com/questions/460668/how-to-use-python3-in-emacs-on-ubuntu-14-04
(setq python-shell-interpreter "python3")

;;; ipython
;; https://stackoverflow.com/a/17817119
;; (defun run-ipython ()
;;   (interactive)
;;   (term "ipython")
;;   (rename-buffer "*IPython*"))
(defun run-ipython ()
  (interactive)
  ;;; count windows: https://emacs.stackexchange.com/questions/3494/how-to-count-all-of-the-windows-in-a-frame
  (if (< (length (mapcar #'window-buffer (window-list))) 2)
      (split-window-right))
  (other-window 1)
  (named-term "ipython" "*IPython*"))