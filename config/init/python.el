
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
