
;---;;; python-mode
;---;; https://www.emacswiki.org/emacs/ProgrammingWithPythonModeDotEl
;---;; https://gitlab.com/python-mode-devs/python-mode
;---(autoload 'python-mode "python-mode" "Python Mode." t)
;---(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;---add-to-list 'interpreter-mode-alist '("python" . python-mode))

;;; python-mode linum-mode setting
(add-hook 'python-mode-hook 'linum-mode)
