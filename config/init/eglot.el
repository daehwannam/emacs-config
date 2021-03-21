
(when (package-installed-p 'eglot)
  (require 'eglot)
  (add-hook 'python-mode-hook 'eglot-ensure))
