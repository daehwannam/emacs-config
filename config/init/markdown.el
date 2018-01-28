

(let ((domain-name (trim-string (get-string-from-file "~/.emacs.d/config/domain.txt"))))
  (cond
   ((string-equal domain-name "engels")
    (progn
      (message "a")
      (message "b")
      (autoload 'markdown-mode "markdown-mode"
	"Major mode for editing Markdown files" t)
      (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
      (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
      (autoload 'gfm-mode "markdown-mode"
	"Major mode for editing GitHub Flavored Markdown files" t)
      (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))))))
