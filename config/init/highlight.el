
(progn
  (defvar highlight-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "r") 'highlight-regexp)
      (define-key map (kbd ".") 'highlight-symbol-at-point)
      (define-key map (kbd "u") 'unhighlight-regexp)
      map)
    "Keymap for rectangle commands.")

  (fset 'highlight-map highlight-map)
  
  (key-chord-define global-map "hh" highlight-map))
