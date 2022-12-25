
(progn
  (defvar dhnam/highlight-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "r") 'highlight-regexp)
      (define-key map (kbd ".") 'highlight-symbol-at-point)
      (define-key map (kbd "u") 'unhighlight-regexp)
      (define-key map (kbd "w") 'dhnam/swiper-over-highlights)
      map)
    "Keymap for rectangle commands.")

  (fset 'dhnam/highlight-map dhnam/highlight-map)
  
  (key-chord-define global-map "qh" dhnam/highlight-map))
