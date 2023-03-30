
(comment
  (defvar dhnam/rectangle-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "k") 'kill-rectangle)
      (define-key map (kbd "M-w") 'copy-rectangle-as-kill)
      (define-key map (kbd "d") 'delete-rectangle)
      (define-key map (kbd "y") 'yank-rectangle)
      (define-key map (kbd "o") 'open-rectangle)
      (define-key map (kbd "N") 'rectangle-number-lines)
      (define-key map (kbd "c") 'clear-rectangle)
      (define-key map (kbd "t") 'string-rectangle)
      map)
    "Keymap for rectangle commands.")

  (fset 'dhnam/rectangle-map dhnam/rectangle-map)
  (comment (define-key global-map (kbd "C-x r") 'dhnam/rectangle-map))
  (key-chord-define-global "rl" dhnam/rectangle-map))

(provide 'init-rectangle)
