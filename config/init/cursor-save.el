;; http://ergoemacs.org/emacs/emacs_save_cursor_position.html

;; remember cursor position
(if (version< emacs-version "25.0")
    (progn
      (require 'saveplace)
      (setq-default save-place t))
  (save-place-mode 1))

(provide 'dhnam-cursor-save)
