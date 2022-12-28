
(comment
 (use-existing-pkg undo-tree
   :init
   (progn
     (defalias 'dhnam/emacs-default-undo 'undo))

   :bind
   (:map undo-tree-map
	 ("C-_" . 'dhnam/emacs-default-undo)
	 ("C-/" . 'dhnam/emacs-default-undo))

   :config
   (progn
     (global-undo-tree-mode)))

 (comment
  (defalias 'dhnam/emacs-default-undo 'undo)
  (require 'undo-tree)
  (define-key undo-tree-map (kbd "C-_") 'dhnam/emacs-default-undo)
  (define-key undo-tree-map (kbd "C-/") 'dhnam/emacs-default-undo)))

(provide 'dhnam-undo-tree)
