
(comment
 (use-existing-pkg undo-tree
   :init
   (progn
     (defalias 'emacs-default-undo 'undo))

   :bind
   (:map undo-tree-map
	 ("C-_" . 'emacs-default-undo)
	 ("C-/" . 'emacs-default-undo))

   :config
   (progn
     (global-undo-tree-mode)))

 (comment
  (defalias 'emacs-default-undo 'undo)
  (require 'undo-tree)
  (define-key undo-tree-map (kbd "C-_") 'emacs-default-undo)
  (define-key undo-tree-map (kbd "C-/") 'emacs-default-undo)))
