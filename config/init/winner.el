(when (fboundp 'winner-mode)
  (winner-mode 1))

(defhydra hydra-winner ()
  ;; https://github.com/abo-abo/hydra/issues/79#issuecomment-83056585

  "Winner"
  ("k" (progn
         (winner-undo)
         (setq this-command 'winner-undo))
   "undo")
  ("l" winner-redo "redo"
   :exit t :bind nil))

(define-key winner-mode-map (kbd "C-c w") 'hydra-winner/body)
  

(provide 'init-winner)
