
(dhnam/install-package-unless-installed 'hydra)

(comment
  ;; Disable helpful messages for all hydra commands
  ;; https://github.com/abo-abo/hydra/issues/196#issuecomment-218900683
  (setq hydra-is-helpful nil))

(progn
  ;; Allow using key-chord
  ;; https://github.com/abo-abo/hydra/issues/292#issuecomment-408130658
  (defun hydra-default-pre ()))
