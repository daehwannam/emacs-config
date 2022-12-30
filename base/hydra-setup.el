
(dhnam/install-package-unless-installed 'hydra)

(comment
  ;; Disable helpful messages for all hydra commands
  ;; https://github.com/abo-abo/hydra/issues/196#issuecomment-218900683
  (setq hydra-is-helpful nil))

(defmacro clone-hydra (new-name old-name body &optional docstring &rest heads)
  ;; Modified from `defhydra+'
  "Redefine an existing hydra by adding new heads and give a new name.
`clone-hydra' is similar with `defhydra+'."
  (declare (indent defun) (doc-string 3))
  (unless (stringp docstring)
    (setq heads
          (cons docstring heads))
    (setq docstring nil))
  `(defhydra ,new-name ,(or body (hydra--prop old-name "/params"))
     ,(or docstring (hydra--prop old-name "/docstring"))
     ,@(cl-delete-duplicates
        (append (hydra--prop old-name "/heads") heads)
        :key #'car
        :test #'equal)))
(comment
  ;; TODO: modify `defhydra+' to define similar hydra
  ;; https://github.com/abo-abo/hydra/issues/185
  )

