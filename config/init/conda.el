(progn
  (defun dhnam/insert-source-conda ()
    (interactive)
    (insert "source $(conda info --base)/etc/profile.d/conda.sh"))

  (defun dhnam/dired-conda-env-activate (env-name)
    (interactive (list (dhnam/get-conda-activate-env)))
    (let ((dir-path (dhnam/string-trim
                     (shell-command-to-string
                      (format "echo $(conda info --base)/envs/%s/etc/conda/activate.d" env-name)))))
      ;; conda environment doesn't have the directory "$(conda info --base)/envs/%s/etc" by default
      (make-directory dir-path t)
      (dired dir-path)))

  (defun dhnam/get-conda-activate-env ()
    (completing-read "Environment name: " (pyvenv-virtualenv-list)
                     nil t nil 'pyvenv-workon-history nil nil))

  (defun dhnam/insert-conda-activate-env (env-name)
    (interactive (list (dhnam/get-conda-activate-env)))
    (insert "conda activate " env-name))

  (comment
    (defun dhnam/conda-env-create (env-name python-version &optional cmd-eval-fn)
      (interactive (list (read-string "Environment name: " nil 'dhnam/conda-new-env-name-history)
                         (read-string "Python version: " "3" 'dhnam/python-version-history)))
      (let ((cmd (format "conda create -y -n %s python=%s" env-name python-version)))
        (funcall (or cmd-eval-fn #'shell-command) cmd))))

  (defun dhnam/conda-env-remove (env-name &optional cmd-eval-fn)
    (interactive (list (dhnam/get-conda-activate-env)))
    (when (y-or-n-p (format "Are you sure you want to remove \"%s\"" env-name))
      (let ((cmd (format "conda env remove -n %s" env-name)))
        (comment (start-process-shell-command cmd nil cmd))
        (comment (shell-command cmd))
        (funcall (or cmd-eval-fn #'shell-command) cmd))))

  (defvar dhnam/python-debugger-install-command
    "pip install ipdb git+https://github.com/pdbpp/pdbpp.git"
    "Install ipbd and pdbpp"))

(progn
  (defvar dhnam/conda-new-env-name-history nil)
  (defvar dhnam/python-version-history nil))

(defmacro dhnam/define-conda-commands (package name run-command prefix-map mode-key-map)
  `(with-eval-after-load ',package
     (defun ,(dhnam/format-symbol 'dhnam/%s-send-conda-activate name) (env-name)
       (interactive (list (dhnam/get-conda-activate-env)))
       (,run-command (concat "conda activate " env-name)))

     (defun ,(dhnam/format-symbol 'dhnam/%s-send-conda-deactivate name) ()
       (interactive)
       (,run-command "conda deactivate"))

     (defun ,(dhnam/format-symbol 'dhnam/%s-send-conda-env-create name) (env-name python-version)
       (interactive (list (read-string "Environment name: " nil 'dhnam/conda-new-env-name-history)
                          (read-string "Python version: " "3" 'dhnam/python-version-history)))
       (,run-command (format "conda create -y -n %s python=%s" env-name python-version)))

     (defun ,(dhnam/format-symbol 'dhnam/%s-send-conda-env-remove name) (env-name)
       (interactive (list (dhnam/get-conda-activate-env)))
       (when (y-or-n-p (format "Are you sure you want to remove \"%s\"" env-name))
         (,run-command (concat "conda env remove -n " env-name))))

     (defun ,(dhnam/format-symbol 'dhnam/%s-install-python-debugger name) ()
       (interactive)
       (,run-command dhnam/python-debugger-install-command))

     (let ((map (make-sparse-keymap)))
       (define-key map (kbd "a") ',(dhnam/format-symbol 'dhnam/%s-send-conda-activate name))
       (define-key map (kbd "d") ',(dhnam/format-symbol 'dhnam/%s-send-conda-deactivate name))
       (define-key map (kbd "c") ',(dhnam/format-symbol 'dhnam/%s-send-conda-env-create name))
       (define-key map (kbd "r") ',(dhnam/format-symbol 'dhnam/%s-send-conda-env-remove name))

	   (defvar ,prefix-map map
	     "Keymap for conda in %s.")

       (fset ',prefix-map ,prefix-map)

       (key-chord-define ,mode-key-map "qn" ',prefix-map))))

(progn
  (defun dhnam/vterm-run-command (cmd)
    (vterm-insert cmd)
    (vterm-send-return))

  (dhnam/define-conda-commands
   vterm-seamless
   vterm
   dhnam/vterm-run-command
   dhnam/vterm-send-conda-prefix-map
   vterm-seamless-mode-map))

(progn
  (defun dhnam/shell-run-command (cmd)
    (insert cmd)
    (comint-send-input))

  (dhnam/define-conda-commands
   shell
   shell
   dhnam/shell-run-command
   dhnam/shell-send-conda-prefix-map
   shell-mode-map))

(progn
  (defun dhnam/term-run-command (cmd)
    (insert cmd)
    (term-send-input))

  (dhnam/define-conda-commands
   term
   term
   dhnam/term-run-command
   dhnam/term-send-conda-prefix-map
   term-mode-map))

(comment
  (with-eval-after-load 'vterm
    (defun dhnam/vterm-run-command (cmd)
      (vterm-insert cmd)
      (vterm-send-return))

    (defun dhnam/vterm-send-conda-activate (env-name)
      (interactive (list (dhnam/get-conda-activate-env)))
      (dhnam/vterm-run-command (concat "conda activate " env-name)))

    (defun dhnam/vterm-send-conda-deactivate ()
      (interactive)
      (dhnam/vterm-run-command "conda deactivate"))

    (defun dhnam/vterm-send-conda-env-create (env-name python-version)
      (interactive (list (read-string "Environment name: " nil 'dhnam/conda-new-env-name-history)
                         (read-string "Python version: " "3" 'dhnam/python-version-history)))
      (dhnam/vterm-run-command (format "conda create -y -n %s python=%s" env-name python-version)))

    (defun dhnam/vterm-send-conda-env-remove (env-name)
      (interactive (list (dhnam/get-conda-activate-env)))
      (when (y-or-n-p (format "Are you sure you want to remove \"%s\"" env-name))
        (dhnam/vterm-run-command (concat "conda env remove -n " env-name))))

    (defun dhnam/vterm-install-python-debugger ()
      (interactive)
      (dhnam/vterm-run-command dhnam/python-debugger-install-command))

    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "a") 'dhnam/vterm-send-conda-activate)
      (define-key map (kbd "d") 'dhnam/vterm-send-conda-deactivate)
      (define-key map (kbd "c") 'dhnam/vterm-send-conda-env-create)
      (define-key map (kbd "r") 'dhnam/vterm-send-conda-env-remove)

	  (defvar dhnam/vterm-send-conda-prefix-map map
	    "Keymap for conda in vterm.")

      (fset 'dhnam/vterm-send-conda-prefix-map dhnam/vterm-send-conda-prefix-map)

      (with-eval-after-load 'vterm-seamless
        (key-chord-define vterm-seamless-mode-map "qn" 'dhnam/vterm-send-conda-prefix-map))))

  (with-eval-after-load 'shell
    (defun dhnam/shell-run-command (cmd)
      (insert cmd)
      (comint-send-input))

    (defun dhnam/shell-send-conda-activate (env-name)
      (interactive (list (dhnam/get-conda-activate-env)))
      (dhnam/shell-run-command (concat "conda activate " env-name)))

    (defun dhnam/shell-send-conda-deactivate ()
      (interactive)
      (dhnam/shell-run-command "conda deactivate"))

    (defun dhnam/shell-send-conda-env-create (env-name python-version)
      (interactive (list (read-string "Environment name: " nil 'dhnam/conda-new-env-name-history)
                         (read-string "Python version: " "3" 'dhnam/python-version-history)))
      (dhnam/shell-run-command (format "conda create -y -n %s python=%s" env-name python-version)))

    (defun dhnam/shell-send-conda-env-remove (env-name)
      (interactive (list (dhnam/get-conda-activate-env)))
      (when (y-or-n-p (format "Are you sure you want to remove \"%s\"" env-name))
        (dhnam/shell-run-command (concat "conda env remove -n " env-name))))

    (defun dhnam/shell-install-python-debugger ()
      (interactive)
      (dhnam/shell-run-command dhnam/python-debugger-install-command))

    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "a") 'dhnam/shell-send-conda-activate)
      (define-key map (kbd "d") 'dhnam/shell-send-conda-deactivate)
      (define-key map (kbd "c") 'dhnam/shell-send-conda-env-create)
      (define-key map (kbd "r") 'dhnam/shell-send-conda-env-remove)

	  (defvar dhnam/shell-send-conda-prefix-map map
	    "Keymap for conda in shell.")

      (fset 'dhnam/shell-send-conda-prefix-map dhnam/shell-send-conda-prefix-map)

      (key-chord-define shell-mode-map "qn" 'dhnam/shell-send-conda-prefix-map)))

  (with-eval-after-load 'term
    (defun dhnam/term-run-command (cmd)
      (insert cmd)
      (term-send-input))

    (defun dhnam/term-send-conda-activate (env-name)
      (interactive (list (dhnam/get-conda-activate-env)))
      (dhnam/term-run-command (concat "conda activate " env-name)))

    (defun dhnam/term-send-conda-deactivate ()
      (interactive)
      (dhnam/term-run-command "conda deactivate"))

    (defun dhnam/term-send-conda-env-create (env-name python-version)
      (interactive (list (read-string "Environment name: " nil 'dhnam/conda-new-env-name-history)
                         (read-string "Python version: " "3" 'dhnam/python-version-history)))
      (dhnam/term-run-command (format "conda create -y -n %s python=%s" env-name python-version)))

    (defun dhnam/term-send-conda-env-remove (env-name)
      (interactive (list (dhnam/get-conda-activate-env)))
      (when (y-or-n-p (format "Are you sure you want to remove \"%s\"" env-name))
        (dhnam/term-run-command (concat "conda env remove -n " env-name))))

    (defun dhnam/term-install-python-debugger ()
      (interactive)
      (dhnam/term-run-command dhnam/python-debugger-install-command))

    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "a") 'dhnam/term-send-conda-activate)
      (define-key map (kbd "d") 'dhnam/term-send-conda-deactivate)
      (define-key map (kbd "c") 'dhnam/term-send-conda-env-create)
      (define-key map (kbd "r") 'dhnam/term-send-conda-env-remove)

	  (defvar dhnam/term-send-conda-prefix-map map
	    "Keymap for conda in term.")

      (fset 'dhnam/term-send-conda-prefix-map dhnam/term-send-conda-prefix-map)

      (key-chord-define term-mode-map "qn" 'dhnam/term-send-conda-prefix-map))))

(provide 'dhnam-conda)
