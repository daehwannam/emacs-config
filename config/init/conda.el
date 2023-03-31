
(require 'dhnam-conda)

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

(provide 'init-conda)
