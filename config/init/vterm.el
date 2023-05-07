(progn
  ;; vterm
  ;; https://github.com/akermu/emacs-libvterm
  ;;
  ;; * Requirements
  ;;
  ;; - GNU Emacs (>= 25.1) with module support. You can check that, by verifying that module-file-suffix is not nil.
  ;; - cmake (>= 3.11)
  ;; - libtool-bin (related issues: #66 #85)
  ;;
  ;; Install with apt:
  ;; $ sudo apt install cmake libtool-bin
  ;;
  ;; Install with conda:
  ;; $ conda install -y -c anaconda cmake
  ;; $ conda install -y -c conda-forge libtool
  ;; $ conda install -y gxx_linux-64
)

(use-existing-pkg vterm
  :bind
  (:map vterm-mode-map
   ("C-_" . vterm-undo)
   ("C-y" . dhnam/vterm-yank)

   ;; :map vterm-copy-mode-map
   ;; ("C-c C-k" . vterm-copy-mode-done)
   ("C-c C-k" . vtsl/deactivate)
   ("C-c C-j" . vtsl/activate))

  :commands (dhnam/vterm-new-instance
             dhnam/vterm-setup)
  :init
  (progn
    (key-chord-define-global "o2" 'dhnam/vterm-new-instance))

  :config
  (require 'dhnam-vterm)

  (progn
    ;; https://github.com/akermu/emacs-libvterm/issues/352#issuecomment-647913789
    (setq vterm-max-scrollback 100000))

  (progn
    (let ((map vterm-seamless-mode-map))
      (define-key map (kbd "C-z")           #'vterm-send-next-key)
      (define-key map (kbd "C-;")           #'vterm-send-next-key)
      (comment (define-key map (kbd "C-c C-j")       #'vterm-copy-mode))
      (define-key map (kbd "C-t")           #'dhnam/vterm-insert-tty-fix-template)
      ;; (define-key map (kbd "C-c c")         #'dhnam/vterm-send-conda-activate-env)
      ;; (define-key map (kbd "C-c C")         #'dhnam/vterm-send-conda-deactivate)
      (define-key map (kbd "M-9")           #'previous-buffer)
      (define-key map (kbd "M-0")           #'next-buffer)
      (define-key map (kbd "M-L")           #'dhnam/reverse-recenter-top-bottom)
      (define-key map (kbd "C-v")           #'dhnam/scroll-up-small)
      (define-key map (kbd "M-v")           #'dhnam/scroll-down-small)
      (define-key map (kbd "<f7>")          #'pop-to-mark-command)
      (define-key map (kbd "<f8>")          #'dhnam/unpop-to-mark-command)
      ;; (define-key map (kbd "C-r")           (vtsl/copy-mode-then 'isearch-backward))
      ;; (define-key map (kbd "C-s")           (vtsl/copy-mode-then 'isearch-forward))
      (define-key map (kbd "₣")             (vtsl/copy-mode-then 'avy-goto-char-timer))
      ;; (define-key map (kbd "M-r")           #'vtsl/vterm-send-ctrl-r)
      ;; (define-key map (kbd "M-s")           #'vtsl/vterm-send-ctrl-s)
      (define-key map (kbd "M-P")           #'vtsl/vterm-send-ctrl-r)
      (define-key map (kbd "M-N")           #'vtsl/vterm-send-ctrl-s)
      (define-key map (kbd "C-g")           #'vtsl/vterm-send-ctrl-c)

      (define-key map (kbd "C-c C-d")       #'pdb-tracking-mode)

      ;; key-chords
      (key-chord-define map "wj" 'vterm-copy-mode)
      (key-chord-define map "w;" 'vterm-send-next-key)
      (key-chord-define map "sj" (vtsl/copy-mode-then 'dhnam/swiper-within-region))
      (key-chord-define map "fj" (vtsl/copy-mode-then 'ctrlf-backward-default)))


    (setq pdb-tracking/check-python-shell-prompt-pdb-regexp nil)

    (progn
      (require 'pdb-tracking)

      (key-chord-define pdb-tracking-mode-map "r;" 'pdb-tracking/search-backward-pdbpp-arrow)

      (defun pdb-tracking/search-backward-pdbpp-arrow-advice-for-vterm (orig-fun &rest args)
        (vterm-copy-mode)
        (apply orig-fun args))

      (advice-add 'pdb-tracking/search-backward-pdbpp-arrow
                  :around 'pdb-tracking/search-backward-pdbpp-arrow-advice-for-vterm))))

(provide 'init-vterm)
