
(comment
  ;; This modalka config crashes TeX input method
  (install-package-unless-installed 'modalka)

  (use-existing-pkg modalka
    :init
    (progn
      (assert (fboundp 'modalka-mode))
      (progn
        (modalka-define-kbd "M-F" "C-M-f")
        (modalka-define-kbd "M-B" "C-M-b")
        (modalka-define-kbd "M-A" "C-M-a")
        (modalka-define-kbd "M-E" "C-M-e"))
      (modalka-global-mode t))))
