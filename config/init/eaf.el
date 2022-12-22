
(let (;; (eaf-path (dhnam/machine-config-get-first 'eaf-path))
      (eaf-enabled (dhnam/machine-config-get-first 'eaf-enabled)))
  (when eaf-enabled  ; eaf-path
    (comment (add-to-list 'load-path eaf-path))
    (require 'eaf)
    (require 'eaf-browser)
    (require 'eaf-pdf-viewer)

    (setq eaf-python-command "/usr/bin/python3")

    (progn
       (eaf-bind-key scroll_right "C-b" eaf-browser-keybinding)
       (eaf-bind-key scroll_left "C-f" eaf-browser-keybinding))

    (progn
      (setq eaf-mindmap-dark-mode "follow")
      (setq eaf-browser-dark-mode "follow")
      (setq eaf-terminal-dark-mode nil)
      (setq eaf-pdf-dark-mode "ignore"))

    (comment (setq eaf-browser-translate-language ""))
    (comment (setq eaf-browser-translate-language "en"))
    (comment (setq eaf-browser-translate-language "ko"))

    (progn
      ;; make eaf as the default browser of emacs
      (setq browse-url-browser-function 'eaf-open-browser)
      (defalias 'browse-web #'eaf-open-browser))

    (setq eaf-browser-enable-adblocker t)
    (setq eaf-browser-download-path "~/Downloads")
    (setq eaf-browser-remember-history t)
    (setq eaf-browser-default-search-engine "google")
    (setq eaf-browse-blank-page-url "https://www.google.com")
    (setq eaf-browser-default-zoom 1.0)

    (progn
      (require 'eaf-org)
      )
    ))

;;; Note
;;; You may
;;; ModuleNotFoundError: No module named 'epc'
