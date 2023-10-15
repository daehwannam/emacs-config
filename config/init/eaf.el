
;;; * EAF
;;; - web site: https://github.com/emacs-eaf/emacs-application-framework
;;; - location: ~/.emacs.d/package/emacs-application-framework/
;;;
;;; ** Setup
;;; 1. M-x dhnam/eaf-clone-project
;;; 2. Install dependencies
;;; #+begin_src sh
;;; cd ~/.emacs.d/package/emacs-application-framework/
;;; ./install-eaf.py --install-core-deps
;;; #+end_src
;;;

(defun dhnam/eaf-clone-project ()
  (interactive)
  (let ((url "https://github.com/emacs-eaf/emacs-application-framework.git")
        (path "~/.emacs.d/package/emacs-application-framework/"))
    (if (file-exists-p path)
        (message "The EAF project already exists.")
      (let ((cmd (format "git clone --depth=1 -b master %s %s" url path)))
        (dhnam/comint-with-command-in-same-window cmd "*eaf project clone*")))))

(let ((eaf-enabled (dhnam/machine-config-get-first 'eaf-enabled)))
  (when eaf-enabled
    ;; (comment (add-to-list 'load-path "~/.emacs.d/package/emacs-application-framework/"))

    (require 'eaf)
    (require 'eaf-browser)
    (require 'eaf-pdf-viewer)

    (comment (setq eaf-python-command "/usr/bin/python3"))

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

(provide 'init-eaf)
