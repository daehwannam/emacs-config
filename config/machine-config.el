
(let ((default-option-file-name (concat dhnam/emacs-root-dir "config/options.txt"))
      (extra-option-file-name (concat dhnam/emacs-root-dir "config/options-extra.txt")))
  (unless (file-exists-p default-option-file-name)
    ;; initialize machine options if it's not exist.
    ;; https://stackoverflow.com/questions/17376706/in-emacs-lisp-how-can-i-append-a-string-to-a-file-that-i-dont-like-to-open
    ;; https://stackoverflow.com/questions/14071991/how-to-create-an-empty-file-by-elisp/14072295
    ;;
    ;; options are sorted in descending order of priority
    (write-region "(base other-machine option1 option2)" nil default-option-file-name))
  (let ((default-options (car (read-from-string (dhnam/get-string-from-file default-option-file-name))))
        (extra-options (when (file-exists-p extra-option-file-name)
                         (car (read-from-string (dhnam/get-string-from-file extra-option-file-name))))))
    (defvar dhnam/machine-options
      (append extra-options default-options)
      "Configuration options")))

(defvar dhnam/machine-config-list)
(setq dhnam/machine-config-list
      '((theme-style (white-theme-machine white))
        (ai-edu-key-binding (vbox t))
        (path-to-slime-helper
         (slime-setup "~/quicklisp/slime-helper.el")
         (descartes "~/quicklisp/slime-helper.el")
         (vbox "~/quicklisp/slime-helper.el")
         (ubuntu-laptop "~/quicklisp/slime-helper.el"))
        (path-to-inferior-lisp-program
         (slime-setup "/usr/bin/sbcl")
         (descartes "/usr/bin/sbcl")
         (vbox "/usr/bin/sbcl")
         (ubuntu-laptop-sbcl "/home/dhnam/program/miniconda3/envs/default/bin/sbcl"))
        (etags-list (vbox ("/usr/share/emacs/25.3/lisp" "~/.emacs.d")))
        (org-agenda-directory-pattern
         (ms-desktop ("e:/data/Dropbox/org/schedule/" ".*\\.org\\(\\.txt\\)?$"))
         (vbox ("~/data/Dropbox/org/schedule/" ".*\\.org\\(\\.txt\\)?$"))
         (descartes ("~/data/Dropbox/org/schedule/" ".*\\.org\\(\\.txt\\)?$"))
         (ubuntu-laptop ("~/Dropbox/org/schedule/" ".*\\.org\\(\\.txt\\)?$")))
        (pyvenv-workon-home-path
         (hegel "~/bin/anaconda2/envs/")
         (engels "~/program/miniconda3/envs/")
         (vbox "~/program/miniconda3/envs/")
         (kant "~/program/miniconda3/envs/")
         (erasmus "~/program/miniconda3/envs/")
         (anaconda-env "~/program/anaconda3/envs/")
         (miniconda-env "~/program/miniconda3/envs/"))
        (elpy-simple-config
         (min-elpy-setup t))
        (pyvenv-name
         ;; (py3-env "py3")
         ;; (engels "default")
         ;; (engels "py3")
         ;; (vbox "py3")
         ;; (kant "py3")
         )
        (elpy-flycheck-activate
         (engels nil)
         )
        (elpy-autopep8-activate
         (engels nil)
         )
        (eaf-enabled (eaf-setup t))
        (face-font (inconsolata-12 "Inconsolata-12")
                   (default-face-font default))
        (exwm-physical-monitor-names
         (descartes-dual ("HDMI-1-1" "DVI-I-1"))
         (descartes-dual-2 ("DVI-I-1" "HDMI-0"))
         (descartes ("HDMI-1-1" "DVI-I-1" "HDMI-0"))
         (ubuntu-laptop-dual ("HDMI-1" "eDP-1")))
        ;; (exwm-multiple-physical-monitor-layout
        ;;  (descartes descartes-triple))
        ;; (exwm-multiple-monitor-layout-type
        ;;  (descartes triple)
        ;;  (ubuntu-laptop nil))
        ;; (ewg/xrandr-update
        ;;  (ubuntu-laptop 'ewg/xrandr-dual-monitor-mirror-update))
        (exwm-font-height
         ;; (descartes 145)
         )
        (define-word-offline-dict-directory-path
          (ubuntu-laptop "~/data/language/dictionaries_enwiktionary/"))
        (languagetool-dir-path
         (descartes "~/program/LanguageTool/")
         (ubuntu-laptop "~/program/LanguageTool/"))
        (xcape-left-alt (ubuntu-laptop-left-alt-as-XF86WWAN "<XF86WWAN>"))

        (installable-packages
         ;; (base (vlf magit key-chord modalka evil avy hydra dash counsel multi-term))
         (base (vlf magit dash multi-term counsel async wgrep which-key paredit poporg ctrlf expand-region company vterm clipetty el-patch avy puni buffer-move yasnippet yasnippet-snippets git-modes))
         (org-setup ((comment org) (refreshing  org-contrib) valign org-fragtog ob-async))
         (documentation (auctex latex-preview-pane pdf-tools define-word pdfgrep biblio))
         (programming (realgud markdown-mode sphinx-doc ein hy-mode jsonnet-mode csv-mode lice))
         (python-setup (pyvenv highlight-indentation (quelpa yaml-mode :fetcher github :repo "yoshiki/yaml-mode")))
         (elpy-setup (elpy flycheck py-autopep8 blacken))
         (min-elpy-setup (elpy))
         (lsp-setup (dash lsp-mode lsp-ui flycheck company lsp-treemacs lsp-ivy dap-mode))
         (eglot-setup (eglot company))
         (scheme-setup (geiser geiser-guile ac-geiser))
         (anaconda-setup (anaconda-mode company-anaconda))
         (ms-laptop (vlf magit key-chord modalka evil dash))
         (ms-desktop (vlf magit key-chord modalka evil dash))
         (semparse (jsonnet-mode))
         (web-edit (atomic-chrome))
         (exwm-setup (exwm volume hide-mode-line ivy-posframe which-key-posframe))
         (vertico-setup (vertico orderless savehist vertico-posframe mini-frame marginalia consult embark-consult))
         (theme-setup (doom-themes))

         ;; =============================
         (vbox (vlf magit key-chord modalka evil dash  multi-term auctex latex-preview-pane material-theme elpy flycheck py-autopep8 blacken sphinx-doc workgroups2 perspective counsel iedit wgrep which-key counsel-projectile persp-projectile paredit markdown-mode tuareg ein hy-mode)) ; removed: highlight
         ;; (engels (vlf magit key-chord modalka evil dash elpy flycheck py-autopep8 blacken wgrep which-key projectile flx-ido))
         (engels (vlf magit key-chord modalka evil dash  multi-term elpy flycheck py-autopep8 blacken sphinx-doc wgrep which-key counsel counsel-projectile markdown-mode hy-mode))
         ;; (engels (vlf magit key-chord modalka evil dash elpy py-autopep8 blacken))
         )))

(progn
  (require 'cl-lib)

  ;; emacs configuration association list

  (comment
    (defun dhnam/machine-config-get (key)
      (car (cdr (cl-assoc dhnam/machine-options (cdr (assoc key dhnam/machine-config-list))
		                  :test (lambda (x y) (member y x))))))

    (defun dhnam/machine-config-get (key)
      (car (cdr (cl-assoc dhnam/machine-options (cdr (assoc key dhnam/machine-config-list))
		                  :test (lambda (x y) (eq (car x) y)))))))

  (defun dhnam/machine-config-get-first (key)
    (car (dhnam/machine-config-get-all key)))

  (defun dhnam/machine-config-get-all (key)
    (mapcar (lambda (x) (car (cdr x)))
	        (cl-remove-if-not (lambda (x) (member (car x) dhnam/machine-options))
			                  (cdr (assoc key dhnam/machine-config-list))))))
