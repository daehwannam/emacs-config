
(comment (add-hook 'LaTeX-mode-hook 'linum-mode))

;;; preview pane mode
;; https://www.emacswiki.org/emacs/LaTeXPreviewPane
;;
;; https://stackoverflow.com/questions/757564/in-emacs-lisp-how-do-i-check-if-a-variable-is-defined
(if (boundp 'latex-preview-pane-enable)
    (latex-preview-pane-enable))

(progn
  ;; https://tex.stackexchange.com/a/967
  (add-hook 'LaTeX-mode-hook #'turn-on-flyspell))

(comment
  ;; Ignore crossref
  ;;
  ;; man bibtex : to avoid these automatic inclusions altogether, give this option a sufficiently large number
  ;; https://tex.stackexchange.com/a/123746
  ;;
  ;; how to configure bibtex command
  ;; https://tex.stackexchange.com/a/397667
  (eval-after-load 'tex
    '(setcdr (assoc "BibTeX" TeX-command-list)
             '("bibtex --min-crossrefs=10000 %s"
               TeX-run-BibTeX nil t :help "Run BibTeX with ..."))))

(progn
  ;; - Adding options -> https://tex.stackexchange.com/a/161303
  ;;
  ;; - Fixing errors below -> https://emacs-china.org/t/auctex-setup-synctex-with-pdf-tools-not-working/11257
  ;; ---------------------------------------------------------------------------------------------------------
  ;; pdf-info-query: epdfinfo: Unable to create synctex scanner, did you run latex with `--synctex=1' ?
  ;; ---------------------------------------------------------------------------------------------------------

  (comment (setq LaTeX-command-style '(("" "%(PDF)%(latex) -shell-escape %S%(PDFout)"))))
  (setq LaTeX-command-style '(("" "%(PDF)%(latex) -shell-escape %S%(PDFout) --synctex=1"))))


(progn
  (defun dhnam/pdf-view-previous-page-command-in-other-window (&optional n)
    (interactive)
    (dhnam/pdf-view-next-page-command-in-other-window (- (or n 1))))

  (defun dhnam/pdf-view-next-page-command-in-other-window (&optional n)
    (interactive)
    (with-current-buffer (window-buffer (next-window (selected-window)))
      (pdf-view-next-page-command n)))

  ;; ===========================================================
  ;; latex-mode-map vs. LaTeX-mode-map
  ;; -----------------------------------------------------------
  ;; latex-mode-map is used for the default latex-mode.
  ;; LaTeX-mode-map is used for AUCTeX.
  ;; ===========================================================

  (comment
    (eval-after-load "latex"
      (define-key LaTeX-mode-map (kbd "M-p") 'dhnam/pdf-view-previous-page-command-in-other-window)
      (define-key LaTeX-mode-map (kbd "M-n") 'dhnam/pdf-view-next-page-command-in-other-window)))

  (add-hook
   'LaTeX-mode-hook
   (lambda ()
     (local-set-key (kbd "M-p") 'dhnam/pdf-view-previous-page-command-in-other-window)
     (local-set-key (kbd "M-n") 'dhnam/pdf-view-next-page-command-in-other-window))))

(when (package-installed-p 'pdf-tools)
  (defvar dhnam/pdf-tools-initialized nil)
  (defun dhnam/initialize-pdf-tools ()
    (unless dhnam/pdf-tools-initialized
      (progn
        ;; install pdf-tools if didin't yet
        (pdf-tools-install))

      (custom-set-variables
       ;; Fix for the following error:
       ;; ---------------------------------------------------------------------------------
       ;; Warning (pdf-view): These modes are incompatible with `pdf-view-mode',
       ;; 	please deactivate them (or customize pdf-view-incompatible-modes): linum-mode
       ;; ---------------------------------------------------------------------------------

       '(pdf-view-incompatible-modes
         '(linum-relative-mode helm-linum-relative-mode nlinum-mode nlinum-hl-mode nlinum-relative-mode yalinum-mode)))

      (comment
        ;; continuous scrolling
        ;; https://github.com/politza/pdf-tools/issues/27#issuecomment-927129868
        (assert (package-installed-p 'quelpa))
        (quelpa '(pdf-continuous-scroll-mode
                  :fetcher github
                  :repo "dalanicolai/pdf-continuous-scroll-mode.el"))
        (add-hook 'pdf-view-mode-hook 'pdf-continuous-scroll-mode))

      (setq pdf-view-resize-factor 1.1)

      (define-key pdf-view-mode-map "D" 'doc-view-fit-window-to-page)

      (when (require 'buffer-move nil t)

        (defun dhnam/pdf-view-previous-page-in-multiple-columns-command (n)
          (interactive "p")
          (dhnam/pdf-view-next-page-in-multiple-columns-command (- n)))

        (comment
          (defun dhnam/pdf-view-next-page-in-multiple-columns-command (n)
            (interactive "p")

            (let ((window (selected-window))
                  (sorted-same-buffer-windows nil))
              (dotimes (n (length (window-list)))
                (when (eq (current-buffer) (window-buffer window))
                  (push window sorted-same-buffer-windows))
                (setq window (next-window window)))

              (let ((current-page (pdf-view-current-page (selected-window))))
                (mapcar (lambda (window)
                          (let ((offset (- (length (member window sorted-same-buffer-windows))
                                           (length (member (selected-window) sorted-same-buffer-windows))
                                           )))
                            (pdf-view-goto-page (+ offset current-page n) window)))
                        sorted-same-buffer-windows)))))

        (defun dhnam/pdf-view-next-page-in-multiple-columns-command (n &optional non-overlapping)
          (interactive "p")

          (let ((found nil)
                (leftmost-window (selected-window)))
            (while (not found)
              (let ((new-window (windmove-find-other-window 'left 1 leftmost-window)))
                (if new-window
                    (setq leftmost-window new-window)
                  (setq found t))))

            (let ((window leftmost-window)
                  (sorted-same-buffer-windows nil))
              (dotimes (i (length (window-list))) ; right (newly created) windows are first in window-list.
                (when (eq (current-buffer) (window-buffer window))
                  (push window sorted-same-buffer-windows))
                (setq window (next-window window)))

              (let ((current-page (pdf-view-current-page (selected-window))))
                (mapcar (lambda (window)
                          (let ((offset (- (length (member window sorted-same-buffer-windows))
                                           (length (member (selected-window) sorted-same-buffer-windows))
                                           )))
                            (pdf-view-goto-page (+ offset current-page
                                                   (if non-overlapping
                                                       (* n (length sorted-same-buffer-windows))
                                                     n))
                                                window)))
                        sorted-same-buffer-windows)))))

        (defun dhnam/pdf-view-previous-non-overlapping-page-in-multiple-columns-command (n)
          (interactive "p")
          (dhnam/pdf-view-next-non-overlapping-page-in-multiple-columns-command (- n)))

        (defun dhnam/pdf-view-next-non-overlapping-page-in-multiple-columns-command (n)
          (interactive "p")
          (dhnam/pdf-view-next-page-in-multiple-columns-command n t))

        (progn
          (eval
           `(defhydra dhnam-ijkl-pdf-view ()

              "ijkl"

              ("C-i" pdf-view-previous-line-or-previous-page)
              ("C-k" pdf-view-next-line-or-next-page)
              ("C-j" image-backward-hscroll)
              ("C-l" image-forward-hscroll)

              ("i" pdf-view-previous-page-command)
              ("k" pdf-view-next-page-command)
              ("j" pdf-view-previous-page-command)
              ("l" pdf-view-next-page-command)

              ("M-i" dhnam/pdf-view-previous-non-overlapping-page-in-multiple-columns-command)
              ("M-k" dhnam/pdf-view-next-non-overlapping-page-in-multiple-columns-command)
              ("M-j" dhnam/pdf-view-previous-page-in-multiple-columns-command)
              ("M-l" dhnam/pdf-view-next-page-in-multiple-columns-command)

              (,dhnam-ijkl/quit-key nil "quit")
              ("q" dhnam/none-command)
              ("w" dhnam/none-command)))

          (progn
            ;; disable any hint message
            (hydra-set-property 'dhnam-ijkl-pdf-view :verbosity 0))

          (defun dhnam/none-command () (interactive))

          (let ((k (if (boundp 'dhnam-ijkl/activation-key) dhnam-ijkl/activation-key "₢")))
            (define-key pdf-view-mode-map (kbd dhnam-ijkl/activation-key) 'dhnam-ijkl-pdf-view/body)
            (define-key pdf-view-mode-map (kbd dhnam-ijkl/quit-key) 'dhnam/none-command)))


        (progn
          ;; (define-key pdf-view-mode-map (kbd "<prior>") 'dhnam/pdf-view-previous-page-in-multiple-columns-command)
          ;; (define-key pdf-view-mode-map (kbd "<next>") 'dhnam/pdf-view-next-page-in-multiple-columns-command)
          (define-key pdf-view-mode-map (kbd "<up>") 'dhnam/pdf-view-previous-non-overlapping-page-in-multiple-columns-command)
          (define-key pdf-view-mode-map (kbd "<down>") 'dhnam/pdf-view-next-non-overlapping-page-in-multiple-columns-command)
          (define-key pdf-view-mode-map (kbd "<left>") 'dhnam/pdf-view-previous-page-in-multiple-columns-command)
          (define-key pdf-view-mode-map (kbd "<right>") 'dhnam/pdf-view-next-page-in-multiple-columns-command)

          (define-key pdf-view-mode-map (kbd "M-p") 'dhnam/pdf-view-previous-non-overlapping-page-in-multiple-columns-command)
          (define-key pdf-view-mode-map (kbd "M-n") 'dhnam/pdf-view-next-non-overlapping-page-in-multiple-columns-command)
          (define-key pdf-view-mode-map (kbd "M-b") 'dhnam/pdf-view-previous-page-in-multiple-columns-command)
          (define-key pdf-view-mode-map (kbd "M-f") 'dhnam/pdf-view-next-page-in-multiple-columns-command)))

      (progn
        ;; dark theme
        ;; https://www.reddit.com/r/emacs/comments/6905nl/comment/dh2qvw1/?utm_source=share&utm_medium=web2x&context=3
        (define-key pdf-view-mode-map (kbd "M") 'pdf-view-midnight-minor-mode))

      (progn
        (defun dhnam/pdfgrep-with-current-file (command-args)
          "This function is modified from `pdfgrep'. 
Run pdfgrep with user-specified COMMAND-ARGS, collect output in a buffer.
You can use \\[next-error], or RET in the `pdfgrep-buffer-name'
buffer, to go to the lines where PDFGrep found matches.  To kill
the PDFGrep job before it finishes, type \\[kill-compilation]."
          (interactive (list (read-shell-command "Run pdfgrep (like this): "
                                                 (let ((default-command
                                                         (concat (pdfgrep-default-command) "'"))
                                                       (appended-arg-str
                                                        (concat "' " (dhnam/get-current-file-path))))
					                               (cons (concat default-command appended-arg-str)
                                                         (1+ (length default-command))))
					                             'pdfgrep-history)))
          (unless pdfgrep-mode
            (error "PDFGrep is not enabled, run `pdfgrep-mode' first."))
          (unless (executable-find "pdfgrep")
            (error "The 'pdfgrep' command not available on your system."))
          (compilation-start command-args 'grep-mode
		                     (lambda (_x) pdfgrep-buffer-name)))

        (key-chord-define pdf-view-mode-map "sj" 'dhnam/pdfgrep-with-current-file))))

  (progn
    ;; dark theme color
    (comment (setq pdf-view-midnight-colors '("light gray" . "#002b36" )))
    (comment (setq pdf-view-midnight-colors '("light gray" . "#002420")))
    (comment (setq pdf-view-midnight-colors '("light gray" . "#001d1d")))
    (comment (setq pdf-view-midnight-colors '("light gray" . "#002824")))
    (setq pdf-view-midnight-colors '("light gray" . "#000000")))

  (add-hook 'doc-view-mode-hook 'dhnam/initialize-pdf-tools)
  (add-hook 'pdf-tools-enabled-hook 'pdf-view-midnight-minor-mode))

(when (progn
        ;; to update `TeX-view-program-selection' with setcar,
        ;; `TeX-view-program-selection' should be loaded by (require 'tex)
        (require 'tex nil t))

  (progn
    ;; Using pdf-tools as the default view for AUCTeX
    ;;
    ;; https://emacs.stackexchange.com/a/19475
    ;; https://emacs.stackexchange.com/a/19475

    ;; to use pdfview with auctex
    (comment
      (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
            ;; TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
            ;; TeX-source-correlate-start-server t ; not sure if last line is neccessary
            ))
    (setcar (cdr (assoc 'output-pdf TeX-view-program-selection)) "PDF Tools")

    ;; to have the buffer refresh after compilation
    (add-hook 'TeX-after-compilation-finished-functions
              #'TeX-revert-document-buffer))

  (comment
    ;; Okular as the default pdf viewer
    ;; https://www.emacswiki.org/emacs/AUCTeX#h5o-27
    (comment (setq TeX-view-program-selection '((output-pdf "Okular"))))
    (setcar (cdr (assoc 'output-pdf TeX-view-program-selection)) "Okular"))

  (progn
    ;; enable forward search
    ;; https://lists.gnu.org/archive/html/auctex/2021-07/msg00018.html
    (setq TeX-source-correlate-mode t
          TeX-source-correlate-start-server t)

    ;; [key bindings for forward/inverse search]
    ;; https://www.gnu.org/software/auctex/manual/auctex/I_002fO-Correlation.html
    ;; forward search -> TeX-view (C-c C-v)
    ;; inverse search -> C-down-mouse-1 for pdf-tools (or "shift + left-mouse" for Okular)
    )
  )

(require 'dhnam-bib-util)

(when (require 'biblio nil t)
  (defvar dhnam/langtool-initialized nil)
  (defun dhnam/initialize-biblio ()
    (interactive)

    (unless dhnam/langtool-initialized
      (setq dhnam/langtool-initialized t)

      (progn
        (define-key biblio-selection-mode-map (kbd "u") #'dhnam/biblio--copy-url)
        (define-key biblio-selection-mode-map (kbd "U") #'dhnam/biblio--copy-url-quit))

      (progn
        (defun dhnam/post-process-biblio-bibtex (bibtex)
          (let* ((normalized (replace-regexp-in-string
                              "@[a-zA-Z]*"
                              #'downcase ; (lambda (matched) (downcase matched))
                              bibtex))
                 (entries (split-string normalized "\n\n")))
            (if (= (length entries) 2)
                (if (string-match "@inproceedings" bibtex)
                    (car entries)
                  normalized)
              normalized)))

        (comment
          (defun dhnam/biblio--selection-copy-callback (bibtex entry)
            "Add BIBTEX (from ENTRY) to kill ring."
            (kill-new (dhnam/post-process-biblio-bibtex bibtex))
            (message "Killed bibtex entry for %S."
                     (biblio--prepare-title (biblio-alist-get 'title entry))))

          (defun dhnam/biblio--selection-insert-callback (bibtex entry)
            "Add BIBTEX (from ENTRY) to kill ring."
            (let ((target-buffer biblio--target-buffer))
              (with-selected-window (or (biblio--target-window) (selected-window))
                (with-current-buffer target-buffer
                  (insert (dhnam/post-process-biblio-bibtex bibtex)
                          "\n\n"))))
            (message "Inserted bibtex entry for %S."
                     (biblio--prepare-title (biblio-alist-get 'title entry)))))

        (comment
          (defun dhnam/biblio--selection-copy-callback-advice (orig-func bibtex &rest args)
            (funcall orig-func (cons (dhnam/post-process-biblio-bibtex bibtex) args)))

          (defun dhnam/biblio--selection-insert-callback-advice (orig-func bibtex &rest args)
            (funcall orig-func (cons (dhnam/post-process-biblio-bibtex bibtex) args))))

        (progn
          (defun dhnam/biblio-format-bibtex-advice (orig-func &rest args)
            (dhnam/post-process-biblio-bibtex (apply orig-func args)))
          (advice-add 'biblio-format-bibtex :around #'dhnam/biblio-format-bibtex-advice)
          (comment (advice-remove 'biblio-format-bibtex #'dhnam/biblio-format-bibtex-advice))))

      (progn
        (defun dhnam/biblio--copy-url-callback (bibtex entry)
          (let* ((metadata (biblio--selection-metadata-at-point)))
            (let-alist metadata
              (if .direct-url
                  (let ((url-str (replace-regexp-in-string "v..?$" "" .direct-url)))
                    (kill-new url-str)
                    (message (concat "Copied: " url-str))
                    )
                (user-error "This record does not contain a direct URL (try arXiv or HAL)")))))

        (defun dhnam/biblio--copy-url ()
          (interactive)
          (biblio--selection-forward-bibtex #'dhnam/biblio--copy-url-callback))

        (defun dhnam/biblio--copy-url-quit ()
          (interactive)
          (biblio--selection-forward-bibtex #'dhnam/biblio--copy-url-callback t)))))

  (add-hook 'bibtex-mode-hook 'dhnam/initialize-biblio))

(progn
  (defvar dhnam/langtool-initialized nil)
  (defun dhnam/initialize-langtool ()
    (interactive)

    (unless dhnam/langtool-initialized
      ;; langtool
      ;; https://github.com/mhayashi1120/Emacs-langtool

      (setq dhnam/langtool-initialized t)

      (let ((languagetool-dir-path (dhnam/machine-config-get-first 'languagetool-dir-path)))
        (when languagetool-dir-path
          ;; command-line
          (setq langtool-language-tool-jar
                (concat languagetool-dir-path "languagetool-commandline.jar"))

          (comment
            ;; language config
            (comment (setq langtool-mother-tongue "en-US"))
            (setq langtool-default-language "en-US"))

          (comment
            ;; server setting

            ;; HTTP server & client
            (setq langtool-language-tool-server-jar
                  (concat languagetool-dir-path "languagetool-server.jar"))
            (setq langtool-server-user-arguments '("-p" "8082"))

            ;; HTTP client
            (setq langtool-http-server-host "localhost"
                  langtool-http-server-port 8082)

            ;; ssl/tls config
            (setq langtool-http-server-stream-type 'tls))

          ;; langtool package load
          (require 'langtool)))

      (progn
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "c") 'langtool-check)
          (define-key map (kbd "C") 'langtool-check-done)
          (define-key map (kbd "s") 'langtool-switch-default-language)
          (define-key map (kbd "m") 'langtool-show-message-at-point)
          (define-key map (kbd "r") 'langtool-correct-buffer)

          (defvar langtool-prefix-map map
            "Keymap for workspace related commands."))

        (fset 'langtool-prefix-map langtool-prefix-map)

        (key-chord-define-global "t;" 'langtool-prefix-map))))

  (add-hook 'TeX-mode-hook 'dhnam/initialize-langtool))

(provide 'dhnam-latex)
