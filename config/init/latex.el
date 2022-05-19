
(comment (add-hook 'LaTeX-mode-hook 'linum-mode))

;;; preview pane mode
;; https://www.emacswiki.org/emacs/LaTeXPreviewPane
;;
;; https://stackoverflow.com/questions/757564/in-emacs-lisp-how-do-i-check-if-a-variable-is-defined
(if (boundp 'latex-preview-pane-enable)
    (latex-preview-pane-enable))

;; https://tex.stackexchange.com/a/967
(add-hook 'LaTeX-mode-hook #'turn-on-flyspell)

(comment
  ;; Ignore crossref
  ;;
  ;; man bibtex : to avoid these automatic inclusions altogether, give this option a sufficiently large number
  ;; https://tex.stackexchange.com/a/123746
  ;;
  ;; how to configure bibtex command
  ;; https://tex.stackexchange.com/a/397667
  (eval-after-load "tex"
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


(when (package-installed-p 'pdf-tools)
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

  (define-key pdf-view-mode-map "D" 'doc-view-fit-window-to-page))

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

(progn
  ;; bibliography utilities

  (comment
    (make-local-variable 'bibliography-file-name-as-source-of-reference)
    (setq-default bibliography-file-name-as-source-of-reference "some-bibliography.bib"))

  (defun get-start-end-content-positions-of-curly-brackets ()
    (let (start-pos end-pos)
      (save-excursion
        ;; find identifiers separated by curly bracket, comma, white-space
        (save-excursion
          (when (re-search-backward "\[{,[:blank:]\]" nil t)
            (setq start-pos (1+ (point)))))
        (save-excursion
          (when (re-search-forward "\[},[:blank:]\]" nil t)
            (setq end-pos (1- (point))))))
      (list start-pos end-pos)))

  (defun kill-current-content-of-curly-brackets-to-clipboard ()
    (interactive)
    (let (start-pos end-pos)
      (let ((start-end-pos-pair (get-start-end-content-positions-of-curly-brackets)))
        (setq start-pos (car start-end-pos-pair)
              end-pos (cadr start-end-pos-pair))
        (when (and start-pos end-pos)
          (copy-region-as-kill start-pos end-pos)))))

  (defun find-reference-in-bibliography-file () ;; (&optional opening-in-other-window)
    (interactive)
    (let ((opening-in-other-window t))
     (let (start-pos end-pos)
       (let ((start-end-pos-pair (get-start-end-content-positions-of-curly-brackets)))
         (setq start-pos (car start-end-pos-pair)
               end-pos (cadr start-end-pos-pair)))
       (let ((ref-id-str nil)
             (ref-id-str-valid nil))
         (when (and start-pos end-pos)
           (setq ref-id-str (trim-string (buffer-substring-no-properties start-pos end-pos)))
           (message ref-id-str)
           (setq ref-id-str-valid (not (or (string-match-p "[{}]" ref-id-str)
                                           (string-empty-p ref-id-str))))
           (when ref-id-str-valid
             (let ((ref-pos nil))
               (funcall (if opening-in-other-window #'find-file-other-window #'find-file)
                        bibliography-file-name-as-source-of-reference)
               (save-excursion
                 (beginning-of-buffer)
                 (re-search-forward ref-id-str)
                 (setq ref-pos (point)))
               (when ref-pos
                 (goto-char ref-pos)
                 (recenter-top-bottom)))))

         (comment
           (unless ref-id-str-valid
             (comment (xref-find-definitions (xref-backend-identifier-at-point (xref-find-backend))))
             (call-interactively 'xref-find-definitions)))))))

  (defun open-pdfurl-of-reference-in-bibliography-file ()
    (interactive)
    (find-reference-in-bibliography-file)
    (re-search-forward "\\(pdfurl\\|@\\)")
    (re-search-forward "{")
    (re-search-forward "[^ ]")
    (let ((original-kill-ring kill-ring))
      (my-org-kill-link-to-clipboard)
      (unless (eq original-kill-ring kill-ring)
        (let ((url (pop kill-ring)))
          (exwm-my-command-open-firefox url)))))


  (comment
    (setq-default pdf-file-dir-path-as-source-of-reference "some-pdf-directory-path"))

  (defun open-pdf-file-of-reference ()
    (interactive)
    (let (start-pos end-pos)
      (let ((start-end-pos-pair (get-start-end-content-positions-of-curly-brackets)))
        (setq start-pos (car start-end-pos-pair)
              end-pos (cadr start-end-pos-pair)))
      (let ((ref-id-str nil)
            (ref-id-str-valid nil))
        (when (and start-pos end-pos)
          (setq ref-id-str (trim-string (buffer-substring-no-properties start-pos end-pos)))
          (message ref-id-str)
          (setq ref-id-str-valid (not (or (string-match-p "[{}]" ref-id-str)
                                          (string-empty-p ref-id-str))))
          (when ref-id-str-valid
            (let* ((file-name (concat
                               (replace-regexp-in-string
                                "/" "+" (replace-regexp-in-string
                                         ":" "=" ref-id-str))
                               ".pdf"))
                   (file-path (dhnam/join-dirs pdf-file-dir-path-as-source-of-reference file-name)))

              (if (file-exists-p file-path)
                  (find-file-other-window file-path)
                (message (format "The file %s doesn't exist" file-path))))))

        (unless ref-id-str-valid
          (message "Invalid cursor position")))))

  (defun pdfgrep-with-default-dir (command-args)
    "This function is modified from `pdfgrep'. 
Run pdfgrep with user-specified COMMAND-ARGS, collect output in a buffer.
You can use \\[next-error], or RET in the `pdfgrep-buffer-name'
buffer, to go to the lines where PDFGrep found matches.  To kill
the PDFGrep job before it finishes, type \\[kill-compilation]."
    (interactive (list (read-shell-command "Run pdfgrep (like this): "
                                           (let ((default-command
                                                   (concat (pdfgrep-default-command) "'"))
                                                 (appended-arg-str
                                                  (concat "' " (dhnam/join-paths pdf-file-dir-path-as-source-of-reference "*"))))
					                         (cons (concat default-command appended-arg-str)
                                                   (1+ (length default-command))))
					                       'pdfgrep-history)))
    (unless pdfgrep-mode
      (error "PDFGrep is not enabled, run `pdfgrep-mode' first."))
    (unless (executable-find "pdfgrep")
      (error "The 'pdfgrep' command not available on your system."))
    (compilation-start command-args 'grep-mode
		               (lambda (_x) pdfgrep-buffer-name))))

(progn
  ;; langtool
  ;; https://github.com/mhayashi1120/Emacs-langtool

  (let ((languagetool-dir-path (machine-config-get-first 'languagetool-dir-path)))
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

    (key-chord-define-global "t;" 'langtool-prefix-map)))
