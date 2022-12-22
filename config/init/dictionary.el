


;;; conditional load
;; http://crazia.tistory.com/entry/Emacs-%EC%82%AC%EC%A0%84-%EC%93%B0%EA%B8%B0-dictionary-%ED%8C%A8%ED%82%A4%EC%A7%80-%EC%84%A4%EC%B9%98%ED%95%98%EA%B8%B0

;; (condition-case nil  ; http://stackoverflow.com/questions/7790382/how-to-determine-whether-a-package-is-installed-in-elisp
;;     (progn
;;       (load "dictionary-init")

;;       ;; dictionary key bindings
;;       (global-set-key "\C-cs" 'dictionary-search)
;;       (global-set-key "\C-cm" 'dictionary-match-words))
;;   ;(file-error (message "Org not available; not configuring") ))
;; )

(catch 'dict-path
  (dolist (path load-path)
    (if (string= (car (last (split-string path "/")))
		 "dictionary-1.10")
	(progn  ; then
	  (load "dictionary-init")
	  ;; dictionary key bindings
	  (global-set-key "\C-cs" 'dictionary-search)
	  (global-set-key "\C-cm" 'dictionary-match-words)
	  (throw 'dict-path t)))
    (progn nil)  ; else
    )
  )

(comment
  (when (require 'define-word nil t)
    (global-set-key (kbd "C-c d") 'define-word-at-point)
    (global-set-key (kbd "C-c D") 'define-word)
    (setq define-word-default-service 'offline-wikitionary)
    (setq define-word-offline-dict-directory
          (dhnam/machine-config-get-first 'define-word-offline-dict-directory-path))))

(progn
  (defvar lookup-word-search-query-table
    '(("daum" . "https://dic.daum.net/search.do?q=%s")
      ("collins" . "https://www.collinsdictionary.com/dictionary/english/%s")
      ("wiktionary" . "http://en.wiktionary.org/wiki/%s")))
  (defvar lookup-word-search-query "https://dic.daum.net/search.do?q=%s")
  (comment (setq lookup-word-search-query "https://www.collinsdictionary.com/dictionary/english/%s"))

  (defun lookup-word-switch-dictionary (dictionary-name)
    (interactive
     (list (completing-read "Dictionary: "
                            lookup-word-search-query-table
                            nil t)))
    (setq lookup-word-search-query (cdr (assoc dictionary-name lookup-word-search-query-table))))

  (defun get-lookup-word-candidate (&optional including-killed)
    (cond
     ((eq major-mode 'pdf-view-mode)
      (car (pdf-view-active-region-text)))
     ((use-region-p)
      (ignore-errors (buffer-substring-no-properties (region-beginning) (region-end))))
     (t
      (or (ignore-errors (substring-no-properties (thing-at-point 'word)))
          (when including-killed (current-kill 0))
          ""))))

  (defun lookup-word-from-web (word)
    (interactive
     (let ((candidate (get-lookup-word-candidate)))
       (list (read-string (format "Word (%s): " candidate)
                          nil nil candidate))))
    (eww (format lookup-word-search-query word)
         (unless (eq major-mode 'eww-mode) 4)))

  (progn
    (defun quit-window-and-other-window-backwards ()
      (interactive)
      (quit-window)
      (other-window-backwards))

    (defun lookup-word-from-web-other-window (word)
      (interactive
       (let ((candidate (get-lookup-word-candidate t)))
         (list (read-string (format "Word (%s): " candidate)
                            nil nil candidate))))
      (split-window-sensibly)
      (other-window 1)    
      (eww (format lookup-word-search-query word) (unless (eq major-mode 'eww-mode) 4))
      (comment (local-set-key (kbd "q") #'quit-window-and-other-window-backwards))
      (comment
       (with-help-window "*look-word-result*"
         ;; (split-window-sensibly)
         (pop-to-buffer "*look-word-result*")
         (eww (format lookup-word-search-query word))
         (switch-to-buffer "*eww*"))))

    (defun lookup-word-from-web-other-window-directly ()
      (interactive)
      (lookup-word-from-web-other-window (get-lookup-word-candidate t)))

    (comment (add-hook 'eww-mode-hook (lambda () (local-set-key (kbd "q") #'quit-window)))))

  (defun lookup-word-from-web-other-window-for-exwm (word)
    (interactive
     (progn
       (let ((keys (gethash (kbd "M-w") exwm-input--simulation-keys)))
         (dolist (key keys)
           (exwm-input--fake-key key)))
       (sleep-for 0.1)
       (let ((candidate (current-kill 0)))
         (list (read-string (format "Word (%s): " candidate)
                            nil nil candidate)))))
    (split-window-sensibly)
    (other-window 1)    
    (eww (format lookup-word-search-query word) 4))

  (comment
   (when (fboundp 'ispell)
     ;; https://www.reddit.com/r/emacs/comments/3yjzmu/comment/cye8lwu/?utm_source=share&utm_medium=web2x&context=3
     (autoload 'ispell-get-word "ispell")
     (defun lookup-word-from-web-at-point-with-ispell (word)
       (interactive (list (save-excursion (car (ispell-get-word nil)))))
       (lookup-word-from-web word))))

  (defun lookup-word-from-web-at-point (word)
    "Use `lookup-word-from-web-at-point' to lookup word at point.
When the region is active, lookup the marked phrase.
Prefix ARG lets you choose service.

In a non-interactive call SERVICE can be passed."
    ;; this is modified code of `define-word-at-point' from define-word package

    (interactive "P")
    (let ((word
           (cond
            ((eq major-mode 'pdf-view-mode)
             (car (pdf-view-active-region-text)))
            ((use-region-p)
             (buffer-substring-no-properties
              (region-beginning)
              (region-end)))
            (t
             (substring-no-properties
              (thing-at-point 'word))))))
      (lookup-word-from-web word)))

  (comment (global-set-key (kbd "C-c d") 'lookup-word-from-web-other-window))
  (comment (global-set-key (kbd "M-#") 'lookup-word-from-web-other-window))
  (comment (global-set-key (kbd "C-c d") 'lookup-word-from-web))
  (comment (global-set-key (kbd "C-c D") 'lookup-word-from-web-at-point))

  (progn
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "w") 'lookup-word-from-web-other-window)
      (define-key map (kbd "d") 'lookup-word-from-web-other-window-directly)
      (define-key map (kbd "s") 'lookup-word-switch-dictionary)

      (defvar lookup-word-prefix-map map
        "Keymap for workspace related commands."))

    (fset 'lookup-word-prefix-map lookup-word-prefix-map)

    (key-chord-define-global "d;" 'lookup-word-prefix-map)))
