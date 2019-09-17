;; https://stackoverflow.com/a/11130708
(require 'rect)

(defun my-search-replace-in-rectangle
  (start end search-pattern replacement search-function literal)
  "Replace all instances of SEARCH-PATTERN (as found by SEARCH-FUNCTION)
with REPLACEMENT, in each line of the rectangle established by the START
and END buffer positions.

SEARCH-FUNCTION should take the same BOUND and NOERROR arguments as
`search-forward' and `re-search-forward'.

The LITERAL argument is passed to `replace-match' during replacement.

If `case-replace' is nil, do not alter case of replacement text."
  (apply-on-rectangle
   (lambda (start-col end-col search-function search-pattern replacement)
     (move-to-column start-col)
     (let ((bound (min (+ (point) (- end-col start-col))
                       (line-end-position)))
           (fixedcase (not case-replace)))
       (while (funcall search-function search-pattern bound t)
         (replace-match replacement fixedcase literal))))
   start end search-function search-pattern replacement))

(defun my-replace-regexp-rectangle-read-args (regexp-flag)
  "Interactively read arguments for `my-replace-regexp-rectangle'
or `my-replace-string-rectangle' (depending upon REGEXP-FLAG)."
  (let ((args (query-replace-read-args
               (concat "Replace"
                       (if current-prefix-arg " word" "")
                       (if regexp-flag " regexp" " string"))
               regexp-flag)))
    (list (region-beginning) (region-end)
          (nth 0 args) (nth 1 args) (nth 2 args))))

(defun my-replace-regexp-rectangle
  (start end regexp to-string &optional delimited)
  "Perform a regexp search and replace on each line of a rectangle
established by START and END (interactively, the marked region),
similar to `replace-regexp'.

Optional arg DELIMITED (prefix arg if interactive), if non-nil, means
replace only matches surrounded by word boundaries.

If `case-replace' is nil, do not alter case of replacement text."
  (interactive (my-replace-regexp-rectangle-read-args t))
  (when delimited
    (setq regexp (concat "\\b" regexp "\\b")))
  (my-search-replace-in-rectangle
   start end regexp to-string 're-search-forward nil))

(defun my-replace-string-rectangle
  (start end from-string to-string &optional delimited)
  "Perform a string search and replace on each line of a rectangle
established by START and END (interactively, the marked region),
similar to `replace-string'.

Optional arg DELIMITED (prefix arg if interactive), if non-nil, means
replace only matches surrounded by word boundaries.

If `case-replace' is nil, do not alter case of replacement text."
  (interactive (my-replace-regexp-rectangle-read-args nil))
  (let ((search-function 'search-forward))
    (when delimited
      (setq search-function 're-search-forward
            from-string (concat "\\b" (regexp-quote from-string) "\\b")))
    (my-search-replace-in-rectangle
     start end from-string to-string search-function t)))

(global-set-key (kbd "C-x r M-%") 'my-replace-string-rectangle)
(global-set-key (kbd "C-x r C-M-%") 'my-replace-regexp-rectangle)
