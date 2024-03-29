(progn
  (defun dhnam/comint-send-input-for-hy (&optional no-newline artificial)
    "comint-send-input for Hy

Below is the original docstring of `comint-send-input':

  Send input to process.
After the process output mark, sends all text from the process mark to
point as input to the process.  Before the process output mark, calls
value of variable `comint-get-old-input' to retrieve old input, copies
it to the process mark, and sends it.

This command also sends and inserts a final newline, unless
NO-NEWLINE is non-nil.

Any history reference may be expanded depending on the value of the variable
`comint-input-autoexpand'.  The list of function names contained in the value
of `comint-input-filter-functions' is called on the input before sending it.
The input is entered into the input history ring, if the value of variable
`comint-input-filter' returns non-nil when called on the input.

If variable `comint-eol-on-send' is non-nil, then point is moved to the
end of line before sending the input.

After the input has been sent, if `comint-process-echoes' is non-nil,
then `comint-send-input' waits to see if the process outputs a string
matching the input, and if so, deletes that part of the output.
If ARTIFICIAL is non-nil, it inhibits such deletion.
Callers sending input not from the user should use ARTIFICIAL = t.

The values of `comint-get-old-input', `comint-input-filter-functions', and
`comint-input-filter' are chosen according to the command interpreter running
in the buffer.  E.g.,

If the interpreter is the csh,
    `comint-get-old-input' is the default:
	If `comint-use-prompt-regexp' is nil, then
	either return the current input field, if point is on an input
	field, or the current line, if point is on an output field.
	If `comint-use-prompt-regexp' is non-nil, then
	return the current line with any initial string matching the
	regexp `comint-prompt-regexp' removed.
    `comint-input-filter-functions' monitors input for \"cd\", \"pushd\", and
	\"popd\" commands.  When it sees one, it cd's the buffer.
    `comint-input-filter' is the default: returns t if the input isn't all white
	space.

If the Comint is Lucid Common Lisp,
    `comint-get-old-input' snarfs the sexp ending at point.
    `comint-input-filter-functions' does nothing.
    `comint-input-filter' returns nil if the input matches input-filter-regexp,
	which matches (1) all whitespace (2) :a, :c, etc.

Similarly for Soar, Scheme, etc."
    (interactive)
    ;; If we're currently completing, stop.  We're definitely done
    ;; completing, and by sending the input, we might cause side effects
    ;; that will confuse the code running in the completion
    ;; post-command-hook.
    (when completion-in-region-mode
      (completion-in-region-mode -1))
    ;; Note that the input string does not include its terminal newline.
    (let ((proc (get-buffer-process (current-buffer))))
      (if (not proc) (user-error "Current buffer has no process")
	    (widen)
	    (let* ((pmark (process-mark proc))
	           (intxt (if (>= (point) (marker-position pmark))
			              (progn (if comint-eol-on-send
				                     (if comint-use-prompt-regexp
					                     (end-of-line)
				                       (goto-char (field-end))))
				                 (buffer-substring pmark (point)))
			            (let ((copy (funcall comint-get-old-input)))
			              (goto-char pmark)
			              (insert copy)
			              copy)))
	           (input (if (not (eq comint-input-autoexpand 'input))
			              ;; Just whatever's already there.
			              intxt
			            ;; Expand and leave it visible in buffer.
			            (comint-replace-by-expanded-history t pmark)
			            (buffer-substring pmark (point))))
	           (history (if (not (eq comint-input-autoexpand 'history))
			                input
			              ;; This is messy 'cos ultimately the original
			              ;; functions used do insertion, rather than return
			              ;; strings.  We have to expand, then insert back.
			              (comint-replace-by-expanded-history t pmark)
			              (let ((copy (buffer-substring pmark (point)))
				                (start (point)))
			                (insert input)
			                (delete-region pmark start)
			                copy))))

	      (unless no-newline
	        (insert ?\n))

	      (comint-add-to-input-history history)

	      (run-hook-with-args 'comint-input-filter-functions
			                  (if no-newline input
				                (concat input "\n")))

	      (let ((beg (marker-position pmark))
		        (end (if no-newline (point) (1- (point)))))
	        (with-silent-modifications
	          (when (> end beg)
		        (add-text-properties beg end
				                     '(front-sticky t
						                            font-lock-face comint-highlight-input))
		        (unless comint-use-prompt-regexp
		          ;; Give old user input a field property of `input', to
		          ;; distinguish it from both process output and unsent
		          ;; input.  The terminating newline is put into a special
		          ;; `boundary' field to make cursor movement between input
		          ;; and output fields smoother.
		          (add-text-properties
		           beg end
		           '(mouse-face highlight
				                help-echo "mouse-2: insert after prompt as new input"))))
	          (unless (or no-newline comint-use-prompt-regexp)
		        ;; Cover the terminating newline
		        (add-text-properties end (1+ end)
				                     '(rear-nonsticky t
						                              field boundary
						                              inhibit-line-move-field-capture t)))))

	      (comint-snapshot-last-prompt)

	      (setq comint-save-input-ring-index comint-input-ring-index)
	      (setq comint-input-ring-index nil)
	      ;; Update the markers before we send the input
	      ;; in case we get output amidst sending the input.
	      (set-marker comint-last-input-start pmark)
	      (set-marker comint-last-input-end (point))
	      (set-marker (process-mark proc) (point))
	      ;; clear the "accumulation" marker
	      (set-marker comint-accum-marker nil)
	      (let ((comint-input-sender-no-newline no-newline))
	        (funcall comint-input-sender proc (concat pdb-hy-input-frame-left-side
						                              input
						                              pdb-hy-input-frame-right-side))) ; modified by dhnam

	      ;; Optionally delete echoed input (after checking it).
	      (when (and comint-process-echoes (not artificial))
	        (let ((echo-len (- comint-last-input-end
			                   comint-last-input-start)))
	          ;; Wait for all input to be echoed:
	          (while (and (> (+ comint-last-input-end echo-len)
			                 (point-max))
			              (accept-process-output proc)
			              (zerop
			               (compare-buffer-substrings
			                nil comint-last-input-start
			                (- (point-max) echo-len)
			                ;; Above difference is equivalent to
			                ;; (+ comint-last-input-start
			                ;;    (- (point-max) comint-last-input-end))
			                nil comint-last-input-end (point-max)))))
	          (if (and
		           (<= (+ comint-last-input-end echo-len)
		               (point-max))
		           (zerop
		            (compare-buffer-substrings
		             nil comint-last-input-start comint-last-input-end
		             nil comint-last-input-end
		             (+ comint-last-input-end echo-len))))
		          ;; Certain parts of the text to be deleted may have
		          ;; been mistaken for prompts.  We have to prevent
		          ;; problems when `comint-prompt-read-only' is non-nil.
		          (let ((inhibit-read-only t))
		            (delete-region comint-last-input-end
				                   (+ comint-last-input-end echo-len))
		            (when comint-prompt-read-only
		              (save-excursion
			            (goto-char comint-last-input-end)
			            (comint-update-fence)))))))

	      ;; This used to call comint-output-filter-functions,
	      ;; but that scrolled the buffer in undesirable ways.
	      (run-hook-with-args 'comint-output-filter-functions "")))))

  (add-hook 'pdb-mode-hook
	        (lambda () (local-set-key (kbd "M-j") #'dhnam/comint-send-input-for-hy)))

  (defun dhnam/load-commands-for-pdb-with-hy ()
    (interactive)
    (local-set-key (kbd "M-j") #'dhnam/comint-send-input-for-hy)))

(provide 'init-comint-for-hy)
