
;;; Highlight Middle Line Number Mode
;;; Modified version of hlinum-mode (https://github.com/tom-tan/hlinum-mode/blob/master/hlinum.el)

(require 'linum)
(require 'cl)

(defface mlinum-highlight-face
    '((t (:inherit default :foreground "black"
          :background "gray")))
  "Face for highlighting middle line"
  :group 'linum)

(defcustom mlinum-highlight-in-all-buffersp
  nil
  "Non-nil means hmlinum highlights middle line in all buffers.
Otherwise hmlinum will highlight only in the active buffer."
  :type 'boolean
  :group 'linum)

(defun middle-position ()
  (condition-case nil
      (save-excursion
	(move-to-window-line nil)
	(point))
    ('error 0)))  ; exception handlling; return 0

;; (condition-case nil
;;       (save-excursion
;; 	(unless (eql (point) 1)
;; 	  (move-to-window-line nil)
;; 	  (point)))
;;     ('save-excursion nil))
  ;; (save-excursion
  ;;   (goto-char (window-start))
  ;;   (forward-line (/ (- (window-height (selected-window)) 1) 2))
  ;;   (point))
  ;; (save-excursion
  ;;   (goto-char (window-start))
  ;;   ;; (call-interactively 'move-to-window-line)
  ;;   (move-to-window-line nil)
  ;;   (point)))

(defun hmlinum-color (face &optional line)
  "Highlight line number LINE by using face FACE.
If LINE is nil, highlight current line."
  (save-excursion
    (when line (forward-line (- line (line-number-at-pos))))
    (let* ((pt (max (window-start)
                    (middle-position)))
           (ov (cl-find-if
                (lambda (e) (stringp (overlay-get e 'linum-str)))
                (overlays-in pt pt))))
      (when ov
        (let* ((str (overlay-get ov 'before-string))
               (lstr (overlay-get ov 'linum-str))
               (nov (move-overlay ov pt pt)))
          (add-text-properties 0 (length lstr)
                               `(face ,face) lstr)
          (add-text-properties 0 1 `(display ((margin left-margin)
                                              ,lstr)) str)
          (overlay-put nov 'before-string str)
          (overlay-put nov 'linum-str lstr))))))

(defun hmlinum-highlight-line (&optional line)
  (hmlinum-color 'mlinum-highlight-face line))
(defun hmlinum-unhighlight-line (&optional line)
  (unless mlinum-highlight-in-all-buffersp
    (hmlinum-color 'linum line)))

(defun hmlinum-highlight-region ()
  (when mark-active
    (cl-loop for l
             from (line-number-at-pos (region-beginning))
             to   (line-number-at-pos (region-end))
             do   (hmlinum-highlight-line l))))

(defun hmlinum-after-scroll (win start)
  (when (eq (current-buffer) (window-buffer))
(if mark-active
(hmlinum-highlight-region)
      (hmlinum-highlight-line))))

;;;###autoload
(defun hmlinum-activate ()
  "Enable highlighting current line number."
  (interactive)
  (advice-add 'linum-update-current :after 'hmlinum-highlight-line)
  (advice-add 'linum-after-scroll :after 'hmlinum-after-scroll)
  (add-hook 'pre-command-hook 'hmlinum-unhighlight-line)
  (add-hook 'post-command-hook 'hmlinum-highlight-region))

;;;###autoload
(defun hmlinum-deactivate ()
  "Disable highlighting current line number."
  (interactive)
  (remove-hook 'pre-command-hook 'hmlinum-unhighlight-line)
  (remove-hook 'post-command-hook 'hmlinum-highlight-region)
  (advice-remove 'linum-update-current 'hmlinum-highlight-line)
  (advice-remove 'linum-after-scroll 'hmlinum-after-scroll))

(provide 'hmlinum)
;;; hmlinum.el ends here
