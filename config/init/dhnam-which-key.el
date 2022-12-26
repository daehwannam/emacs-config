
(when (fboundp 'which-key-mode)
  (which-key-mode))

(when (fboundp 'which-key-posframe-mode)
  (with-eval-after-load 'which-key-posframe
    (comment (assert (fboundp 'ivy-posframe-mode)))

    ;; `which-key-posframe--show-buffer' is updated to fix the problem of hidden posframe by X windows
    ;; https://github.com/ch11ng/exwm/issues/550#issuecomment-816361564

    (defun dhnam/which-key-posframe-refposhandler-default (&optional frame)
      "The default posframe refposhandler used by ivy-posframe.
It's the same function with `ivy-posframe-refposhandler-default'"
      (cond
       ;; EXWM environment
       ((bound-and-true-p exwm--connection)
        (or (ignore-errors
              (let ((info (elt exwm-workspace--workareas
                               exwm-workspace-current-index)))
                (cons (elt info 0)
                      (elt info 1))))
            ;; Need user install xwininfo.
            (ignore-errors
              (posframe-refposhandler-xwininfo frame))
            ;; Fallback, this value will incorrect sometime, for example: user
            ;; have panel.
            (cons 0 0)))
       (t nil)))

    (defun-override which-key-posframe--show-buffer (act-popup-dim)
      "Show which-key buffer when popup type is posframe.
Argument ACT-POPUP-DIM includes the dimension, (height . width)
of the buffer text to be displayed in the popup"
      (when (posframe-workable-p)
        (save-window-excursion
          (posframe-show
           which-key--buffer
           :font which-key-posframe-font
           :position (point)
           :poshandler which-key-posframe-poshandler
           :background-color (face-attribute 'which-key-posframe :background nil t)
           :foreground-color (face-attribute 'which-key-posframe :foreground nil t)
           :height (car act-popup-dim)
           :width (cdr act-popup-dim)
           :internal-border-width which-key-posframe-border-width
           :internal-border-color (face-attribute 'which-key-posframe-border :background nil t)
           :override-parameters which-key-posframe-parameters
           :refposhandler #'dhnam/which-key-posframe-refposhandler-default))))))
