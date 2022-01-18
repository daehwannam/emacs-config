
(when (fboundp 'which-key-mode)
  (which-key-mode))

(when (fboundp 'which-key-posframe-mode)
  (assert (fboundp 'ivy-posframe-mode))

  ;; `which-key-posframe--show-buffer' is updated to fix the problem of hidden posframe by X windows
  ;; https://github.com/ch11ng/exwm/issues/550#issuecomment-816361564
  (defun which-key-posframe--show-buffer (act-popup-dim)
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
         :refposhandler ivy-posframe-refposhandler)))))
