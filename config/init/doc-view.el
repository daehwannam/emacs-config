
(progn
  (setq doc-view-scroll-small-height 3)

  (defun doc-view-scroll-up-or-next-page-small (&optional arg)
    "Scroll page up ARG lines if possible, else goto next page.
When `doc-view-continuous' is non-nil, scrolling upward
at the bottom edge of the page moves to the next page.
Otherwise, goto next page only on typing SPC (ARG is nil)."
    (interactive "P")
    (if (= (window-vscroll nil t) (doc-view-scroll-up-or-next-page (or arg doc-view-scroll-small-height)))
        (doc-view-next-page)))

  (defun doc-view-scroll-down-or-previous-page-small (&optional arg)
    "Scroll page down ARG lines if possible, else goto previous page.
When `doc-view-continuous' is non-nil, scrolling downward
at the top edge of the page moves to the previous page.
Otherwise, goto previous page only on typing DEL (ARG is nil)."
    (interactive "P")
    (when (= (window-vscroll nil t)
             (doc-view-scroll-down-or-previous-page (or arg doc-view-scroll-small-height)))
      (doc-view-previous-page)))

  (comment
   (define-key doc-view-mode-map (kbd "C-v") 'doc-view-scroll-up-or-next-page-small)
   (define-key doc-view-mode-map (kbd "M-v") 'doc-view-scroll-down-or-previous-page-small)))

(progn
  (setq doc-view-num-next-prev-lines 3)
  (setq doc-view-continuous t)

  (defun doc-view-next-line-or-next-page-small (&optional arg)
    "Scroll upward by ARG lines if possible, else goto next page.
When `doc-view-continuous' is non-nil, scrolling a line upward
at the bottom edge of the page moves to the next page."
    (interactive "p")
    (doc-view-next-line-or-next-page doc-view-num-next-prev-lines))

  (defun doc-view-previous-line-or-previous-page-small (&optional arg)
    "Scroll downward by ARG lines if possible, else goto previous page.
When `doc-view-continuous' is non-nil, scrolling a line downward
at the top edge of the page moves to the previous page."
    (interactive "p")
    (doc-view-previous-line-or-previous-page doc-view-num-next-prev-lines))

  (define-key doc-view-mode-map (kbd "C-v") 'doc-view-next-line-or-next-page-small)
  (define-key doc-view-mode-map (kbd "M-v") 'doc-view-previous-line-or-previous-page-small))
