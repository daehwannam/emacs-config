;;; diable gui default setting
;; http://kb.mit.edu/confluence/display/istcontrib/Disabling+the+Emacs+menubar,+toolbar,+or+scrollbar

(progn
  ;;To disable the menu bar, place the following line in your .emacs file:
  (menu-bar-mode -1)
  (comment (menu-bar-mode t)))

(progn
  (comment
    ;;To disable the scrollbar for the current frame, use the following line:
    (toggle-scroll-bar -1))

  (progn
    ;; To disable the scrollbar for newly created frames
    ;; https://emacs.stackexchange.com/questions/23773/disable-scrollbar-on-new-frame
    (add-to-list 'default-frame-alist
                 '(vertical-scroll-bars . nil))

    ;; or
    (comment (scroll-bar-mode -1))))

(progn
  ;;To disable the toolbar, use the following line:
  (tool-bar-mode -1))

(comment
  ;; enable maximization
  ;; http://emacs.stackexchange.com/questions/2999/how-to-maximize-my-emacs-frame-on-start-up
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

(progn
  ;; left mouse click to open a file or a directory in the current buffer
  ;; https://emacs.stackexchange.com/a/20893
  (define-key dired-mode-map [mouse-2] 'dired-find-file))

(progn
  ;; disable blink-cursor-mode
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Cursor-Display.html
  (blink-cursor-mode 0))

(comment
  ;; fringe mode prevents to show the arrow indicating the current line of code in "gud"
  (fringe-mode 1))

(progn
  ;; window-divider config
  ;; the default value of window-divider-default-right-width is 6
  (setq window-divider-default-right-width 1)
  (window-divider-mode 1))

(provide 'init-gui)
