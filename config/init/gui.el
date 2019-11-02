;;; diable gui default setting
;; http://kb.mit.edu/confluence/display/istcontrib/Disabling+the+Emacs+menubar,+toolbar,+or+scrollbar

;;To disable the menu bar, place the following line in your .emacs file:
;; (menu-bar-mode -1) 
(menu-bar-mode t) 

;;To disable the scrollbar, use the following line:
(toggle-scroll-bar -1) 

;;To disable the toolbar, use the following line:
(tool-bar-mode -1) 

;;; enable maximization
;; http://emacs.stackexchange.com/questions/2999/how-to-maximize-my-emacs-frame-on-start-up
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
