
;;; hight light current line
;; http://emacsredux.com/blog/2013/04/02/highlight-current-line/
;(global-hl-line-mode t)


;;; configuration
;;https://stackoverflow.com/questions/17701576/changing-highlight-line-color-in-emacs
;(global-hl-line-mode t)


;;; hlt-highlight-symbol
;;; https://emacs.stackexchange.com/questions/23958/combine-highlight-symbol-mode-and-hl-line-mode
;; (require 'highlight)
;; (hlt-highlight-symbol)


;;; highlight with keeping syntax coloring
(global-hl-line-mode 1)
(set-face-foreground 'hl-line nil)
