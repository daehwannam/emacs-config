

;; making TAGS
;; https://www.emacswiki.org/emacs/BuildTags#toc3
;;
;; command: M-x compile RET find . -type f -iname "*.el" | etags - RET


;; setting tags table
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Select-Tags-Table.html
(cond
 ((string-equal machine-domain "vbox")
  (setq tags-table-list
	'("/usr/share/emacs/25.3/lisp"
	  "~/.emacs.d"))))
