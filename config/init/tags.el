

;; making TAGS
;; https://www.emacswiki.org/emacs/BuildTags#toc3
;;
;; command: M-x compile RET find . -type f -iname "*.el" | etags - RET


(setq domain-name (trim-string (get-string-from-file "~/.emacs.d/config/domain.txt")))


;; setting tags table
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Select-Tags-Table.html
(cond
 ((string-equal domain-name "vbox")
  (setq tags-table-list
	'("/usr/share/emacs/24.3/lisp" "~/.emacs.d"))))
