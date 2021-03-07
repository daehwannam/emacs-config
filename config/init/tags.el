

;; making TAGS
;; https://www.emacswiki.org/emacs/BuildTags#toc3
;;
;; command: M-x compile RET find . -type f -iname "*.el" | etags - RET


;; setting tags table
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Select-Tags-Table.html

(when (machine-config-get-first 'etags-list)
  (setq tags-table-list (machine-config-get-first 'etags-list)))
